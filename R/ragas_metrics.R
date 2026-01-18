# R/ragas_metrics.R

#' Create an empty QA metrics tibble
#'
#' This helper creates an empty tibble with the columns used to store
#' RAGAS-style evaluation metrics for each QA interaction.
#'
#' All metric values are expected to be in \eqn{[0,1]}.
#'
#' @return A tibble with zero rows and the standard QA metrics columns.
#' @export
qa_metrics_empty <- function() {
  tibble::tibble(
    qa_id             = integer(),
    context_precision = numeric(),
    context_recall    = numeric(),
    answer_relevance  = numeric(),
    faithfulness      = numeric(),
    ragas_overall     = numeric()
  )
}

# ---- APPROX / PROXY METRICS --------------------------------------------------

#' Compute RAGAS-style metrics (approx / lexical proxy)
#'
#' Computes deterministic lexical approximations (token overlap / Jaccard).
#' These do NOT require ground-truth answers.
#'
#' @param qa_log A tibble created and populated by [log_rag_interaction()].
#'
#' @return A tibble with one row per `qa_id` and metric columns.
#' @export
compute_ragas_metrics_approx <- function(qa_log) {
  if (is.null(qa_log) || nrow(qa_log) == 0L) {
    return(qa_metrics_empty())
  }

  required_cols <- c("qa_id", "question", "answer_model", "retrieved_texts")
  missing_cols <- setdiff(required_cols, names(qa_log))
  if (length(missing_cols) > 0L) {
    stop(
      "qa_log is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  tokenize <- function(x) {
    if (is.na(x) || !nzchar(x)) return(character(0L))
    toks <- unlist(strsplit(tolower(x), "[^[:alnum:]]+"))
    toks[nzchar(toks)]
  }

  metric_rows <- lapply(seq_len(nrow(qa_log)), function(i) {
    qa_id  <- qa_log$qa_id[i]
    q_text <- qa_log$question[i]
    a_text <- qa_log$answer_model[i]

    ctx_vec <- character(0L)
    if (!is.null(qa_log$retrieved_texts) && length(qa_log$retrieved_texts) >= i) {
      ctx_vec <- unlist(qa_log$retrieved_texts[[i]])
    }
    ctx_text <- paste(ctx_vec, collapse = " ")

    q_tokens <- unique(tokenize(q_text))
    a_tokens <- unique(tokenize(a_text))
    c_tokens <- unique(tokenize(ctx_text))

    # Context precision: fraction of answer tokens that appear in retrieved context
    if (length(a_tokens) == 0L || length(c_tokens) == 0L) {
      context_precision <- 0
    } else {
      context_precision <- length(intersect(a_tokens, c_tokens)) / length(a_tokens)
    }

    # Context recall: fraction of context tokens that appear in answer
    if (length(c_tokens) == 0L) {
      context_recall <- 0
    } else {
      context_recall <- length(intersect(c_tokens, a_tokens)) / length(c_tokens)
    }

    # Answer relevance: Jaccard similarity between question and answer tokens
    if (length(q_tokens) == 0L && length(a_tokens) == 0L) {
      answer_relevance <- 0
    } else {
      inter_qa <- length(intersect(q_tokens, a_tokens))
      union_qa <- length(union(q_tokens, a_tokens))
      answer_relevance <- if (union_qa == 0L) 0 else inter_qa / union_qa
    }

    # Faithfulness: proxy (same as context_precision here)
    faithfulness <- context_precision

    ragas_overall <- mean(c(context_precision, context_recall, answer_relevance, faithfulness))

    tibble::tibble(
      qa_id             = as.integer(qa_id),
      context_precision = context_precision,
      context_recall    = context_recall,
      answer_relevance  = answer_relevance,
      faithfulness      = faithfulness,
      ragas_overall     = ragas_overall
    )
  })

  dplyr::bind_rows(metric_rows)
}

# ---- LLM-BASED "ACTUAL" METRICS (SCORING) -----------------------------------

# Internal: parse numeric score from model response (robust-ish)
parse_score_01 <- function(x) {
  if (is.null(x) || !is.character(x) || length(x) != 1L) return(NA_real_)
  # try JSON first
  out <- suppressWarnings({
    tryCatch(jsonlite::fromJSON(x), error = function(e) NULL)
  })
  if (is.list(out) && !is.null(out$score)) {
    s <- suppressWarnings(as.numeric(out$score))
    if (!is.na(s)) return(max(0, min(1, s)))
  }
  # fallback: first number in text
  m <- regmatches(x, regexpr("[0-9]*\\.?[0-9]+", x))
  s <- suppressWarnings(as.numeric(m))
  if (is.na(s)) return(NA_real_)
  max(0, min(1, s))
}

# Internal: score one metric with OpenAI chat (expects generate_openai_chat in embeddings_openai.R)
score_metric_llm <- function(metric_name, question, answer, contexts, ground_truth = NULL, model = "gpt-4o-mini", seed = NULL) {
  ctx_block <- paste0("- ", contexts, collapse = "\n")
  gt_line <- if (!is.null(ground_truth) && is.character(ground_truth) && length(ground_truth) == 1L && nzchar(ground_truth)) {
    paste0("\nGround truth answer:\n", ground_truth, "\n")
  } else {
    ""
  }

  prompt <- paste0(
    "You are scoring a RAG evaluation metric. Return ONLY valid JSON: {\"score\": <number between 0 and 1>}.\n\n",
    "Metric to score: ", metric_name, "\n\n",
    "Question:\n", question, "\n\n",
    "Model answer:\n", answer, "\n\n",
    "Retrieved contexts:\n", ctx_block, "\n",
    gt_line,
    "\nScoring guidance:\n",
    "- Use 0 for worst, 1 for best.\n",
    "- Be strict and consistent.\n"
  )

  raw <- generate_openai_chat(prompt = prompt, model = model, temperature = 0, seed = seed)
  parse_score_01(raw)
}

#' Compute RAGAS-style metrics (LLM scored)
#'
#' This is an LLM-based scoring implementation: it asks a judge model to score
#' each metric in \eqn{[0,1]} from the QA log’s question/answer/retrieved contexts.
#'
#' Notes:
#' - If `answer_reference` exists and is non-NA, it may be used as ground truth.
#' - Requires an OpenAI API key via `OPENAI_API_KEY`.
#'
#' @param qa_log A tibble created and populated by [log_rag_interaction()].
#' @param judge_model Character scalar; judge model name (default: "gpt-4o-mini").
#' @param seed Optional integer. If provided (and supported by the underlying
#'   model/provider), it is forwarded to the judge model calls to encourage
#'   reproducible scores (typically together with `temperature = 0`).
#'
#' @return A tibble with one row per `qa_id` and metric columns.
#' @export
compute_ragas_metrics_llm <- function(qa_log, judge_model = "gpt-4o-mini", seed = NULL) {
  if (is.null(qa_log) || nrow(qa_log) == 0L) {
    return(qa_metrics_empty())
  }

  required_cols <- c("qa_id", "question", "answer_model", "retrieved_texts")
  missing_cols <- setdiff(required_cols, names(qa_log))
  if (length(missing_cols) > 0L) {
    stop(
      "qa_log is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  has_gt <- "answer_reference" %in% names(qa_log)

  rows <- lapply(seq_len(nrow(qa_log)), function(i) {
    qa_id  <- qa_log$qa_id[i]
    q      <- qa_log$question[i]
    a      <- qa_log$answer_model[i]
    ctx    <- unlist(qa_log$retrieved_texts[[i]])
    if (length(ctx) == 0L) ctx <- ""

    gt <- NULL
    if (has_gt) {
      gt_val <- qa_log$answer_reference[i]
      if (!is.na(gt_val) && nzchar(gt_val)) gt <- gt_val
    }

    cp <- score_metric_llm("context_precision", q, a, ctx, ground_truth = gt, model = judge_model, seed = seed)
    cr <- score_metric_llm("context_recall",    q, a, ctx, ground_truth = gt, model = judge_model, seed = seed)
    ar <- score_metric_llm("answer_relevance",  q, a, ctx, ground_truth = gt, model = judge_model, seed = seed)
    fa <- score_metric_llm("faithfulness",      q, a, ctx, ground_truth = gt, model = judge_model, seed = seed)

    overall <- mean(c(cp, cr, ar, fa), na.rm = TRUE)
    if (is.nan(overall)) overall <- NA_real_

    tibble::tibble(
      qa_id             = as.integer(qa_id),
      context_precision = cp,
      context_recall    = cr,
      answer_relevance  = ar,
      faithfulness      = fa,
      ragas_overall     = overall
    )
  })

  dplyr::bind_rows(rows)
}

# ---- WRAPPER (MODE SWITCH) ---------------------------------------------------

#' Compute RAGAS metrics (mode switch)
#'
#' Convenience wrapper to compute either approximate proxy metrics or
#' LLM-scored metrics.
#'
#' @param qa_log QA log tibble.
#' @param mode One of `"approx"` or `"llm"`. If NULL, uses option
#'   `ragR.ragas_mode` (defaults to `"approx"`).
#' @param judge_model Judge model for `mode="llm"` (default: "gpt-4o-mini").
#' @param seed Optional integer. Forwarded to LLM-scored metrics when `mode="llm"`.
#'
#' @return A metrics tibble.
#' @export
compute_ragas_metrics <- function(qa_log, mode = NULL, judge_model = "gpt-4o-mini", seed = NULL) {
  if (is.null(mode)) {
    mode <- getOption("ragR.ragas_mode", "approx")
  }
  mode <- match.arg(mode, choices = c("approx", "llm"))

  if (identical(mode, "approx")) {
    compute_ragas_metrics_approx(qa_log)
  } else {
    compute_ragas_metrics_llm(qa_log, judge_model = judge_model, seed = seed)
  }
}
