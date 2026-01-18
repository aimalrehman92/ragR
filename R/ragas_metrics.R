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

# Internal: clamp to [0,1]
.clamp01 <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (is.na(x)) return(NA_real_)
  max(0, min(1, x))
}

# Internal: parse numeric score from model response (robust-ish)
.parse_score_01 <- function(x) {
  if (is.null(x) || !is.character(x) || length(x) != 1L) return(NA_real_)

  out <- suppressWarnings(tryCatch(jsonlite::fromJSON(x), error = function(e) NULL))
  if (is.list(out) && !is.null(out$score)) {
    s <- suppressWarnings(as.numeric(out$score))
    if (!is.na(s)) return(.clamp01(s))
  }

  m <- regmatches(x, regexpr("[0-9]*\\.?[0-9]+", x))
  s <- suppressWarnings(as.numeric(m))
  if (is.na(s)) return(NA_real_)
  .clamp01(s)
}

# Internal: parse verdict (0/1) from model response
.parse_verdict_01 <- function(x) {
  if (is.null(x) || !is.character(x) || length(x) != 1L) return(NA_integer_)

  out <- suppressWarnings(tryCatch(jsonlite::fromJSON(x), error = function(e) NULL))
  if (is.list(out) && !is.null(out$verdict)) {
    v <- suppressWarnings(as.integer(out$verdict))
    if (!is.na(v)) return(ifelse(v == 1L, 1L, 0L))
  }

  m <- regmatches(x, regexpr("[0-9]+", x))
  v <- suppressWarnings(as.integer(m))
  if (is.na(v)) return(NA_integer_)
  ifelse(v == 1L, 1L, 0L)
}

# Internal: parse attributed (0/1) from model response
.parse_attributed_01 <- function(x) {
  if (is.null(x) || !is.character(x) || length(x) != 1L) return(NA_integer_)

  out <- suppressWarnings(tryCatch(jsonlite::fromJSON(x), error = function(e) NULL))
  if (is.list(out) && !is.null(out$attributed)) {
    v <- suppressWarnings(as.integer(out$attributed))
    if (!is.na(v)) return(ifelse(v == 1L, 1L, 0L))
  }

  m <- regmatches(x, regexpr("[0-9]+", x))
  v <- suppressWarnings(as.integer(m))
  if (is.na(v)) return(NA_integer_)
  ifelse(v == 1L, 1L, 0L)
}

# Internal: compute Average Precision from a ranked binary relevance vector
# Mirrors RAGAS Python implementation used by context_precision.
.average_precision <- function(verdicts) {
  if (length(verdicts) == 0L) return(0)
  v <- as.integer(verdicts)
  v[is.na(v)] <- 0L
  v <- ifelse(v == 1L, 1L, 0L)

  denom <- sum(v) + 1e-10
  numer <- 0
  for (i in seq_along(v)) {
    if (v[[i]] == 1L) {
      numer <- numer + (sum(v[seq_len(i)]) / i) * v[[i]]
    }
  }
  .clamp01(numer / denom)
}

# Internal: split into sentences (simple + robust for evaluation)
.split_sentences <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(trimws(x))) return(character(0L))
  x <- gsub("\r\n|\r", "\n", x)
  x <- gsub("[ \t]+", " ", x)
  x <- gsub("\n+", "\n", x)

  parts <- unlist(strsplit(x, "(?<=[.!?])\\s+|\\n+", perl = TRUE))
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  parts
}

# ---- CONTEXT PRECISION (DO NOT CHANGE) --------------------------------------

# Internal: context precision verdict prompt (matches ragas/main prompt intent)
.score_context_precision_llm <- function(question, answer_for_eval, contexts, model = "gpt-4o-mini") {
  if (length(contexts) == 0L) return(0)

  verdicts <- integer(length(contexts))

  for (i in seq_along(contexts)) {
    prompt <- paste0(
      "Given question, answer and context verify if the context was useful in arriving at the given answer. ",
      "Give verdict as \"1\" if useful and \"0\" if not with json output.\n\n",
      "Question:\n", question, "\n\n",
      "Context:\n", contexts[[i]], "\n\n",
      "Answer:\n", answer_for_eval, "\n\n",
      "Return ONLY valid JSON: {\"reason\": <string>, \"verdict\": 0 or 1}."
    )

    raw <- generate_openai_chat(prompt = prompt, model = model, temperature = 0)
    v <- .parse_verdict_01(raw)
    if (is.na(v)) v <- 0L
    verdicts[[i]] <- v
  }

  .average_precision(verdicts)
}

# ---- CONTEXT RECALL (ALIGN WITH PYTHON) -------------------------------------

# Internal: Context recall classification prompt (sentence-level attribution)
# Mirrors the Python definition: classify each sentence in the reference answer
# as attributable (1) or not (0) to the retrieved context; score = mean(attributed).
.score_context_recall_llm <- function(question, reference_answer, contexts, model = "gpt-4o-mini") {
  if (is.null(reference_answer) || is.na(reference_answer) || !nzchar(trimws(reference_answer))) {
    return(NA_real_)
  }

  ctx_joined <- paste(contexts, collapse = "\n")
  answer_sents <- .split_sentences(reference_answer)

  if (length(answer_sents) == 0L) {
    return(NA_real_)
  }

  attributed_vec <- integer(length(answer_sents))

  for (i in seq_along(answer_sents)) {
    stmt <- answer_sents[[i]]

    prompt <- paste0(
      "Given a context, and an answer, analyze each sentence in the answer and classify if the sentence can be attributed to the given context or not. ",
      "Use only 'Yes' (1) or 'No' (0) as a binary classification. Output json with reason.\n\n",
      "Question:\n", question, "\n\n",
      "Context:\n", ctx_joined, "\n\n",
      "Answer sentence:\n", stmt, "\n\n",
      "Return ONLY valid JSON: {\"statement\": <string>, \"reason\": <string>, \"attributed\": 0 or 1}."
    )

    raw <- generate_openai_chat(prompt = prompt, model = model, temperature = 0)
    v <- .parse_attributed_01(raw)
    if (is.na(v)) v <- 0L
    attributed_vec[[i]] <- v
  }

  score <- sum(attributed_vec) / length(attributed_vec)
  .clamp01(score)
}

# Internal: score one metric with OpenAI chat (expects generate_openai_chat in embeddings_openai.R)
score_metric_llm <- function(metric_name, question, answer, contexts, ground_truth = NULL, model = "gpt-4o-mini") {
  # Special-case: context_precision in RAGAS is Average Precision over per-context 0/1 verdicts.
  if (identical(metric_name, "context_precision")) {
    answer_for_eval <- if (!is.null(ground_truth) && is.character(ground_truth) && length(ground_truth) == 1L && nzchar(trimws(ground_truth))) {
      ground_truth
    } else {
      answer
    }
    return(.score_context_precision_llm(question, answer_for_eval, contexts, model = model))
  }

  # Special-case: context_recall uses the REFERENCE (ground truth) answer, sentence-level attribution.
  if (identical(metric_name, "context_recall")) {
    if (is.null(ground_truth) || !is.character(ground_truth) || length(ground_truth) != 1L || !nzchar(trimws(ground_truth))) {
      return(NA_real_)
    }
    return(.score_context_recall_llm(question, ground_truth, contexts, model = model))
  }

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

  raw <- generate_openai_chat(prompt = prompt, model = model, temperature = 0)
  .parse_score_01(raw)
}

#' Compute RAGAS-style metrics (LLM scored)
#'
#' This is an LLM-based scoring implementation: it asks a judge model to score
#' each metric in \eqn{[0,1]} from the QA log’s question/answer/retrieved contexts.
#'
#' Notes:
#' - If `answer_reference` exists and is non-NA, it may be used as ground truth.
#' - `context_precision` is implemented to mirror RAGAS (Python) behavior:
#'   it computes **Average Precision** over per-context binary usefulness verdicts.
#' - `context_recall` is implemented to mirror RAGAS (Python) behavior:
#'   it computes the fraction of sentences in the **reference answer** that can be attributed to the retrieved context.
#' - Requires an OpenAI API key via `OPENAI_API_KEY`.
#'
#' @param qa_log A tibble created and populated by [log_rag_interaction()].
#' @param judge_model Character scalar; judge model name (default: "gpt-4o-mini").
#'
#' @return A tibble with one row per `qa_id` and metric columns.
#' @export
compute_ragas_metrics_llm <- function(qa_log, judge_model = "gpt-4o-mini") {
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

    ctx <- unlist(qa_log$retrieved_texts[[i]])
    ctx <- as.character(ctx)
    ctx <- ctx[nzchar(trimws(ctx))]
    if (length(ctx) == 0L) ctx <- ""

    gt <- NULL
    if (isTRUE(has_gt)) {
      gt_val <- qa_log$answer_reference[i]
      if (!is.na(gt_val) && nzchar(trimws(gt_val))) gt <- as.character(gt_val)
    }

    cp <- score_metric_llm("context_precision", q, a, ctx, ground_truth = gt, model = judge_model)
    cr <- score_metric_llm("context_recall",    q, a, ctx, ground_truth = gt, model = judge_model)
    ar <- score_metric_llm("answer_relevance",  q, a, ctx, ground_truth = gt, model = judge_model)
    fa <- score_metric_llm("faithfulness",      q, a, ctx, ground_truth = gt, model = judge_model)

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
#'
#' @return A metrics tibble.
#' @export
compute_ragas_metrics <- function(qa_log, mode = NULL, judge_model = "gpt-4o-mini") {
  if (is.null(mode)) {
    mode <- getOption("ragR.ragas_mode", "approx")
  }
  mode <- match.arg(mode, choices = c("approx", "llm"))

  if (identical(mode, "approx")) {
    compute_ragas_metrics_approx(qa_log)
  } else {
    compute_ragas_metrics_llm(qa_log, judge_model = judge_model)
  }
}
