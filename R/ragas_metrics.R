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

# ---- LLM-BASED METRICS (CANONICAL, RUBRIC-DEFINED) --------------------------

.assert_required_cols <- function(qa_log, required_cols) {
  missing_cols <- setdiff(required_cols, names(qa_log))
  if (length(missing_cols) > 0L) {
    stop(
      "qa_log is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
}

.normalize_contexts <- function(x) {
  # retrieved_texts is expected to be a list-column; each row is a character vector.
  if (is.null(x)) return(character(0L))
  if (is.character(x)) {
    # If a single string was stored, treat as one context.
    return(x)
  }
  if (is.list(x)) {
    # Unlist one level, keep as character
    y <- unlist(x, use.names = FALSE)
    return(as.character(y))
  }
  as.character(x)
}

.clamp01 <- function(v) {
  if (is.na(v)) return(NA_real_)
  max(0, min(1, as.numeric(v)))
}

# Strict-ish JSON reader: accepts a single JSON object; returns NULL if invalid.
.parse_json_object <- function(txt) {
  if (is.null(txt) || !is.character(txt) || length(txt) != 1L) return(NULL)
  out <- tryCatch(jsonlite::fromJSON(txt), error = function(e) NULL)
  if (is.null(out) || !is.list(out)) return(NULL)
  out
}

# One JSON-mode judge call, with a single retry if parsing fails.
.judge_json <- function(
  prompt,
  judge_model = "gpt-4o-mini",
  system_message = NULL,
  temperature = 0,
  max_output_tokens = 700L,
  retry_once = TRUE
) {
  if (is.null(system_message)) {
    system_message <- paste(
      "You are a strict evaluator for RAG metrics.",
      "You MUST output a SINGLE valid JSON object and nothing else.",
      "Do not wrap JSON in markdown fences."
    )
  }

  raw <- generate_openai_chat(
    prompt = prompt,
    model = judge_model,
    system_message = system_message,
    temperature = temperature,
    max_output_tokens = max_output_tokens,
    response_format = list(type = "json_object")
  )

  parsed <- .parse_json_object(raw)
  if (!is.null(parsed)) return(parsed)

  if (!isTRUE(retry_once)) return(NULL)

  # Retry with an even stricter reminder.
  raw2 <- generate_openai_chat(
    prompt = paste0(
      prompt,
      "\n\nIMPORTANT: Output ONLY ONE valid JSON object. No prose, no explanations."
    ),
    model = judge_model,
    system_message = system_message,
    temperature = temperature,
    max_output_tokens = max_output_tokens,
    response_format = list(type = "json_object")
  )
  .parse_json_object(raw2)
}

# ---- Metric: Answer Relevance (0-1) -----------------------------------------

.score_answer_relevance <- function(question, answer, judge_model, temperature) {
  prompt <- paste0(
    "Task: Score ANSWER RELEVANCE in [0,1].\n",
    "Definition:\n",
    "- 1.0: The answer directly addresses the question, is on-topic, and attempts to fully answer what is asked.\n",
    "- 0.5: Partially addresses the question but misses key parts, is vague, or contains significant irrelevant content.\n",
    "- 0.0: Does not answer the question, is unrelated, or says it cannot answer without attempting when it actually could.\n",
    "Rules:\n",
    "- Judge relevance only (not factual correctness).\n",
    "- If the answer is empty or only filler, score 0.\n\n",
    "Return JSON: {\"score\": <number>}.\n\n",
    "Question:\n", question, "\n\n",
    "Answer:\n", answer, "\n"
  )

  out <- .judge_json(
    prompt = prompt,
    judge_model = judge_model,
    temperature = temperature,
    max_output_tokens = 300L
  )
  if (is.null(out) || is.null(out$score)) return(NA_real_)
  .clamp01(out$score)
}

# ---- Metric: Context Precision (chunk relevance; mean of relevant chunks) ----

.score_context_precision <- function(question, contexts, judge_model, temperature) {
  if (length(contexts) == 0L) return(0)

  # One call returns an array of 0/1 relevances.
  ctx_block <- paste0(
    vapply(seq_along(contexts), function(i) {
      paste0(i, ". ", contexts[[i]])
    }, character(1)),
    collapse = "\n\n"
  )

  prompt <- paste0(
    "Task: Score CONTEXT PRECISION.\n",
    "Definition:\n",
    "- A context chunk is RELEVANT if it contains information that helps answer the question.\n",
    "- Irrelevant chunks are off-topic, purely administrative, or do not help answer the question.\n",
    "Procedure:\n",
    "- For each chunk, output 1 if relevant, else 0.\n",
    "- Then compute precision = mean(relevance).\n\n",
    "Return JSON: {\"relevance\": [0/1,...], \"score\": <precision in [0,1]>}.\n\n",
    "Question:\n", question, "\n\n",
    "Contexts:\n", ctx_block, "\n"
  )

  out <- .judge_json(
    prompt = prompt,
    judge_model = judge_model,
    temperature = temperature,
    max_output_tokens = 800L
  )

  if (is.null(out) || is.null(out$score)) return(NA_real_)
  .clamp01(out$score)
}

# ---- Metric: Faithfulness (claim support; supported/total) ------------------

.extract_claims <- function(answer, judge_model, temperature) {
  prompt <- paste0(
    "Task: Extract ATOMIC FACTUAL CLAIMS from the answer.\n",
    "Rules:\n",
    "- A claim should be a single checkable statement.\n",
    "- Ignore purely stylistic or conversational text.\n",
    "- If there are no factual claims, return an empty list.\n\n",
    "Return JSON: {\"claims\": [\"...\"]}.\n\n",
    "Answer:\n", answer, "\n"
  )

  out <- .judge_json(
    prompt = prompt,
    judge_model = judge_model,
    temperature = temperature,
    max_output_tokens = 700L
  )

  if (is.null(out) || is.null(out$claims)) return(character(0L))
  claims <- out$claims
  if (is.null(claims)) return(character(0L))
  if (!is.character(claims)) claims <- as.character(unlist(claims, use.names = FALSE))
  claims <- claims[nzchar(trimws(claims))]
  unique(claims)
}

.check_claim_support <- function(claims, contexts, judge_model, temperature) {
  if (length(claims) == 0L) {
    return(list(supported = logical(0), score = 1))
  }
  ctx_block <- paste0(
    vapply(seq_along(contexts), function(i) {
      paste0(i, ". ", contexts[[i]])
    }, character(1)),
    collapse = "\n\n"
  )
  claims_block <- paste0(
    vapply(seq_along(claims), function(i) {
      paste0(i, ". ", claims[[i]])
    }, character(1)),
    collapse = "\n"
  )

  prompt <- paste0(
    "Task: Check FAITHFULNESS (support in retrieved contexts).\n",
    "Definition:\n",
    "- A claim is SUPPORTED if the retrieved contexts explicitly contain the information needed for that claim.\n",
    "- If the contexts do not support it, mark unsupported.\n",
    "Rules:\n",
    "- Be strict: if support is missing or only implied, mark unsupported.\n",
    "- Use ONLY the contexts.\n\n",
    "Return JSON: {\"supported\": [true/false,...], \"score\": <supported_fraction in [0,1]>}.\n\n",
    "Claims:\n", claims_block, "\n\n",
    "Contexts:\n", ctx_block, "\n"
  )

  out <- .judge_json(
    prompt = prompt,
    judge_model = judge_model,
    temperature = temperature,
    max_output_tokens = 900L
  )

  if (is.null(out) || is.null(out$score)) return(list(supported = rep(NA, length(claims)), score = NA_real_))
  list(supported = out$supported, score = .clamp01(out$score))
}

.score_faithfulness <- function(answer, contexts, judge_model, temperature) {
  if (!nzchar(trimws(answer))) return(0)
  if (length(contexts) == 0L) return(0)

  claims <- .extract_claims(answer, judge_model, temperature)
  checked <- .check_claim_support(claims, contexts, judge_model, temperature)
  checked$score
}

# ---- Metric: Context Recall (ground-truth keypoints covered by contexts) ----

.extract_keypoints <- function(question, ground_truth, judge_model, temperature) {
  prompt <- paste0(
    "Task: Extract KEYPOINTS from the ground-truth answer that are needed to answer the question.\n",
    "Rules:\n",
    "- Each keypoint should be a short, checkable statement.\n",
    "- Aim for 3-8 keypoints when possible.\n",
    "- Do not invent info; only use ground truth.\n\n",
    "Return JSON: {\"keypoints\": [\"...\"]}.\n\n",
    "Question:\n", question, "\n\n",
    "Ground truth answer:\n", ground_truth, "\n"
  )

  out <- .judge_json(
    prompt = prompt,
    judge_model = judge_model,
    temperature = temperature,
    max_output_tokens = 700L
  )

  if (is.null(out) || is.null(out$keypoints)) return(character(0L))
  kps <- out$keypoints
  if (!is.character(kps)) kps <- as.character(unlist(kps, use.names = FALSE))
  kps <- kps[nzchar(trimws(kps))]
  unique(kps)
}

.check_keypoint_coverage <- function(keypoints, contexts, judge_model, temperature) {
  if (length(keypoints) == 0L) {
    return(list(covered = logical(0), score = 1))
  }
  ctx_block <- paste0(
    vapply(seq_along(contexts), function(i) {
      paste0(i, ". ", contexts[[i]])
    }, character(1)),
    collapse = "\n\n"
  )
  kp_block <- paste0(
    vapply(seq_along(keypoints), function(i) {
      paste0(i, ". ", keypoints[[i]])
    }, character(1)),
    collapse = "\n"
  )

  prompt <- paste0(
    "Task: Check CONTEXT RECALL (coverage of ground-truth keypoints by retrieved contexts).\n",
    "Definition:\n",
    "- A keypoint is COVERED if the retrieved contexts contain enough information to support it.\n",
    "Rules:\n",
    "- Be strict: if not clearly present, mark not covered.\n",
    "- Use ONLY the contexts.\n\n",
    "Return JSON: {\"covered\": [true/false,...], \"score\": <covered_fraction in [0,1]>}.\n\n",
    "Keypoints:\n", kp_block, "\n\n",
    "Contexts:\n", ctx_block, "\n"
  )

  out <- .judge_json(
    prompt = prompt,
    judge_model = judge_model,
    temperature = temperature,
    max_output_tokens = 900L
  )

  if (is.null(out) || is.null(out$score)) return(list(covered = rep(NA, length(keypoints)), score = NA_real_))
  list(covered = out$covered, score = .clamp01(out$score))
}

.score_context_recall <- function(question, ground_truth, contexts, judge_model, temperature) {
  if (!nzchar(trimws(ground_truth))) return(NA_real_)
  if (length(contexts) == 0L) return(0)

  kps <- .extract_keypoints(question, ground_truth, judge_model, temperature)
  checked <- .check_keypoint_coverage(kps, contexts, judge_model, temperature)
  checked$score
}

#' Compute RAGAS-style metrics (LLM scored, rubric-defined)
#'
#' Implements RAGAS-style metrics using an LLM judge with explicit rubrics:
#' - Context Precision: fraction of retrieved chunks relevant to the question.
#' - Context Recall: fraction of ground-truth keypoints covered by retrieved contexts.
#' - Answer Relevance: relevance of answer to question (not correctness).
#' - Faithfulness: fraction of answer claims supported by retrieved contexts.
#'
#' IMPORTANT:
#' - `context_recall` REQUIRES `answer_reference` (ground truth). If missing/NA,
#'   context_recall is returned as NA_real_ for that row.
#'
#' Requires an OpenAI API key via `OPENAI_API_KEY`.
#'
#' @param qa_log A tibble created and populated by [log_rag_interaction()].
#' @param judge_model Character scalar; judge model name (default: "gpt-4o-mini").
#' @param judge_temperature Numeric; default 0 for deterministic scoring.
#'
#' @return A tibble with one row per `qa_id` and metric columns.
#' @export
compute_ragas_metrics_llm <- function(
  qa_log,
  judge_model = "gpt-4o-mini",
  judge_temperature = 0
) {
  if (is.null(qa_log) || nrow(qa_log) == 0L) {
    return(qa_metrics_empty())
  }

  .assert_required_cols(qa_log, c("qa_id", "question", "answer_model", "retrieved_texts"))

  has_gt <- "answer_reference" %in% names(qa_log)

  rows <- lapply(seq_len(nrow(qa_log)), function(i) {
    qa_id <- qa_log$qa_id[i]
    q     <- qa_log$question[i]
    a     <- qa_log$answer_model[i]

    ctx <- .normalize_contexts(qa_log$retrieved_texts[[i]])
    ctx <- ctx[nzchar(trimws(ctx))]

    gt <- NULL
    if (isTRUE(has_gt)) {
      gt_val <- qa_log$answer_reference[i]
      if (!is.na(gt_val) && nzchar(trimws(gt_val))) gt <- as.character(gt_val)
    }

    cp <- .score_context_precision(q, ctx, judge_model, judge_temperature)
    ar <- .score_answer_relevance(q, a, judge_model, judge_temperature)
    fa <- .score_faithfulness(a, ctx, judge_model, judge_temperature)

    cr <- NA_real_
    if (!is.null(gt)) {
      cr <- .score_context_recall(q, gt, ctx, judge_model, judge_temperature)
    }

    # Overall: only defined when all four metrics are available (canonical run).
    overall <- NA_real_
    if (!any(is.na(c(cp, cr, ar, fa)))) {
      overall <- mean(c(cp, cr, ar, fa))
    }

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
#' LLM-scored rubric-defined metrics.
#'
#' @param qa_log QA log tibble.
#' @param mode One of `"approx"` or `"llm"`. If NULL, uses option
#'   `ragR.ragas_mode` (defaults to `"approx"`).
#' @param judge_model Judge model for `mode="llm"` (default: "gpt-4o-mini").
#' @param judge_temperature Judge temperature for `mode="llm"` (default: 0).
#'
#' @return A metrics tibble.
#' @export
compute_ragas_metrics <- function(
  qa_log,
  mode = NULL,
  judge_model = "gpt-4o-mini",
  judge_temperature = 0
) {
  if (is.null(mode)) {
    mode <- getOption("ragR.ragas_mode", "approx")
  }
  mode <- match.arg(mode, choices = c("approx", "llm"))

  if (identical(mode, "approx")) {
    compute_ragas_metrics_approx(qa_log)
  } else {
    compute_ragas_metrics_llm(
      qa_log,
      judge_model = judge_model,
      judge_temperature = judge_temperature
    )
  }
}
