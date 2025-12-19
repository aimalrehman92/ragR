# R/ragas_metrics.R

#' Create an empty QA metrics tibble
#'
#' @return A tibble with zero rows and the standard QA metrics columns.
#' @export
qa_metrics_empty <- function() {
  tibble::tibble(
    qa_id = integer(),
    context_precision = numeric(),
    context_recall = numeric(),
    answer_relevance = numeric(),
    faithfulness = numeric(),
    ragas_overall = numeric()
  )
}

# ---- Internal helpers -----------------------------------------------------

.ragas_require_jsonlite <- function() {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required for exact RAGAS metrics. Please add it to DESCRIPTION Imports.", call. = FALSE)
  }
}

.ragas_safe_from_json <- function(x) {
  .ragas_require_jsonlite()
  if (is.null(x) || !is.character(x) || length(x) != 1L) return(NULL)
  if (is.na(x) || !nzchar(x)) return(NULL)
  tryCatch(jsonlite::fromJSON(x), error = function(e) NULL)
}

.ragas_extract_first_json_object <- function(txt) {
  # Sometimes models wrap JSON in text; try to extract first {...} block.
  if (is.null(txt) || is.na(txt) || !nzchar(txt)) return(txt)
  m <- regmatches(txt, regexpr("\\{[\\s\\S]*\\}", txt))
  if (length(m) == 0L || !nzchar(m)) return(txt)
  m
}

.ragas_clamp01 <- function(x) {
  if (is.na(x)) return(NA_real_)
  x <- as.numeric(x)
  if (!is.finite(x)) return(NA_real_)
  max(0, min(1, x))
}

.ragas_cosine <- function(a, b) {
  a <- as.numeric(a); b <- as.numeric(b)
  if (length(a) == 0L || length(b) == 0L || length(a) != length(b)) return(NA_real_)
  denom <- sqrt(sum(a * a)) * sqrt(sum(b * b))
  if (!is.finite(denom) || denom == 0) return(NA_real_)
  sum(a * b) / denom
}

.ragas_average_precision <- function(v) {
  # v: numeric 0/1/NA relevance indicators in retrieval order
  if (length(v) == 0L) return(0)
  v <- as.numeric(v)
  v[is.na(v)] <- 0
  if (sum(v) == 0) return(0)
  prec_at_k <- cumsum(v) / seq_along(v)
  sum(prec_at_k * v) / sum(v)
}

.ragas_call_llm_json <- function(prompt, model, system_message, temperature = 0) {
  # Uses your existing wrapper generate_openai_chat()
  txt <- generate_openai_chat(
    prompt = prompt,
    model = model,
    system_message = system_message,
    temperature = temperature
  )
  txt <- as.character(txt)
  txt <- paste(txt, collapse = "\n")
  txt <- .ragas_extract_first_json_object(txt)
  txt
}

.ragas_extract_claims <- function(text, model, temperature = 0) {
  if (is.null(text) || is.na(text) || !nzchar(text)) return(character(0L))
  sys <- paste(
    "You extract atomic, verifiable claims from text.",
    "Return ONLY valid JSON with the schema: {\"claims\": [\"...\"]}.",
    "Each claim must be short and self-contained.",
    "If there are no claims, return {\"claims\": []}."
  )
  prompt <- paste0(
    "TEXT:\n---\n", text, "\n---\n\n",
    "Extract atomic factual claims."
  )
  raw <- .ragas_call_llm_json(prompt, model = model, system_message = sys, temperature = temperature)
  obj <- .ragas_safe_from_json(raw)
  if (is.null(obj) || is.null(obj$claims)) return(character(0L))
  claims <- unlist(obj$claims, use.names = FALSE)
  claims <- as.character(claims)
  claims[nzchar(claims)]
}

.ragas_judge_supported <- function(claim, contexts, model, temperature = 0) {
  # Return 1 if supported, 0 if not supported, NA if unparsable
  if (is.na(claim) || !nzchar(claim)) return(NA_real_)
  ctx <- paste(contexts, collapse = "\n\n")
  sys <- paste(
    "You are a strict fact-checker.",
    "Decide whether the CLAIM is supported by the CONTEXT.",
    "Use ONLY the provided context; do not use outside knowledge.",
    "Return ONLY JSON: {\"supported\": true/false}."
  )
  prompt <- paste0(
    "CONTEXT:\n---\n", ctx, "\n---\n\n",
    "CLAIM:\n---\n", claim, "\n---\n\n",
    "Is the claim supported by the context?"
  )
  raw <- .ragas_call_llm_json(prompt, model = model, system_message = sys, temperature = temperature)
  obj <- .ragas_safe_from_json(raw)
  if (is.null(obj) || is.null(obj$supported)) return(NA_real_)
  if (isTRUE(obj$supported)) 1 else 0
}

.ragas_judge_chunk_relevant <- function(question, answer, chunk, model, temperature = 0) {
  # Return 1 if relevant, 0 if not, NA if unparsable
  if (is.na(chunk) || !nzchar(chunk)) return(NA_real_)
  sys <- paste(
    "You are evaluating retrieval quality for a RAG system.",
    "Decide whether the CONTEXT CHUNK is relevant and useful to answer the QUESTION.",
    "Return ONLY JSON: {\"relevant\": true/false}."
  )
  prompt <- paste0(
    "QUESTION:\n---\n", question, "\n---\n\n",
    "ANSWER:\n---\n", answer, "\n---\n\n",
    "CONTEXT CHUNK:\n---\n", chunk, "\n---\n\n",
    "Is this context chunk relevant/useful for answering the question?"
  )
  raw <- .ragas_call_llm_json(prompt, model = model, system_message = sys, temperature = temperature)
  obj <- .ragas_safe_from_json(raw)
  if (is.null(obj) || is.null(obj$relevant)) return(NA_real_)
  if (isTRUE(obj$relevant)) 1 else 0
}

.ragas_generate_questions <- function(answer, n, model, temperature = 0) {
  if (is.null(answer) || is.na(answer) || !nzchar(answer)) return(character(0L))
  sys <- paste(
    "You generate questions that could be answered by the given answer.",
    "Return ONLY valid JSON: {\"questions\": [\"...\"]}.",
    "Return at most N questions; keep them concise."
  )
  prompt <- paste0(
    "N = ", as.integer(n), "\n\n",
    "ANSWER:\n---\n", answer, "\n---\n\n",
    "Generate questions that this answer would correctly answer."
  )
  raw <- .ragas_call_llm_json(prompt, model = model, system_message = sys, temperature = temperature)
  obj <- .ragas_safe_from_json(raw)
  if (is.null(obj) || is.null(obj$questions)) return(character(0L))
  qs <- unlist(obj$questions, use.names = FALSE)
  qs <- as.character(qs)
  qs <- qs[nzchar(qs)]
  if (length(qs) > n) qs <- qs[seq_len(n)]
  qs
}

#' Compute Actual / Exact-style RAGAS metrics for a QA log
#'
#' This replaces the previous lexical proxy implementation with an
#' LLM+embedding implementation. Output format is unchanged to keep
#' downstream summary/report stable.
#'
#' @param qa_log A tibble created and populated by [log_rag_interaction()].
#' @param judge_model OpenAI chat model used for judging/claim extraction.
#' @param embedding_model OpenAI embedding model used for answer relevance.
#' @param n_gen_questions Number of generated questions for answer relevance (default 3).
#' @param temperature Temperature for judge model (default 0).
#'
#' @return A tibble with one row per `qa_id` and columns for the metrics.
#' @export
compute_ragas_metrics <- function(
  qa_log,
  judge_model = "gpt-4o-mini",
  embedding_model = "text-embedding-3-small",
  n_gen_questions = 3L,
  temperature = 0
) {
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

  has_reference <- "answer_reference" %in% names(qa_log)

  metric_rows <- lapply(seq_len(nrow(qa_log)), function(i) {
    qa_id <- qa_log$qa_id[i]
    q_text <- qa_log$question[i]
    a_text <- qa_log$answer_model[i]

    ctx_vec <- character(0L)
    if (!is.null(qa_log$retrieved_texts) && length(qa_log$retrieved_texts) >= i) {
      ctx_vec <- unlist(qa_log$retrieved_texts[[i]])
    }
    ctx_vec <- as.character(ctx_vec)
    ctx_vec <- ctx_vec[!is.na(ctx_vec) & nzchar(ctx_vec)]

    # ---- 1) Context Precision: AP over chunk relevance in rank order ----
    rels <- if (length(ctx_vec) == 0L) numeric(0L) else {
      vapply(
        ctx_vec,
        function(ch) .ragas_judge_chunk_relevant(q_text, a_text, ch, model = judge_model, temperature = temperature),
        numeric(1)
      )
    }
    context_precision <- .ragas_clamp01(.ragas_average_precision(rels))

    # ---- 2) Faithfulness: answer claims supported by context ----
    ans_claims <- .ragas_extract_claims(a_text, model = judge_model, temperature = temperature)
    if (length(ans_claims) == 0L) {
      faithfulness <- NA_real_
    } else if (length(ctx_vec) == 0L) {
      faithfulness <- 0
    } else {
      sup <- vapply(
        ans_claims,
        function(cl) .ragas_judge_supported(cl, ctx_vec, model = judge_model, temperature = temperature),
        numeric(1)
      )
      if (all(is.na(sup))) {
        faithfulness <- NA_real_
      } else {
        faithfulness <- sum(sup == 1, na.rm = TRUE) / sum(!is.na(sup))
      }
    }
    faithfulness <- if (!is.na(faithfulness)) .ragas_clamp01(faithfulness) else NA_real_

    # ---- 3) Context Recall: reference claims supported by context ----
    context_recall <- NA_real_
    if (has_reference) {
      ref <- qa_log$answer_reference[i]
      if (!is.na(ref) && nzchar(ref)) {
        ref_claims <- .ragas_extract_claims(ref, model = judge_model, temperature = temperature)
        if (length(ref_claims) == 0L) {
          context_recall <- NA_real_
        } else if (length(ctx_vec) == 0L) {
          context_recall <- 0
        } else {
          sup_ref <- vapply(
            ref_claims,
            function(cl) .ragas_judge_supported(cl, ctx_vec, model = judge_model, temperature = temperature),
            numeric(1)
          )
          if (all(is.na(sup_ref))) {
            context_recall <- NA_real_
          } else {
            context_recall <- sum(sup_ref == 1, na.rm = TRUE) / sum(!is.na(sup_ref))
          }
        }
      }
    }
    context_recall <- if (!is.na(context_recall)) .ragas_clamp01(context_recall) else NA_real_

    # ---- 4) Answer Relevance: generated questions + embedding cosine ----
    answer_relevance <- NA_real_
    gen_qs <- .ragas_generate_questions(a_text, n = as.integer(n_gen_questions), model = judge_model, temperature = temperature)
    if (!is.na(q_text) && nzchar(q_text) && length(gen_qs) > 0L) {
      emb_mat <- get_openai_embeddings(texts = c(q_text, gen_qs), model = embedding_model)
      if (is.matrix(emb_mat) && nrow(emb_mat) >= 2L) {
        qv <- as.numeric(emb_mat[1, ])
        sims <- vapply(2:nrow(emb_mat), function(r) {
          .ragas_cosine(qv, as.numeric(emb_mat[r, ]))
        }, numeric(1))
        sims <- sims[is.finite(sims)]
        if (length(sims) > 0L) {
          # cosine in [-1,1] -> map to [0,1]
          answer_relevance <- mean((sims + 1) / 2)
        }
      }
    }
    answer_relevance <- if (!is.na(answer_relevance)) .ragas_clamp01(answer_relevance) else NA_real_

    # ---- Overall: mean of available metrics (NA-safe) ----
    ragas_overall <- .ragas_clamp01(mean(c(
      context_precision,
      context_recall,
      answer_relevance,
      faithfulness
    ), na.rm = TRUE))

    tibble::tibble(
      qa_id = as.integer(qa_id),
      context_precision = context_precision,
      context_recall = context_recall,
      answer_relevance = answer_relevance,
      faithfulness = faithfulness,
      ragas_overall = ragas_overall
    )
  })

  dplyr::bind_rows(metric_rows)
}