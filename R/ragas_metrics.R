#' Create an empty QA metrics tibble
#'
#' This helper creates an empty tibble with the columns used to store
#' RAGAS-style evaluation metrics for each QA interaction.
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

#' Compute simple RAGAS-style metrics for a QA log
#'
#' This function takes a QA log (as produced by [log_rag_interaction()])
#' and computes simple, deterministic proxy metrics for each row. These
#' metrics are lexical approximations designed for reproducible testing
#' and exploratory analysis. Later, they can be replaced or extended
#' with LLM-based scoring.
#'
#' The metrics are:
#' - context_precision: fraction of answer tokens that also appear in any
#'   retrieved context.
#' - context_recall: fraction of retrieved context tokens that appear in
#'   the answer.
#' - answer_relevance: lexical similarity between question and answer,
#'   measured as Jaccard similarity of token sets.
#' - faithfulness: here, approximated as the same as context_precision
#'   (placeholder proxy).
#' - ragas_overall: simple average of the above four metrics.
#'
#' @param qa_log A tibble created and populated by [log_rag_interaction()].
#'
#' @return A tibble with one row per `qa_id` and columns for the metrics.
#' @export
compute_ragas_metrics <- function(qa_log) {
  if (is.null(qa_log) || nrow(qa_log) == 0L) {
    return(qa_metrics_empty())
  }

  required_cols <- c(
    "qa_id",
    "question",
    "answer_model",
    "retrieved_texts"
  )

  missing_cols <- setdiff(required_cols, names(qa_log))
  if (length(missing_cols) > 0L) {
    stop(
      "qa_log is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # Helper: tokenize a string into lowercase word tokens
  tokenize <- function(x) {
    if (is.na(x) || !nzchar(x)) return(character(0L))
    # split on non-word characters, drop empties
    toks <- unlist(strsplit(tolower(x), "[^[:alnum:]]+"))
    toks[nzchar(toks)]
  }

  # Compute metrics row by row
  metric_rows <- lapply(seq_len(nrow(qa_log)), function(i) {
    qa_id   <- qa_log$qa_id[i]
    q_text  <- qa_log$question[i]
    a_text  <- qa_log$answer_model[i]

    # retrieved_texts is a list-column: each element is a character vector
    ctx_vec <- character(0L)
    if (!is.null(qa_log$retrieved_texts) && length(qa_log$retrieved_texts) >= i) {
      ctx_vec <- unlist(qa_log$retrieved_texts[[i]])
    }
    ctx_text <- paste(ctx_vec, collapse = " ")

    q_tokens <- unique(tokenize(q_text))
    a_tokens <- unique(tokenize(a_text))
    c_tokens <- unique(tokenize(ctx_text))

    # Context precision: among answer tokens, which are covered by context?
    if (length(a_tokens) == 0L || length(c_tokens) == 0L) {
      context_precision <- 0
    } else {
      overlap_ac <- length(intersect(a_tokens, c_tokens))
      context_precision <- overlap_ac / length(a_tokens)
    }

    # Context recall: among context tokens, which are present in answer?
    if (length(c_tokens) == 0L) {
      context_recall <- 0
    } else {
      overlap_ca <- length(intersect(c_tokens, a_tokens))
      context_recall <- overlap_ca / length(c_tokens)
    }

    # Answer relevance: Jaccard similarity between question and answer tokens
    if (length(q_tokens) == 0L && length(a_tokens) == 0L) {
      answer_relevance <- 0
    } else {
      inter_qa <- length(intersect(q_tokens, a_tokens))
      union_qa <- length(union(q_tokens, a_tokens))
      answer_relevance <- if (union_qa == 0L) 0 else inter_qa / union_qa
    }

    # Faithfulness: proxy metric, here just use context_precision
    faithfulness <- context_precision

    # Overall: mean of the four metrics
    ragas_overall <- mean(c(
      context_precision,
      context_recall,
      answer_relevance,
      faithfulness
    ))

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
