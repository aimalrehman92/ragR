# R/qa_logging.R

#' Create an empty QA log tibble
#'
#' Standard schema for storing Q/A interactions produced by the RAG pipeline.
#'
#' @return A tibble with zero rows and the standard QA log columns.
#' @export
qa_log_empty <- function() {
  tibble::tibble(
    qa_id            = integer(),
    question         = character(),
    prompt_final     = character(),   # <-- NEW: final prompt sent to the model
    answer_model     = character(),
    answer_reference = character(),
    collection       = character(),
    retrieved_ids    = list(),
    retrieved_texts  = list(),
    chat_model       = character(),
    embedding_model  = character(),
    timestamp        = as.POSIXct(character())
  )
}

#' Append one RAG interaction to the QA log
#'
#' Adds one row to an in-memory QA log tibble.
#'
#' @param qa_log Existing QA log tibble, or NULL.
#' @param question Character scalar.
#' @param rag_result List returned by query_rag() (must include `answer`; may include `retrieved` and `prompt`).
#' @param collection Collection name used for retrieval.
#' @param chat_model Chat model name.
#' @param embedding_model Embedding model name.
#' @param qa_id Optional integer QA id. If NULL, auto-increments.
#' @param timestamp POSIXct timestamp.
#'
#' @return Updated QA log tibble.
#' @export
log_rag_interaction <- function(
  qa_log,
  question,
  rag_result,
  collection,
  chat_model      = "gpt-4o-mini",
  embedding_model = "text-embedding-3-small",
  qa_id           = NULL,
  timestamp       = Sys.time()
) {
  # Initialise log if NULL
  if (is.null(qa_log)) {
    qa_log <- qa_log_empty()
  }

  # Basic checks
  if (!tibble::is_tibble(qa_log)) {
    stop("qa_log must be a tibble or NULL.", call. = FALSE)
  }
  if (!is.character(question) || length(question) != 1L) {
    stop("question must be a single character string.", call. = FALSE)
  }
  if (!is.list(rag_result) || is.null(rag_result$answer)) {
    stop("rag_result must be a list returned by query_rag() with an 'answer' element.", call. = FALSE)
  }

  # Extract retrieved context from rag_result$retrieved if present
  retrieved_ids   <- character(0L)
  retrieved_texts <- character(0L)

  if (!is.null(rag_result$retrieved)) {
    retr <- rag_result$retrieved
    if (is.data.frame(retr)) {
      if ("id" %in% names(retr)) {
        retrieved_ids <- as.character(retr$id)
      }
      if ("text" %in% names(retr)) {
        retrieved_texts <- as.character(retr$text)
      }
    }
  }

  # Extract final prompt from rag_result$prompt if present
  prompt_final <- NA_character_
  if (!is.null(rag_result$prompt) && is.character(rag_result$prompt) && length(rag_result$prompt) >= 1L) {
    prompt_final <- as.character(rag_result$prompt[[1]])
  }

  # Compute qa_id if not supplied
  if (is.null(qa_id)) {
    if (nrow(qa_log) == 0L || !("qa_id" %in% names(qa_log))) {
      qa_id <- 1L
    } else {
      current_ids <- qa_log$qa_id
      if (all(is.na(current_ids))) {
        qa_id <- 1L
      } else {
        qa_id <- max(current_ids, na.rm = TRUE) + 1L
      }
    }
  }

  new_row <- tibble::tibble(
    qa_id            = as.integer(qa_id),
    question         = question,
    prompt_final     = prompt_final,                 # <-- NEW
    answer_model     = as.character(rag_result$answer),
    answer_reference = NA_character_,                # you can fill this in later
    collection       = collection,
    retrieved_ids    = list(retrieved_ids),
    retrieved_texts  = list(retrieved_texts),
    chat_model       = chat_model,
    embedding_model  = embedding_model,
    timestamp        = as.POSIXct(timestamp)
  )

  dplyr::bind_rows(qa_log, new_row)
}
