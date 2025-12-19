#' Create an empty QA log tibble
#'
#' This helper creates an empty tibble with the columns used to store
#' question–answer pairs and their retrieval context. You typically use
#' this once at the start of an evaluation session.
#'
#' @return A tibble with zero rows and the standard QA log columns.
#' @export
qa_log_empty <- function() {
  tibble::tibble(
    qa_id            = integer(),          # unique id per interaction
    question         = character(),        # user question
    answer_model     = character(),        # model's answer
    answer_reference = character(),        # your ideal/gold answer (can be NA)
    collection       = character(),        # vectorstore collection name
    retrieved_ids    = vector("list", 0L), # list-column: character vectors of chunk ids
    retrieved_texts  = vector("list", 0L), # list-column: character vectors of chunk texts
    chat_model       = character(),        # e.g. "gpt-4o-mini"
    embedding_model  = character(),        # e.g. "text-embedding-3-small"
    timestamp        = as.POSIXct(character()), # when the answer was generated
    eval_id          = character()  # evaluation item id (from experiment CSV)
  )
}

#' Log a single RAG interaction into a QA log
#'
#' Given a question and the result returned by [query_rag()], this helper
#' constructs one row with question, model answer, retrieved context, and
#' metadata, and appends it to an existing QA log tibble.
#'
#' @param qa_log A tibble as created by [qa_log_empty()], or `NULL` to
#'   start a new log.
#' @param question Character scalar; the user question that was asked.
#' @param rag_result A list returned by [query_rag()], expected to contain
#'   at least components `answer` (character scalar) and `retrieved`
#'   (a tibble with columns `id` and `text`).
#' @param collection Character scalar; name of the collection that was
#'   queried in the vector store.
#' @param chat_model Character scalar; chat model name used by the RAG
#'   pipeline (e.g., `"gpt-4o-mini"`).
#' @param embedding_model Character scalar; embedding model name used to
#'   embed the question (e.g., `"text-embedding-3-small"`).
#' @param qa_id Optional integer id. If `NULL`, the id will be computed
#'   as one plus the current maximum `qa_id` in `qa_log` (or 1 if empty).
#' @param timestamp Optional timestamp for the interaction; defaults to
#'   [Sys.time()].
#'
#' @return A tibble containing the existing `qa_log` rows plus the new row.
#' @export
log_rag_interaction <- function(
  qa_log,
  question,
  rag_result,
  collection,
  chat_model      = "gpt-4o-mini",
  embedding_model = "text-embedding-3-small",
  qa_id           = NULL,
  timestamp       = Sys.time(),
  eval_id         = NA_character_
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

  # Compute qa_id if not supplied
  if (is.null(qa_id)) {
    if (nrow(qa_log) == 0L || !("qa_id" %in% names(qa_log))) {
      qa_id <- 1L
    } else {
      # handle all-NA just in case
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
    answer_model     = as.character(rag_result$answer),
    answer_reference = NA_character_,  # you can fill this in later
    collection       = collection,
    retrieved_ids    = list(retrieved_ids),
    retrieved_texts  = list(retrieved_texts),
    chat_model       = chat_model,
    embedding_model  = embedding_model,
    timestamp        = as.POSIXct(timestamp),
    eval_id          = as.character(eval_id)
  )

  dplyr::bind_rows(qa_log, new_row)
}
