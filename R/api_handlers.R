# R/api_handlers.R

#' API handler for document ingestion
#'
#' This function is intended to be called from a plumber endpoint.
#' It expects a request body (already parsed from JSON) that contains
#' the paths of files to ingest and optional collection/config options.
#'
#' Expected fields in `body`:
#' - `paths` (required): character vector of file paths (server-side)
#' - `collection` (optional): collection name (default: "default")
#' - `chunk_size` (optional): integer, default 500
#' - `chunk_overlap` (optional): integer, default 50
#' - `embedding_model` (optional): embedding model name
#'
#' @param body A list parsed from JSON request body.
#'
#' @return A list suitable for JSON serialization, including a summary
#'   of ingestion results.
#' @export
api_ingest_handler <- function(body) {
  if (is.null(body) || !is.list(body)) {
    stop("Request body must be a list parsed from JSON.", call. = FALSE)
  }

  paths <- body$paths
  if (is.null(paths)) {
    stop("Request body must contain a 'paths' field.", call. = FALSE)
  }

  collection      <- if (!is.null(body$collection))      body$collection      else "default"
  chunk_size      <- if (!is.null(body$chunk_size))      body$chunk_size      else 500L
  chunk_overlap   <- if (!is.null(body$chunk_overlap))   body$chunk_overlap   else 50L
  embedding_model <- if (!is.null(body$embedding_model)) body$embedding_model else "text-embedding-3-small"

  summary_df <- ingest_documents(
    paths          = paths,
    collection     = collection,
    chunk_size     = chunk_size,
    chunk_overlap  = chunk_overlap,
    embedding_model = embedding_model,
    verbose        = TRUE
  )

  # plumber/jsonlite can serialize data frames directly
  list(
    status     = "ok",
    collection = collection,
    summary    = summary_df
  )
}

#' API handler for RAG chat
#'
#' This function is intended to be called from a plumber endpoint.
#' It expects a request body with a question and optional configuration.
#'
#' Expected fields in `body`:
#' - `question` (required): character scalar
#' - `collection` (optional): collection name (default: "default")
#' - `top_k` (optional): integer (default: 4)
#' - `embedding_model` (optional): embedding model for the question
#' - `chat_model` (optional): chat model name
#'
#' @param body A list parsed from JSON request body.
#'
#' @return A list suitable for JSON serialization, typically including:
#'   - `answer`: model answer
#'   - `retrieved`: retrieved chunks (as a data frame)
#'   - `model`: chat model used
#' @export
api_chat_handler <- function(body) {
  if (is.null(body) || !is.list(body)) {
    stop("Request body must be a list parsed from JSON.", call. = FALSE)
  }

  question <- body$question
  if (is.null(question) || !is.character(question) || length(question) != 1L) {
    stop("Request body must contain a character scalar 'question'.", call. = FALSE)
  }

  collection      <- if (!is.null(body$collection))      body$collection      else "default"
  top_k           <- if (!is.null(body$top_k))           body$top_k           else 4L
  embedding_model <- if (!is.null(body$embedding_model)) body$embedding_model else "text-embedding-3-small"
  chat_model      <- if (!is.null(body$chat_model))      body$chat_model      else "gpt-4o-mini"

  rag_res <- query_rag(
    question        = question,
    collection      = collection,
    top_k           = top_k,
    embedding_model = embedding_model,
    chat_model      = chat_model
  )

  list(
    status    = "ok",
    answer    = rag_res$answer,
    model     = rag_res$model,
    retrieved = rag_res$retrieved,
    prompt    = rag_res$prompt
  )
}


#' API handler to clear a collection ("memory")
#'
#' This function is intended to be called from a plumber endpoint.
#' It deletes a collection from the underlying vector store.
#'
#' Expected fields in `body`:
#' - `collection` (optional): collection name (default: "default")
#'
#' @param body A list parsed from JSON request body.
#'
#' @return A list suitable for JSON serialization, indicating status.
#' @export
api_clear_handler <- function(body) {
  if (!is.null(body) && !is.list(body)) {
    stop("Request body must be a list parsed from JSON or NULL.", call. = FALSE)
  }

  collection <- if (!is.null(body) && !is.null(body$collection)) {
    body$collection
  } else {
    "default"
  }

  vectorstore_delete_collection(collection)

  list(
    status     = "ok",
    collection = collection,
    message    = sprintf("Collection '%s' cleared.", collection)
  )
}


#' API handler: compute RAGAS metrics and summary from saved QA log
#'
#' This function is intended to be called from an HTTP endpoint. It loads
#' the QA log from disk (via [load_qa_log()]), computes RAGAS-style
#' metrics (via [compute_ragas_metrics()]) and a summary (via
#' [summarize_ragas()]), and returns a list that can be serialized as JSON.
#'
#' @param path Character scalar; path to the QA log RDS file. Defaults to
#'   `"db/qa_log.rds"`.
#'
#' @return A list with elements:
#'   - `status`: "ok"
#'   - `n_qa`: number of rows in the QA log
#'   - `metrics`: a data frame of per-QA metrics
#'   - `summary`: a data frame of summary statistics per metric
#' @export
api_ragas_handler <- function(path = "db/qa_log.rds") {
  qa_log <- load_qa_log(path)

  metrics <- compute_ragas_metrics(qa_log)
  summary <- summarize_ragas(metrics)

  list(
    status  = "ok",
    n_qa    = nrow(qa_log),
    metrics = metrics,
    summary = summary
  )
}

#' API handler: generate RAGAS performance report
#'
#' This handler is intended to be called from an HTTP endpoint. It
#' generates RAGAS-style metrics based on the saved QA log, writes a
#' summary table (CSV) and a bar chart image (PNG) to disk, and returns
#' a small list with paths and status information.
#'
#' @param qa_log_path Character scalar; path to the QA log RDS file.
#'   Defaults to `"db/qa_log.rds"`.
#' @param qa_metrics_path Character scalar; path to the QA metrics RDS
#'   file. Defaults to `"db/qa_metrics.rds"`.
#' @param output_dir Character scalar; directory where the summary CSV
#'   and PNG image will be written. Defaults to `"reports/ragas"`.
#'
#' @return A list suitable for JSON serialization, with elements:
#'   `status`, `n_qa`, `qa_metrics_path`, `summary_csv_path`, `plot_path`.
#' @export
api_ragas_report_handler <- function(
  qa_log_path     = "db/qa_log.rds",
  qa_metrics_path = "db/qa_metrics.rds",
  output_dir      = "reports/ragas"
) {
  generate_ragas_report(
    qa_log_path     = qa_log_path,
    qa_metrics_path = qa_metrics_path,
    output_dir      = output_dir
  )
}

#' API handler: clear QA log and QA metrics
#'
#' This handler overwrites the QA log and QA metrics files on disk with
#' empty tibbles, effectively resetting the evaluation state.
#'
#' @param qa_log_path Character scalar; path to the QA log RDS file.
#'   Defaults to `"db/qa_log.rds"`.
#' @param qa_metrics_path Character scalar; path to the QA metrics RDS
#'   file. Defaults to `"db/qa_metrics.rds"`.
#'
#' @return A list with elements `status`, `qa_log_path`, and
#'   `qa_metrics_path`, suitable for JSON serialization.
#' @export
api_ragas_clear_handler <- function(
  qa_log_path     = "db/qa_log.rds",
  qa_metrics_path = "db/qa_metrics.rds"
) {
  clear_qa_log(qa_log_path)
  clear_qa_metrics(qa_metrics_path)

  list(
    status          = "ok",
    qa_log_path     = qa_log_path,
    qa_metrics_path = qa_metrics_path
  )
}

#' API handler: clear the entire vector store
#'
#' This handler wipes the R-native vector store by calling
#' [vectorstore_clear_all()], removing all collections and stored
#' embeddings.
#'
#' @return A list with elements `status` and `message`, suitable for
#'   JSON serialization.
#' @export
api_clear_all_handler <- function() {
  vectorstore_clear_all()

  list(
    status  = "ok",
    message = "Vector store cleared: all collections removed."
  )
}
