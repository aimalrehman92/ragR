# R/api_handlers.R

#' API handler for document ingestion
#'
#' This function is intended to be called from a plumber endpoint.
#' It expects a request body (already parsed from JSON) that contains
#' the paths of files to ingest and optional collection/config options.
#'
#' Expected fields in \code{body}:
#' \itemize{
#'   \item \code{paths}: character vector of file paths (server-side)
#'   \item \code{collection}: optional collection name (default: "default")
#'   \item \code{chunk_size}: optional integer
#'   \item \code{chunk_overlap}: optional integer
#' }
#'
#' @param body A list parsed from JSON request body.
#'
#' @return A list suitable for JSON serialization (e.g., via plumber),
#'   usually including a summary of ingestion results.
#' @export
api_ingest_handler <- function(body) {
  stop("api_ingest_handler() not implemented yet.")
}

#' API handler for RAG chat
#'
#' This function is intended to be called from a plumber endpoint.
#' It expects a request body with a question and optional configuration.
#'
#' Expected fields in \code{body}:
#' \itemize{
#'   \item \code{question}: character scalar
#'   \item \code{collection}: optional collection name
#'   \item \code{top_k}: optional integer
#' }
#'
#' @param body A list parsed from JSON request body.
#'
#' @return A list suitable for JSON serialization, typically including:
#'   \itemize{
#'     \item \code{answer}: model answer
#'     \item \code{retrieved}: retrieved chunks (possibly simplified)
#'   }
#' @export
api_chat_handler <- function(body) {
  stop("api_chat_handler() not implemented yet.")
}
