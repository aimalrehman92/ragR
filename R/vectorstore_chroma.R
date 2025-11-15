# R/vectorstore_chroma.R

#' Chroma configuration
#'
#' Reads configuration for the Chroma vector store. Eventually, this may
#' read from a YAML config file, but for now it simply returns defaults
#' or environment variables.
#'
#' @return A list containing at least the base URL of the Chroma server.
#' @keywords internal
chroma_config <- function() {
  list(
    base_url = Sys.getenv("CHROMA_BASE_URL", unset = "http://localhost:8000")
  )
}

#' Upsert embeddings into Chroma
#'
#' Backend-specific implementation of upserts. This will later contain
#' the REST API call to Chroma's `/upsert` endpoint.
#'
#' @inheritParams vectorstore_upsert
#' @return Invisibly, TRUE on success.
#' @keywords internal
chroma_upsert <- function(
  collection,
  ids,
  embeddings,
  documents,
  metadatas = NULL
) {
  stop("chroma_upsert() not implemented yet.")
}

#' Query a Chroma collection
#'
#' Backend-specific implementation of nearest neighbor queries.
#' This will later call Chroma's `/query` endpoint.
#'
#' @inheritParams vectorstore_query
#' @return A tibble with retrieved documents and similarity scores.
#' @keywords internal
chroma_query <- function(
  collection,
  query_embedding,
  top_k = 4L
) {
  stop("chroma_query() not implemented yet.")
}

#' Delete a Chroma collection
#'
#' Backend-specific deletion of a collection from Chroma.
#'
#' @inheritParams vectorstore_delete_collection
#' @return Invisibly, TRUE on success.
#' @keywords internal
chroma_delete_collection <- function(collection) {
  stop("chroma_delete_collection() not implemented yet.")
}

