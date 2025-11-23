# R/vectorstore_interface.R

#' Upsert embeddings into a vector store
#'
#' Generic interface for inserting or updating vector embeddings
#' into a named collection. Currently delegates to the Chroma backend.
#'
#' @param collection Character; name of the collection.
#' @param ids Character vector of IDs, one for each embedding.
#' @param embeddings Numeric matrix or data frame; one row per id.
#' @param documents Character vector of raw text for each embedding.
#' @param metadatas Optional list or data frame of metadata per row.
#'
#' @return Invisibly, TRUE on success.
#' @export
vectorstore_upsert <- function(
  collection,
  ids,
  embeddings,
  documents,
  metadatas = NULL
) {
  # Basic validation
  n <- length(ids)
  if (nrow(embeddings) != n) {
    stop("embeddings must have one row per id.", call. = FALSE)
  }
  if (length(documents) != n) {
    stop("documents must have length equal to length(ids).", call. = FALSE)
  }
  if (!is.null(metadatas) && length(metadatas) != n) {
    stop("metadatas must have length equal to length(ids).", call. = FALSE)
  }

  # Delegate to Chroma backend (can be swapped later if needed)
  chroma_upsert(
    collection = collection,
    ids        = ids,
    embeddings = embeddings,
    documents  = documents,
    metadatas  = metadatas
  )

  invisible(TRUE)
}

#' Query a vector store for nearest neighbors
#'
#' Generic interface for querying a collection with a single query
#' embedding. Returns the top-k most similar chunks.
#'
#' @param collection Character; name of the collection.
#' @param query_embedding Numeric vector representing the query.
#' @param top_k Integer; number of neighbors to retrieve.
#'
#' @return A tibble with at least: id, document, score, metadata.
#' @export
vectorstore_query <- function(
  collection,
  query_embedding,
  top_k = 4L
) {
  chroma_query(
    collection      = collection,
    query_embedding = query_embedding,
    top_k           = top_k
  )
}

#' Delete a collection from the vector store
#'
#' @param collection Character; collection name to delete.
#'
#' @return Invisibly, TRUE on success.
#' @export
vectorstore_delete_collection <- function(collection) {
  chroma_delete_collection(collection)
  invisible(TRUE)
}
