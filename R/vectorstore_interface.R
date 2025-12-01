# R/vectorstore_interface.R
#
# R-native vector store backend using an RDS file.
# This avoids any external DB, Python, or HTTP dependency.
#
# Data model:
# A tibble with columns:
# - collection : character
# - id         : character
# - text       : character (chunk text)
# - embedding  : list-column of numeric vectors
# - metadata   : list-column (e.g., list(path=..., chunk_index=..., ...))

# Internal helper: path to the RDS "database"
vectorstore_path <- function() {
  file.path("db", "vectorstore.rds")
}

#' Load the R-native vector store
#'
#' This function loads the current contents of the R-native vector store
#' from `db/vectorstore.rds`. If the file does not exist yet, it returns
#' an empty tibble with the expected columns:
#' `collection`, `id`, `text`, `embedding`, and `metadata`.
#'
#' @return A tibble with one row per stored chunk (or zero rows if the
#'   store is empty).
#' @export
vectorstore_load <- function() {
  p <- vectorstore_path()
  if (!file.exists(p)) {
    return(
      tibble::tibble(
        collection = character(),
        id         = character(),
        text       = character(),
        embedding  = list(),
        metadata   = list()
      )
    )
  }
  readRDS(p)
}

# Internal helper: save the store
# (kept internal; not exported)
vectorstore_save <- function(store) {
  dir.create("db", showWarnings = FALSE, recursive = TRUE)
  saveRDS(store, vectorstore_path())
}

#' Upsert embeddings into the R-native vector store
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

  # Ensure embeddings is a matrix
  emb_mat <- as.matrix(embeddings)

  # Build list-column of embedding vectors (one per row)
  emb_list <- lapply(seq_len(n), function(i) {
    as.numeric(emb_mat[i, ])
  })

  # If no metadatas provided, fill with empty lists
  if (is.null(metadatas)) {
    metadatas <- replicate(n, list(), simplify = FALSE)
  }

  # New rows tibble
  new_rows <- tibble::tibble(
    collection = rep(collection, n),
    id         = ids,
    text       = documents,
    embedding  = emb_list,
    metadata   = metadatas
  )

  store <- vectorstore_load()

  # Remove existing rows with same (collection, id) to implement "upsert"
  if (nrow(store) > 0) {
    store <- store[!(store$collection == collection & store$id %in% ids), , drop = FALSE]
  }

  # Append new rows
  store <- dplyr::bind_rows(store, new_rows)

  # Save back to disk
  vectorstore_save(store)

  invisible(TRUE)
}

# Cosine similarity between a matrix of embeddings and a single vector
# Each row of emb_mat is a vector; q is a numeric vector.
cosine_similarity <- function(emb_mat, q) {
  q <- as.numeric(q)
  # row-wise dot product
  dots <- as.numeric(emb_mat %*% q)
  emb_norms <- sqrt(rowSums(emb_mat^2))
  q_norm <- sqrt(sum(q^2))
  sims <- dots / (emb_norms * q_norm)
  sims
}

#' Query the R-native vector store for nearest neighbors
#'
#' @param collection Character; name of the collection.
#' @param query_embedding Numeric vector representing the query.
#' @param top_k Integer; number of neighbors to retrieve.
#'
#' @return A tibble with at least:
#'   - `collection`
#'   - `id`
#'   - `text`
#'   - `score` (similarity)
#'   - `metadata`
#' @export
vectorstore_query <- function(
  collection,
  query_embedding,
  top_k = 4L
) {
  store <- vectorstore_load()

  # Filter to the requested collection
  subset <- store[store$collection == collection, , drop = FALSE]

  if (nrow(subset) == 0) {
    # Return empty tibble with expected columns
    return(
      tibble::tibble(
        collection = character(),
        id         = character(),
        text       = character(),
        score      = numeric(),
        metadata   = list()
      )
    )
  }

  # Build embedding matrix
  emb_mat <- do.call(rbind, lapply(subset$embedding, as.numeric))

  sims <- cosine_similarity(emb_mat, query_embedding)

  subset$score <- sims

  # Order by descending similarity and take top_k
  subset <- subset[order(subset$score, decreasing = TRUE), , drop = FALSE]

  if (nrow(subset) > top_k) {
    subset <- subset[seq_len(top_k), , drop = FALSE]
  }

  # Return a tibble
  tibble::tibble(
    collection = subset$collection,
    id         = subset$id,
    text       = subset$text,
    score      = subset$score,
    metadata   = subset$metadata
  )
}

#' Delete a collection from the R-native vector store
#'
#' @param collection Character; collection name to delete.
#'
#' @return Invisibly, TRUE on success.
#' @export
vectorstore_delete_collection <- function(collection) {
  store <- vectorstore_load()

  if (nrow(store) == 0) {
    return(invisible(TRUE))
  }

  # Keep only rows NOT in this collection
  store <- store[store$collection != collection, , drop = FALSE]

  vectorstore_save(store)

  invisible(TRUE)
}

#' Delete ALL collections from the vector store
#'
#' Completely wipes the vector store by replacing it with an empty tibble.
#'
#' @return Invisibly, TRUE
#' @export
vectorstore_clear_all <- function() {
  empty <- tibble::tibble(
    collection = character(),
    id         = character(),
    text       = character(),
    embedding  = list(),
    metadata   = list()
  )
  vectorstore_save(empty)
  invisible(TRUE)
}
