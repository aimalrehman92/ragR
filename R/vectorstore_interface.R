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
#' @return A tibble with one row per stored chunk, or zero rows if the store is empty.
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

  store <- readRDS(p)

  if (!is.data.frame(store)) {
    stop("Vector store file is not a data frame/tibble.", call. = FALSE)
  }

  required_cols <- c("collection", "id", "text", "embedding", "metadata")
  missing_cols <- setdiff(required_cols, names(store))

  if (length(missing_cols) > 0L) {
    stop(
      "Vector store is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  tibble::as_tibble(store)
}

# Internal helper: save the store
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
  if (!is.character(collection) || length(collection) != 1L || !nzchar(collection)) {
    stop("collection must be a single non-empty character string.", call. = FALSE)
  }
  if (!is.character(ids) || length(ids) == 0L) {
    stop("ids must be a non-empty character vector.", call. = FALSE)
  }
  if (any(is.na(ids)) || any(!nzchar(ids))) {
    stop("ids must not contain NA or empty strings.", call. = FALSE)
  }
  if (!is.matrix(embeddings) && !is.data.frame(embeddings)) {
    stop("embeddings must be a numeric matrix or data frame.", call. = FALSE)
  }
  if (!is.character(documents)) {
    stop("documents must be a character vector.", call. = FALSE)
  }

  n <- length(ids)

  if (nrow(embeddings) != n) {
    stop("embeddings must have one row per id.", call. = FALSE)
  }
  if (length(documents) != n) {
    stop("documents must have length equal to length(ids).", call. = FALSE)
  }
  if (any(is.na(documents))) {
    stop("documents must not contain NA values.", call. = FALSE)
  }
  if (!is.null(metadatas) && length(metadatas) != n) {
    stop("metadatas must have length equal to length(ids).", call. = FALSE)
  }

  emb_mat <- as.matrix(embeddings)
  storage.mode(emb_mat) <- "double"

  if (anyNA(emb_mat)) {
    stop("embeddings must not contain NA values.", call. = FALSE)
  }

  emb_list <- lapply(seq_len(n), function(i) {
    as.numeric(emb_mat[i, ])
  })

  if (is.null(metadatas)) {
    metadatas <- replicate(n, list(), simplify = FALSE)
  }

  if (is.data.frame(metadatas)) {
    metadatas <- split(metadatas, seq_len(nrow(metadatas)))
  }

  new_rows <- tibble::tibble(
    collection = rep(collection, n),
    id         = ids,
    text       = documents,
    embedding  = emb_list,
    metadata   = metadatas
  )

  store <- vectorstore_load()

  if (nrow(store) > 0L) {
    store <- store[
      !(store$collection == collection & store$id %in% ids),
      ,
      drop = FALSE
    ]
  }

  store <- dplyr::bind_rows(store, new_rows)

  vectorstore_save(store)

  invisible(TRUE)
}

# Internal: cosine similarity between a matrix of embeddings and a single vector.
cosine_similarity <- function(emb_mat, q) {
  q <- as.numeric(q)
  emb_mat <- as.matrix(emb_mat)
  storage.mode(emb_mat) <- "double"

  if (ncol(emb_mat) != length(q)) {
    stop("Embedding dimension mismatch between stored embeddings and query.", call. = FALSE)
  }

  dots <- as.numeric(emb_mat %*% q)
  emb_norms <- sqrt(rowSums(emb_mat^2))
  q_norm <- sqrt(sum(q^2))

  denom <- emb_norms * q_norm
  sims <- dots / denom

  sims[!is.finite(sims)] <- 0
  sims
}

#' Query the R-native vector store for nearest neighbors
#'
#' @param collection Character; name of the collection.
#' @param query_embedding Numeric vector representing the query.
#' @param top_k Integer; number of neighbors to retrieve.
#'
#' @return A tibble with columns `collection`, `id`, `text`, `score`, and `metadata`.
#' @export
vectorstore_query <- function(
  collection,
  query_embedding,
  top_k = 4L
) {
  if (!is.character(collection) || length(collection) != 1L || !nzchar(collection)) {
    stop("collection must be a single non-empty character string.", call. = FALSE)
  }
  if (!is.numeric(query_embedding) || length(query_embedding) == 0L) {
    stop("query_embedding must be a non-empty numeric vector.", call. = FALSE)
  }
  if (anyNA(query_embedding)) {
    stop("query_embedding must not contain NA values.", call. = FALSE)
  }
  if (!is.numeric(top_k) || length(top_k) != 1L || is.na(top_k) || top_k <= 0) {
    stop("top_k must be a positive integer.", call. = FALSE)
  }

  top_k <- as.integer(top_k)

  store <- vectorstore_load()
  subset <- store[store$collection == collection, , drop = FALSE]

  if (nrow(subset) == 0L) {
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

  emb_mat <- do.call(rbind, lapply(subset$embedding, as.numeric))

  sims <- cosine_similarity(emb_mat, query_embedding)

  subset$score <- sims
  subset <- subset[order(subset$score, decreasing = TRUE), , drop = FALSE]

  if (nrow(subset) > top_k) {
    subset <- subset[seq_len(top_k), , drop = FALSE]
  }

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
  if (!is.character(collection) || length(collection) != 1L || !nzchar(collection)) {
    stop("collection must be a single non-empty character string.", call. = FALSE)
  }

  store <- vectorstore_load()

  if (nrow(store) == 0L) {
    return(invisible(TRUE))
  }

  store <- store[store$collection != collection, , drop = FALSE]

  vectorstore_save(store)

  invisible(TRUE)
}

#' Delete ALL collections from the vector store
#'
#' Completely wipes the vector store by replacing it with an empty tibble.
#'
#' @return Invisibly, TRUE.
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