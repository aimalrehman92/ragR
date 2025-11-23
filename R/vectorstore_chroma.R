# R/vectorstore_chroma.R

#' Chroma configuration
#'
#' Reads configuration for the Chroma vector store. For now, this
#' simply returns a base URL taken from the CHROMA_BASE_URL environment
#' variable, or a default of http://localhost:8000.
#'
#' @return A list containing at least the base URL of the Chroma server.
#' @keywords internal
chroma_config <- function() {
  list(
    base_url = Sys.getenv("CHROMA_BASE_URL", unset = "http://localhost:8000")
  )
}

# Internal helper to build a Chroma request
chroma_request <- function(path) {
  cfg <- chroma_config()
  url <- paste0(rtrim_slash(cfg$base_url), path)
  httr2::request(url)
}

# Remove trailing slash from a URL, if present
rtrim_slash <- function(x) {
  sub("/+$", "", x)
}

#' Upsert embeddings into Chroma
#'
#' Backend-specific implementation of upserts. This assumes a running
#' Chroma server with an HTTP API. The exact endpoint may vary depending
#' on the Chroma version you use; this is a template to be adapted.
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
  # NOTE:
  # The concrete HTTP format here depends on your Chroma server setup.
  # For now, we keep this as a structured placeholder that you can
  # adapt to your chosen deployment / version of Chroma.
  #
  # Pseudocode-ish outline (to be filled in later when Chroma is set up):
  #
  # body <- list(
  #   collection = collection,
  #   ids        = as.list(ids),
  #   embeddings = split_embeddings(embeddings),
  #   documents  = as.list(documents),
  #   metadatas  = metadatas
  # )
  #
  # req <- chroma_request("/upsert") |>
  #   httr2::req_headers("Content-Type" = "application/json") |>
  #   httr2::req_body_json(body)
  #
  # resp <- httr2::req_perform(req)
  #
  # if (httr2::resp_status(resp) >= 300) {
  #   stop("Chroma upsert failed with status ", httr2::resp_status(resp), call. = FALSE)
  # }
  #
  # invisible(TRUE)

  stop("chroma_upsert() not implemented yet. Adapt this to your Chroma HTTP API.")
}

#' Query a Chroma collection
#'
#' Backend-specific implementation of nearest neighbor queries.
#'
#' @inheritParams vectorstore_query
#' @return A tibble with retrieved documents and similarity scores.
#' @keywords internal
chroma_query <- function(
  collection,
  query_embedding,
  top_k = 4L
) {
  # NOTE:
  # As with chroma_upsert(), the exact endpoint and payload depend on
  # your Chroma server version. This is a structured placeholder.
  #
  # body <- list(
  #   collection = collection,
  #   query_embedding = as.numeric(query_embedding),
  #   top_k = top_k
  # )
  #
  # req <- chroma_request("/query") |>
  #   httr2::req_headers("Content-Type" = "application/json") |>
  #   httr2::req_body_json(body)
  #
  # resp <- httr2::req_perform(req)
  #
  # if (httr2::resp_status(resp) >= 300) {
  #   stop("Chroma query failed with status ", httr2::resp_status(resp), call. = FALSE)
  # }
  #
  # parsed <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  #
  # # Here you would adapt to the shape of parsed and return a tibble:
  # # tibble::tibble(
  # #   id       = ...,
  # #   document = ...,
  # #   score    = ...,
  # #   metadata = ...
  # # )
  #
  # stop("chroma_query() parsing not implemented yet; adapt to your Chroma API response.")

  stop("chroma_query() not implemented yet. Adapt this to your Chroma HTTP API.")
}

#' Delete a Chroma collection
#'
#' Backend-specific deletion of a collection from Chroma.
#'
#' @inheritParams vectorstore_delete_collection
#' @return Invisibly, TRUE on success.
#' @keywords internal
chroma_delete_collection <- function(collection) {
  # NOTE:
  # Again, adjust this to your chosen Chroma deployment.
  #
  # req <- chroma_request(paste0("/collections/", collection)) |>
  #   httr2::req_method("DELETE")
  #
  # resp <- httr2::req_perform(req)
  #
  # if (httr2::resp_status(resp) >= 300) {
  #   stop("Chroma delete collection failed with status ",
  #        httr2::resp_status(resp), call. = FALSE)
  # }
  #
  # invisible(TRUE)

  stop("chroma_delete_collection() not implemented yet. Adapt this to your Chroma HTTP API.")
}
