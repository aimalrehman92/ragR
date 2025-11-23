# R/ingestion_files.R

#' Ingest documents into the vector store
#'
#' Reads PDF, DOCX, or TXT files, extracts text, chunks it, embeds chunks,
#' and stores them in a vector store collection.
#'
#' @param paths Character vector of file paths.
#' @param collection Name of the vector store collection.
#' @param chunk_size Approximate size of each chunk (characters).
#' @param chunk_overlap Overlap between chunks (characters).
#' @param embedding_model OpenAI embedding model name.
#' @param verbose Whether to print progress messages.
#'
#' @return A tibble summarizing ingestion results (one row per file).
#' @export
ingest_documents <- function(
  paths,
  collection      = "default",
  chunk_size      = 500L,
  chunk_overlap   = 50L,
  embedding_model = "text-embedding-3-small",
  verbose         = TRUE
) {
  if (!is.character(paths)) {
    stop("paths must be a character vector.", call. = FALSE)
  }
  if (length(paths) == 0L) {
    stop("paths must have length >= 1.", call. = FALSE)
  }

  # 1) Extract and chunk all documents -------------------------------------

  all_chunks <- list()
  idx <- 1L

  for (p in paths) {
    if (verbose) {
      message("Ingesting file: ", p)
    }

    txt <- extract_text(p)
    chunks <- chunk_text(
      text          = txt,
      chunk_size    = chunk_size,
      chunk_overlap = chunk_overlap
    )

    n_chunks <- length(chunks)

    if (n_chunks == 0L) next

    # Create IDs like "<basename>_0001", "<basename>_0002", ...
    base <- tools::file_path_sans_ext(basename(p))
    ids <- sprintf("%s_%04d", base, seq_len(n_chunks))

    all_chunks[[idx]] <- tibble::tibble(
      path        = p,
      collection  = collection,
      id          = ids,
      chunk_index = seq_len(n_chunks),
      text        = chunks
    )
    idx <- idx + 1L
  }

  if (length(all_chunks) == 0L) {
    stop("No chunks were generated from the provided paths.", call. = FALSE)
  }

  chunks_df <- dplyr::bind_rows(all_chunks)

  if (verbose) {
    message("Total chunks: ", nrow(chunks_df))
  }

  # 2) Compute embeddings for all chunks -----------------------------------

  embeddings <- get_openai_embeddings(
    texts = chunks_df$text,
    model = embedding_model
  )

  # 3) Build metadata list for each chunk ----------------------------------

  metadatas <- lapply(seq_len(nrow(chunks_df)), function(i) {
    list(
      path        = chunks_df$path[i],
      collection  = chunks_df$collection[i],
      chunk_index = chunks_df$chunk_index[i]
    )
  })

  # 4) Upsert into the vector store (e.g., Chroma) -------------------------

  vectorstore_upsert(
    collection = collection,
    ids        = chunks_df$id,
    embeddings = embeddings,
    documents  = chunks_df$text,
    metadatas  = metadatas
  )

  # 5) Return a per-file summary -------------------------------------------

  summary_df <- chunks_df |>
    dplyr::count(path, name = "n_chunks") |>
    dplyr::mutate(collection = collection, .before = 1)

  summary_df
}

#' Extract text from a document
#'
#' Detects file type and extracts raw text from PDF, DOCX, or TXT.
#'
#' @param path Path to a single file.
#'
#' @return Character scalar containing extracted text.
#' @keywords internal

extract_text <- function(path) {
  ext <- tolower(tools::file_ext(path))

  if (!file.exists(path)) {
    stop(sprintf("File does not exist: %s", path))
  }

  if (ext == "pdf") {
    # Returns a character vector (one element per page)
    pages <- pdftools::pdf_text(path)
    return(paste(pages, collapse = "\n\n"))
  }

  if (ext %in% c("docx", "doc")) {
    txt <- readtext::readtext(path)$text
    return(txt)
  }

  if (ext %in% c("txt", "md")) {
    lines <- readLines(path, warn = FALSE)
    return(paste(lines, collapse = "\n"))
  }

  stop(sprintf("Unsupported file type: %s", ext))
}


#' Chunk text into overlapping segments
#'
#' @param text Character scalar.
#' @param chunk_size Numeric, target chunk length.
#' @param chunk_overlap Numeric, overlap between consecutive chunks.
#'
#' @return Character vector of chunks.
#' @keywords internal
chunk_text <- function(text, chunk_size = 500L, chunk_overlap = 50L) {
  if (!is.character(text) || length(text) != 1) {
    stop("text must be a single character string.")
  }

  txt <- trimws(text)

  n <- nchar(txt)
  if (n <= chunk_size) {
    return(txt)
  }

  chunks <- list()
  start <- 1

  repeat {
    end <- min(start + chunk_size - 1, n)
    chunks[[length(chunks) + 1]] <- substr(txt, start, end)

    if (end == n) break

    start <- start + (chunk_size - chunk_overlap)
  }

  unlist(chunks, use.names = FALSE)
}

