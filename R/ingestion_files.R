# R/ingestion_files.R

#' Ingest documents into the vector store
#'
#' Reads PDF, DOCX, or TXT files, extracts text, cleans it lightly, chunks it,
#' embeds chunks, and stores them in a vector store collection.
#'
#' Chunking strategies:
#' - `"character"`: overlapping fixed-size character windows.
#' - `"sentence"`: strict sentence splitting via `chunk_text_sentence()`.
#'
#' Cleaning strategy (minimal, deterministic):
#' - Replace \verb{\\r} and \verb{\\n}line breaks with spaces
#' - Collapse multiple whitespace to a single space
#' - Trim leading/trailing whitespace
#'
#' @param paths Character vector of file paths to ingest.
#' @param collection Character scalar; name of the collection.
#' @param chunk_size Integer; target chunk size (in characters). Used for `"character"`.
#' @param chunk_overlap Integer; overlap between consecutive chunks. Used for `"character"`.
#' @param chunking_strategy Character; `"character"` or `"sentence"`.
#' @param embedding_model Character; OpenAI embedding model name.
#' @param use_openai Logical; if TRUE, use OpenAI embeddings. If FALSE,
#'   use a local dummy embedding generator (for offline development / tests).
#' @param verbose Logical; if TRUE, log progress messages.
#'
#' @return A tibble with a per-file ingestion summary (`collection`, `path`, `n_chunks`).
#' @export
ingest_documents <- function(
  paths,
  collection         = "default",
  chunk_size         = 500L,
  chunk_overlap      = 50L,
  chunking_strategy  = c("character", "sentence"),
  embedding_model    = "text-embedding-3-small",
  use_openai         = TRUE,
  verbose            = TRUE
) {
  chunking_strategy <- match.arg(chunking_strategy)

  if (!is.character(paths) || length(paths) == 0L) {
    stop("paths must be a non-empty character vector.", call. = FALSE)
  }
  if (!is.numeric(chunk_size) || length(chunk_size) != 1L || chunk_size <= 0) {
    stop("chunk_size must be a positive number.", call. = FALSE)
  }
  if (!is.numeric(chunk_overlap) || length(chunk_overlap) != 1L || chunk_overlap < 0) {
    stop("chunk_overlap must be a non-negative number.", call. = FALSE)
  }

  chunk_size    <- as.integer(chunk_size)
  chunk_overlap <- as.integer(chunk_overlap)

  all_chunks <- list()
  idx <- 1L

  for (p in paths) {
    if (verbose) message("Ingesting file: ", p)

    raw_txt <- extract_text(p)

    # --- NEW: minimal cleaning applied consistently before chunking ---
    txt <- clean_raw_text(raw_txt)

    chunks <- switch(
      chunking_strategy,
      character = chunk_text_character(
        text          = txt,
        chunk_size    = chunk_size,
        chunk_overlap = chunk_overlap
      ),
      sentence = chunk_text_sentence(text = txt)
    )

    # Normalize chunks defensively
    chunks <- unlist(chunks, use.names = FALSE)
    chunks <- as.character(chunks)
    chunks <- trimws(chunks)
    chunks <- chunks[nzchar(chunks)]

    n_chunks <- length(chunks)
    if (n_chunks == 0L) {
      if (verbose) message("No chunks produced for file: ", p)
      next
    }

    base <- tools::file_path_sans_ext(basename(p))
    ids  <- sprintf("%s_%04d", base, seq_len(n_chunks))

    all_chunks[[idx]] <- tibble::tibble(
      path        = rep(p, n_chunks),
      collection  = rep(collection, n_chunks),
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

  if (verbose) message("Total chunks: ", nrow(chunks_df))

  # Embeddings
  if (use_openai) {
    # Robust to naming differences, so ingest won't break if you renamed the helper elsewhere.
    if (exists("get_openai_embeddings", mode = "function")) {
      embeddings <- get_openai_embeddings(texts = chunks_df$text, model = embedding_model)
    } else if (exists("get_openai_embeddings", mode = "function")) {
      embeddings <- get_openai_embeddings(texts = chunks_df$text, model = embedding_model)
    } else {
      stop(
        "OpenAI embedding helper not found. Expected `get_openai_embeddings()` ",
        "or `get_openai_embeddings()` to exist in the package.",
        call. = FALSE
      )
    }
  } else {
    if (verbose) message("Using dummy embeddings (no OpenAI call).")
    embeddings <- dummy_embeddings(chunks_df$text, dims = 16L)
  }

  # Metadata
  metadatas <- lapply(seq_len(nrow(chunks_df)), function(i) {
    list(
      path              = chunks_df$path[i],
      collection        = chunks_df$collection[i],
      chunk_index       = chunks_df$chunk_index[i],
      chunking_strategy = chunking_strategy
    )
  })

  vectorstore_upsert(
    collection = collection,
    ids        = chunks_df$id,
    embeddings = embeddings,
    documents  = chunks_df$text,
    metadatas  = metadatas
  )

  summary_df <- chunks_df |>
    dplyr::count(path, name = "n_chunks") |>
    dplyr::mutate(collection = collection, .before = 1)

  summary_df
}

# --- NEW: minimal text cleaning -----------------------------------------------

#' Clean raw extracted text (minimal)
#'
#' - Replaces \verb{\\r} and \verb{\\n} with spaces
#' - Collapses multiple whitespace
#' - Trims
#'
#' @param text Character scalar
#' @return Cleaned character scalar
#' @keywords internal
clean_raw_text <- function(text) {
  if (!is.character(text) || length(text) != 1L) {
    stop("text must be a single character string.", call. = FALSE)
  }
  x <- text
  x <- gsub("[\r\n]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
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
    stop(sprintf("File does not exist: %s", path), call. = FALSE)
  }

  if (ext == "pdf") {
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

  stop(sprintf("Unsupported file type: %s", ext), call. = FALSE)
}

# --- Chunking: character windows ----------------------------------------------

#' Chunk text into overlapping character segments
#'
#' @param text Character scalar.
#' @param chunk_size Integer, target chunk length (characters).
#' @param chunk_overlap Integer, overlap between consecutive chunks (characters).
#'
#' @return Character vector of chunks.
#' @keywords internal
chunk_text_character <- function(text, chunk_size = 500L, chunk_overlap = 50L) {
  if (!is.character(text) || length(text) != 1L) {
    stop("text must be a single character string.", call. = FALSE)
  }

  txt <- trimws(text)
  if (!nzchar(txt)) return(character(0L))

  n <- nchar(txt)
  if (n <= chunk_size) {
    return(c(txt))
  }

  step <- chunk_size - chunk_overlap
  if (step <= 0L) {
    stop("chunk_overlap must be smaller than chunk_size for character chunking.", call. = FALSE)
  }

  chunks <- list()
  start <- 1L

  repeat {
    end <- min(start + chunk_size - 1L, n)
    chunks[[length(chunks) + 1L]] <- substr(txt, start, end)
    if (end == n) break
    start <- start + step
  }

  unlist(chunks, use.names = FALSE)
}

# --- Chunking: sentence splitting (strict-ish) --------------------------------

#' Chunk text into sentences (strict)
#'
#' Splits text into individual sentences. Each returned element is intended
#' to be exactly one sentence (best-effort based on punctuation).
#'
#' @param text Character scalar.
#'
#' @return Character vector; one sentence per element.
#' @export
chunk_text_sentence <- function(text) {
  if (!is.character(text) || length(text) != 1L) {
    stop("text must be a single character string.", call. = FALSE)
  }

  x <- trimws(text)
  if (!nzchar(x)) {
    return(character(0L))
  }

  parts <- unlist(strsplit(x, "(?<=[.!?])\\s+", perl = TRUE), use.names = FALSE)
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]

  parts
}
