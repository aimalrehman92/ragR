# R/ingestion_files.R

#' Ingest documents into the vector store
#'
#' Reads PDF, DOCX, or TXT files, extracts text, chunks it, embeds chunks,
#' and stores them in a vector store collection.
#'
#' Chunking strategies:
#' - `"character"`: overlapping fixed-size character windows (current behavior).
#' - `"sentence"`: sentence-by-sentence packing into chunks up to `chunk_size`
#'   characters; overlap is applied in *sentences* (the last `chunk_overlap`
#'   sentences of the previous chunk are prepended to the next chunk).
#'
#' @param paths Character vector of file paths to ingest.
#' @param collection Character scalar; name of the collection.
#' @param chunk_size Integer; target chunk size (in characters).
#' @param chunk_overlap Integer; overlap between consecutive chunks.
#'   For `"character"` chunking: number of overlapping characters.
#'   For `"sentence"` chunking: number of overlapping sentences.
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

  if (!is.character(paths)) {
    stop("paths must be a character vector.", call. = FALSE)
  }
  if (length(paths) == 0L) {
    stop("paths must have length >= 1.", call. = FALSE)
  }
  if (!is.numeric(chunk_size) || length(chunk_size) != 1L || chunk_size <= 0) {
    stop("chunk_size must be a positive number.", call. = FALSE)
  }
  if (!is.numeric(chunk_overlap) || length(chunk_overlap) != 1L || chunk_overlap < 0) {
    stop("chunk_overlap must be a non-negative number.", call. = FALSE)
  }

  # 1) Extract and chunk all documents -------------------------------------

  all_chunks <- list()
  idx <- 1L

  for (p in paths) {
    if (verbose) message("Ingesting file: ", p)

    txt <- extract_text(p)

    chunks <- switch(
      chunking_strategy,
      character = chunk_text_character(
        text          = txt,
        chunk_size    = as.integer(chunk_size),
        chunk_overlap = as.integer(chunk_overlap)
      ),
      sentence = chunk_text_sentence(
        text          = txt,
        chunk_size    = as.integer(chunk_size),
        chunk_overlap = as.integer(chunk_overlap)
      )
    )

    n_chunks <- length(chunks)
    if (n_chunks == 0L) next

    # Create IDs like "<basename>_0001", "<basename>_0002", ...
    base <- tools::file_path_sans_ext(basename(p))
    ids  <- sprintf("%s_%04d", base, seq_len(n_chunks))

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

  if (verbose) message("Total chunks: ", nrow(chunks_df))

  # 2) Compute embeddings for all chunks -----------------------------------

  if (use_openai) {
    embeddings <- get_openai_embeddings(
      texts = chunks_df$text,
      model = embedding_model
    )
  } else {
    if (verbose) message("Using dummy embeddings (no OpenAI call).")
    embeddings <- dummy_embeddings(chunks_df$text, dims = 16L)
  }

  # 3) Build metadata list for each chunk ----------------------------------

  metadatas <- lapply(seq_len(nrow(chunks_df)), function(i) {
    list(
      path        = chunks_df$path[i],
      collection  = chunks_df$collection[i],
      chunk_index = chunks_df$chunk_index[i],
      chunking_strategy = chunking_strategy
    )
  })

  # 4) Upsert into the vector store ----------------------------------------

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

# --- Chunking: sentence packing -----------------------------------------------

#' Chunk text by packing sentences into chunks
#'
#' Sentences are split using a simple punctuation heuristic. Sentences are
#' then packed into chunks up to `chunk_size` characters (approx).
#'
#' Overlap behavior: the last `chunk_overlap` sentences from the previous
#' chunk are prepended to the next chunk.
#'
#' @param text Character scalar.
#' @param chunk_size Integer, target max chunk length (characters).
#' @param chunk_overlap Integer, overlap between chunks (sentences).
#'
#' @return Character vector of chunks.
#' @keywords internal
chunk_text_sentence <- function(text, chunk_size = 500L, chunk_overlap = 2L) {
  if (!is.character(text) || length(text) != 1L) {
    stop("text must be a single character string.", call. = FALSE)
  }

  txt <- trimws(text)
  if (!nzchar(txt)) return(character(0L))

  # Split into sentences (heuristic)
  sentences <- unlist(
    strsplit(txt, "(?<=[.!?])\\s+", perl = TRUE),
    use.names = FALSE
  )
  sentences <- trimws(sentences)
  sentences <- sentences[nzchar(sentences)]

  if (length(sentences) == 0L) return(character(0L))

  # Pack sentences into chunks
  chunks <- character(0L)
  cur <- character(0L)
  cur_n <- 0L

  flush_chunk <- function(cur_sentences) {
    paste(cur_sentences, collapse = " ")
  }

  for (s in sentences) {
    s_len <- nchar(s)
    add_len <- if (length(cur) == 0L) s_len else (1L + s_len) # + space

    if (length(cur) > 0L && (cur_n + add_len) > chunk_size) {
      # emit current chunk
      chunks <- c(chunks, flush_chunk(cur))

      # start next chunk with overlap in sentences
      ov <- as.integer(chunk_overlap)
      if (is.na(ov) || ov < 0L) ov <- 0L
      if (ov > 0L) {
        keep <- utils::tail(cur, min(ov, length(cur)))
        cur <- keep
        cur_n <- nchar(flush_chunk(cur))
      } else {
        cur <- character(0L)
        cur_n <- 0L
      }
    }

    # If a single sentence is longer than chunk_size, we still keep it as its own chunk
    if (length(cur) == 0L && s_len > chunk_size) {
      chunks <- c(chunks, s)
      next
    }

    # append sentence
    cur <- c(cur, s)
    cur_n <- nchar(flush_chunk(cur))
  }

  if (length(cur) > 0L) {
    chunks <- c(chunks, flush_chunk(cur))
  }

  chunks
}
