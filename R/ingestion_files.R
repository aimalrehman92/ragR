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
#' - Replace carriage returns and newlines with spaces
#' - Remove control characters
#' - Collapse multiple whitespace to a single space
#' - Trim leading/trailing whitespace
#'
#' Embedding strategy (robust by default):
#' - Embeddings are computed in batches (default `embedding_batch_size = 128`)
#' - Batches are retried with exponential backoff on transient failures (e.g., 429/5xx)
#' - If a 400 occurs due to request size, the batch is automatically split
#' - Each chunk is truncated to a safe maximum length before embedding
#' - Optional resumable ingestion via a local checkpoint file
#'
#' @param paths Character vector of file paths to ingest.
#' @param collection Character scalar; name of the collection.
#' @param chunk_size Integer; target chunk size (in characters). Used for `"character"`.
#' @param chunk_overlap Integer; overlap between consecutive chunks. Used for `"character"`.
#' @param chunking_strategy Character; `"character"` or `"sentence"`.
#' @param embedding_model Character; OpenAI embedding model name.
#' @param embedding_batch_size Integer; number of chunks per embeddings request.
#'   Default 128. Internally capped for safety.
#' @param embedding_max_chars Integer; hard cap (in characters) applied to each chunk
#'   before embedding to avoid request failures. Default 8000.
#' @param retry Logical; whether to retry transient API failures. Default TRUE.
#' @param max_retries Integer; maximum retries per batch (transient failures). Default 5.
#' @param resume Logical; whether to use a local checkpoint to skip chunks already
#'   embedded in a previous run. Default TRUE.
#' @param checkpoint_path Optional character scalar; file path for the resume
#'   checkpoint. If NULL, uses a temp file based on the collection name.
#' @param use_openai Logical; if TRUE, use OpenAI embeddings. If FALSE,
#'   use a local dummy embedding generator (for offline development / tests).
#' @param verbose Logical; if TRUE, log progress messages.
#'
#' @return A tibble with a per-file ingestion summary (`collection`, `path`, `n_chunks`).
#' @export
ingest_documents <- function(
  paths,
  collection            = "default",
  chunk_size            = 500L,
  chunk_overlap         = 50L,
  chunking_strategy     = c("character", "sentence"),
  embedding_model       = "text-embedding-3-small",
  embedding_batch_size  = 128L,
  embedding_max_chars   = 8000L,
  retry                 = TRUE,
  max_retries           = 5L,
  resume                = TRUE,
  checkpoint_path       = NULL,
  use_openai            = TRUE,
  verbose               = TRUE
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
  if (!is.numeric(embedding_batch_size) || length(embedding_batch_size) != 1L || embedding_batch_size <= 0) {
    stop("embedding_batch_size must be a positive number.", call. = FALSE)
  }
  if (!is.numeric(embedding_max_chars) || length(embedding_max_chars) != 1L || embedding_max_chars <= 0) {
    stop("embedding_max_chars must be a positive number.", call. = FALSE)
  }
  if (!is.numeric(max_retries) || length(max_retries) != 1L || max_retries < 0) {
    stop("max_retries must be a non-negative number.", call. = FALSE)
  }

  chunk_size           <- as.integer(chunk_size)
  chunk_overlap        <- as.integer(chunk_overlap)
  embedding_batch_size <- as.integer(embedding_batch_size)
  embedding_max_chars  <- as.integer(embedding_max_chars)
  max_retries          <- as.integer(max_retries)

  # Safety cap: even if user requests huge batches, keep payload manageable
  embedding_batch_size <- min(embedding_batch_size, 256L)

  if (is.null(checkpoint_path) || !nzchar(checkpoint_path)) {
    checkpoint_path <- file.path(tempdir(), paste0("ragR_ingest_checkpoint_", collection, ".rds"))
  }

  all_chunks <- list()
  idx <- 1L

  for (p in paths) {
    if (verbose) message("Ingesting file: ", p)

    raw_txt <- extract_text(p)

    # Minimal, deterministic cleaning before chunking
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

  if (verbose) {
    message("Total chunks: ", nrow(chunks_df))
    message("Embedding model: ", embedding_model)
    message("Embedding batch size: ", embedding_batch_size)
    message("Embedding max chars per chunk: ", embedding_max_chars)
    message("Resume: ", if (resume) "TRUE" else "FALSE",
            " (checkpoint: ", checkpoint_path, ")")
  }

  # Prepare checkpoint (resumable ingestion)
  already_done_ids <- character(0L)
  if (resume && file.exists(checkpoint_path)) {
    ck <- tryCatch(readRDS(checkpoint_path), error = function(e) NULL)
    if (is.character(ck)) already_done_ids <- ck
    if (verbose && length(already_done_ids) > 0L) {
      message("Checkpoint loaded. Already embedded chunks: ", length(already_done_ids))
    }
  }

  # Remove chunks already embedded (based on local checkpoint)
  if (resume && length(already_done_ids) > 0L) {
    chunks_df <- chunks_df[!(chunks_df$id %in% already_done_ids), , drop = FALSE]
    if (verbose) message("Chunks remaining after resume-skip: ", nrow(chunks_df))
  }

  if (nrow(chunks_df) == 0L) {
    if (verbose) message("Nothing to ingest (all chunks were already embedded per checkpoint).")
    # Return original per-file summary (based on all chunks)
    summary_df <- dplyr::bind_rows(all_chunks) |>
      dplyr::count(path, name = "n_chunks") |>
      dplyr::mutate(collection = collection, .before = 1)
    return(summary_df)
  }

  # Clean + truncate chunks for embeddings
  trunc_stats <- list(truncated = 0L)
  chunks_df$text <- vapply(
    chunks_df$text,
    FUN = function(x) {
      y <- clean_text_for_embeddings(x)
      if (nchar(y) > embedding_max_chars) {
        trunc_stats$truncated <- trunc_stats$truncated + 1L
        y <- substr(y, 1L, embedding_max_chars)
      }
      y
    },
    FUN.VALUE = character(1L),
    USE.NAMES = FALSE
  )
  if (verbose && trunc_stats$truncated > 0L) {
    message("Truncated chunks for embedding: ", trunc_stats$truncated)
  }

  # Ingest in batches so progress persists even if a later batch fails
  n_total <- nrow(chunks_df)
  batch_starts <- seq.int(1L, n_total, by = embedding_batch_size)

  for (b in seq_along(batch_starts)) {
    start_i <- batch_starts[[b]]
    end_i   <- min(start_i + embedding_batch_size - 1L, n_total)

    batch_df <- chunks_df[start_i:end_i, , drop = FALSE]
    if (verbose) message(sprintf("Embedding batch %d/%d (%d chunks)",
                                 b, length(batch_starts), nrow(batch_df)))

    # Compute embeddings (batched + retry + auto-split on 400)
    if (use_openai) {
      if (!exists("get_openai_embeddings", mode = "function")) {
        stop("OpenAI embedding helper not found: expected `get_openai_embeddings()`.", call. = FALSE)
      }

      embeddings <- embed_texts_batched(
        texts           = batch_df$text,
        model           = embedding_model,
        retry           = retry,
        max_retries     = max_retries,
        verbose         = verbose
      )
    } else {
      if (verbose) message("Using dummy embeddings (no OpenAI call).")
      embeddings <- dummy_embeddings(batch_df$text, dims = 16L)
    }

    # Metadata
    metadatas <- lapply(seq_len(nrow(batch_df)), function(i) {
      list(
        path              = batch_df$path[i],
        collection        = batch_df$collection[i],
        chunk_index       = batch_df$chunk_index[i],
        chunking_strategy = chunking_strategy
      )
    })

    # Upsert this batch immediately (incremental / resumable)
    vectorstore_upsert(
      collection = collection,
      ids        = batch_df$id,
      embeddings = embeddings,
      documents  = batch_df$text,
      metadatas  = metadatas
    )

    # Update checkpoint
    if (resume) {
      already_done_ids <- c(already_done_ids, batch_df$id)
      # Write atomically-ish (best effort)
      tryCatch(saveRDS(unique(already_done_ids), checkpoint_path), error = function(e) NULL)
    }
  }

  # Final per-file summary (based on all chunks originally generated)
  summary_df <- dplyr::bind_rows(all_chunks) |>
    dplyr::count(path, name = "n_chunks") |>
    dplyr::mutate(collection = collection, .before = 1)

  summary_df
}

# --- Text cleaning helpers ----------------------------------------------------

#' Clean raw extracted text (minimal)
#'
#' - Forces UTF-8
#' - Replaces carriage returns and newlines with spaces
#' - Removes control characters
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
  x <- enc2utf8(text)
  x <- gsub("[\r\n]+", " ", x)
  x <- gsub("[[:cntrl:]]", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

#' Clean text for embeddings (minimal, deterministic)
#'
#' @param text Character scalar
#' @return Cleaned character scalar
#' @keywords internal
clean_text_for_embeddings <- function(text) {
  if (!is.character(text) || length(text) != 1L) {
    stop("text must be a single character string.", call. = FALSE)
  }
  x <- enc2utf8(text)
  # Ensure no line breaks or control chars leak into requests
  x <- gsub("[\r\n]+", " ", x)
  x <- gsub("[[:cntrl:]]", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

# --- Embeddings: batching + retry + auto-split --------------------------------

#' Embed texts robustly using OpenAI embeddings helper
#'
#' Calls `get_openai_embeddings()` on the provided `texts`. On transient failures,
#' retries with exponential backoff. On HTTP 400 (often request too large),
#' the batch is split recursively.
#'
#' @param texts Character vector
#' @param model Character scalar
#' @param retry Logical
#' @param max_retries Integer
#' @param verbose Logical
#' @return A numeric matrix of embeddings aligned with `texts`
#' @keywords internal
embed_texts_batched <- function(
  texts,
  model,
  retry       = TRUE,
  max_retries = 5L,
  verbose     = TRUE
) {
  texts <- as.character(texts)
  if (length(texts) == 0L) stop("texts is empty.", call. = FALSE)

  # Recursive splitter for 400s
  embed_split <- function(txts) {
    attempt <- 0L
    repeat {
      attempt <- attempt + 1L
      res <- tryCatch(
        get_openai_embeddings(texts = txts, model = model),
        error = function(e) e
      )

      if (!inherits(res, "error")) {
        return(res)
      }

      msg <- conditionMessage(res)

      # Detect 400-ish failures (message varies depending on httr2)
      is_400 <- grepl("HTTP\\s*400|400\\s*Bad\\s*Request", msg, ignore.case = TRUE)

      # Transient failures (rate limit / service / gateway)
      is_transient <- grepl("HTTP\\s*(429|500|502|503|504)|Rate\\s*limit|Service\\s*Unavailable|timeout",
                            msg, ignore.case = TRUE)

      # If request too large: split the batch
      if (is_400 && length(txts) > 1L) {
        mid <- floor(length(txts) / 2L)
        if (verbose) message("Embedding request too large (400). Splitting batch into ", mid, " + ", length(txts) - mid)
        left  <- embed_split(txts[seq_len(mid)])
        right <- embed_split(txts[(mid + 1L):length(txts)])
        return(rbind(left, right))
      }

      # If single item still causes 400, surface a clear message
      if (is_400 && length(txts) == 1L) {
        stop(
          "Embedding failed with HTTP 400 for a single chunk. This usually means the chunk is too large ",
          "or contains problematic characters. Try reducing `chunk_size`, lowering `embedding_max_chars`, ",
          "or cleaning the source text.",
          call. = FALSE
        )
      }

      # Retry transient failures if enabled
      if (retry && is_transient && attempt <= max_retries) {
        sleep_s <- min(2^(attempt - 1L), 30L)
        if (verbose) message("Transient embedding error (attempt ", attempt, "/", max_retries, "). Retrying in ", sleep_s, "s...")
        Sys.sleep(sleep_s)
        next
      }

      # Give up
      stop(res)
    }
  }

  embed_split(texts)
}

# --- Text extraction -----------------------------------------------------------

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
