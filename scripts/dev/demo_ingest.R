# scripts/dev/demo_ingest.R
#
# Purpose:
#   Ingest one or more files into the vector store using ragR (no HTTP API).
#
# Usage (from project root):
#   Rscript scripts/dev/demo_ingest.R
#
# Notes:
#   - Set `input_paths` to the files you want to ingest.
#   - Supported file types depend on ingest_documents().
#   - PDF ingestion may require the optional pdftools/poppler setup.
#   - Set `chunking_strategy` to "character" or "sentence".
#   - Embeddings are computed in batches by default.
#   - Ingestion is resumable via a local checkpoint.
#   - This script only performs ingestion; it does not ask questions.

library(ragR)

# ---------------- User settings ----------------

collection_name <- "default"

# Files to ingest.
# Use paths relative to the project root.
input_paths <- c(
  # "data-raw/social_media policy brief.pdf",
  "data-raw/Anatomy_Gray.txt",
  "data-raw/Anatomy_Gray.txt"
)

# Choose one: "character" or "sentence"
chunking_strategy <- "character"

# Only used for character chunking.
chunk_size    <- 1600L
chunk_overlap <- as.integer(round(0.30 * chunk_size))

embedding_model <- "text-embedding-3-small"

# Robust defaults.
embedding_batch_size <- 128L
embedding_max_chars  <- 8000L
retry                <- TRUE
max_retries          <- 5L
resume               <- TRUE

# Optional: explicit checkpoint path. Leave NULL to use tempdir().
checkpoint_path <- NULL

# -----------------------------------------------

existing_paths <- input_paths[file.exists(input_paths)]
missing_paths  <- input_paths[!file.exists(input_paths)]

if (length(existing_paths) == 0L) {
  stop(
    "No input files found. Please check the paths in `input_paths`.",
    call. = FALSE
  )
}

if (length(missing_paths) > 0L) {
  warning(
    "Some input files were not found and will be skipped: ",
    paste(missing_paths, collapse = ", "),
    call. = FALSE
  )
}

cat("Ingestion settings:\n")
cat("  Collection            :", collection_name, "\n")
cat("  Chunking strategy     :", chunking_strategy, "\n")
cat("  Chunk size            :", chunk_size, "\n")
cat("  Chunk overlap         :", chunk_overlap, "\n")
cat("  Embedding model       :", embedding_model, "\n")
cat("  Embedding batch size  :", embedding_batch_size, "\n")
cat("  Embedding max chars   :", embedding_max_chars, "\n")
cat("  Resume                :", resume, "\n")
cat("  Files found           :", length(existing_paths), "\n\n")

cat("Files to ingest:\n")
for (path in existing_paths) {
  cat("  -", path, "\n")
}
cat("\n")

ingestion_result <- ingest_documents(
  paths                = existing_paths,
  collection           = collection_name,
  chunk_size           = chunk_size,
  chunk_overlap        = chunk_overlap,
  chunking_strategy    = chunking_strategy,
  embedding_model      = embedding_model,
  embedding_batch_size = embedding_batch_size,
  embedding_max_chars  = embedding_max_chars,
  retry                = retry,
  max_retries          = max_retries,
  resume               = resume,
  checkpoint_path      = checkpoint_path,
  verbose              = TRUE
)

cat("\nIngestion finished successfully.\n")

if (is.list(ingestion_result) && "collection" %in% names(ingestion_result)) {
  cat("Collection:", ingestion_result[["collection"]], "\n")
} else {
  cat("Collection:", collection_name, "\n")
}

cat("Documents ingested:", length(existing_paths), "\n")

if (is.list(ingestion_result) && "n_chunks" %in% names(ingestion_result)) {
  cat("Chunks ingested:", ingestion_result[["n_chunks"]], "\n")
}