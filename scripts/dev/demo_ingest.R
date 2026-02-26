# scripts/dev/demo_ingest.R
#
# Purpose:
#   Ingest one or more files into the vector store using ragR (no HTTP API).
#
# Usage (from project root):
#   Rscript scripts/dev/demo_ingest.R
#
# Notes:
#   - Set `chunking_strategy` to "character" or "sentence".
#   - Embeddings are computed in batches by default (robust for large corpora).
#   - Ingestion is resumable via a local checkpoint (can be disabled).
#   - This script only performs ingestion; it does not ask questions.

library(ragR)

# ---------------- User settings ----------------

collection_name   <- "default"

# Choose one: "character" or "sentence"
chunking_strategy <- "character"

# Only used for character chunking:
chunk_size    <- 3200L
chunk_overlap <- as.integer(round(0.30 * chunk_size))

embedding_model  <- "text-embedding-3-small"

# Robust defaults (recommended)
embedding_batch_size  <- 128L     # batches per embeddings request (internally capped)
embedding_max_chars   <- 8000L    # truncate each chunk to this many chars before embedding
retry                 <- TRUE
max_retries           <- 5L
resume                <- TRUE     # use a local checkpoint to skip already-embedded chunks

# Optional: explicit checkpoint path (leave NULL to use tempdir())
checkpoint_path       <- NULL

example_paths <- c(
  "data-raw/Math 8600 Syllabus.pdf",
  "data-raw/STAT8670_syllabus_Sep2.pdf",
  "data-raw/Stat8760Syllabus.pdf",
  "data-raw/Syllabus_STAT8672_Lin_Spring2026.pdf",
  "data-raw/Syllabus4752_6752_Lin_Fall2025.pdf"
)

# -----------------------------------------------

existing_paths <- example_paths[file.exists(example_paths)]

if (length(existing_paths) == 0L) {
  stop(
    "No example files found. Please check that the paths exist under data-raw/ ",
    call. = FALSE
  )
}

cat("Ingesting the following files:\n")
print(existing_paths)
cat("\nCollection:", collection_name, "\n")
cat("Chunking strategy:", chunking_strategy, "\n")
cat("Embedding model:", embedding_model, "\n")
cat("Embedding batch size:", embedding_batch_size, "\n")
cat("Embedding max chars:", embedding_max_chars, "\n")
cat("Resume:", resume, "\n\n")

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

cat("\nIngestion finished.\n")
print(ingestion_result)
cat("\nCollection used:", collection_name, "\n")
cat("Chunking strategy used:", chunking_strategy, "\n")
