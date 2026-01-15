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
#   - Raw text is minimally cleaned inside ingest_documents(): newlines -> spaces.
#   - This script only performs ingestion; it does not ask questions.

library(ragR)

# ---------------- User settings ----------------

collection_name   <- "default"

# Choose one: "character" or "sentence"
chunking_strategy <- "character"

# Only used for character chunking:
chunk_size        <- 700L
chunk_overlap     <- 100L

embedding_model   <- "text-embedding-3-small"

example_paths <- c(
  "data-raw/STAT_8670_Syllabus.txt",
  "data-raw/STAT_8581_Syllabus.txt",
  "data-raw/happy_essay.pdf"
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
cat("Chunking strategy:", chunking_strategy, "\n\n")

ingestion_result <- ingest_documents(
  paths             = existing_paths,
  collection        = collection_name,
  chunk_size        = chunk_size,
  chunk_overlap     = chunk_overlap,
  chunking_strategy = chunking_strategy,
  embedding_model   = embedding_model,
  verbose           = TRUE
)

cat("\nIngestion finished.\n")
print(ingestion_result)
cat("\nCollection used:", collection_name, "\n")
cat("Chunking strategy used:", chunking_strategy, "\n")
