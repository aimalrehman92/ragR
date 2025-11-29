# scripts/dev/demo_ingest.R
#
# Purpose:
#   Demonstrate how to populate the vector store by ingesting one or more
#   local documents using the ragR package (without using the HTTP API).
#
# Usage (from project root):
#   Rscript scripts/dev/demo_ingest.R
#
# Notes:
#   - Adjust `example_paths` and `collection` as needed.
#   - This script only performs ingestion; it does not ask questions.

library(ragR)

# 1. Define which files to ingest ------------------------------------------------

example_paths <- c(
  "data-raw/happy_essay.txt"   # adjust or add more paths if needed
)

# Keep only the files that actually exist
existing_paths <- example_paths[file.exists(example_paths)]

if (length(existing_paths) == 0L) {
  stop(
    "No example files found. Please check that the paths exist under data-raw/ ",
    call. = FALSE
  )
}

cat("Ingesting the following files:\n")
print(existing_paths)

# 2. Run ingestion --------------------------------------------------------------

collection_name <- "default"

ingestion_result <- ingest_documents(
  paths           = existing_paths,
  collection      = collection_name,
  chunk_size      = 500,
  chunk_overlap   = 50,
  embedding_model = "text-embedding-3-small",
  verbose         = TRUE
)

cat("\nIngestion finished.\n")
print(ingestion_result)
cat("\nCollection used:", collection_name, "\n")
