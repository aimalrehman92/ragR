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
#   - Adjust `example_paths` and `collection_name` as needed.
#   - Choose chunking strategy via `chunking_strategy`.
#   - This script only performs ingestion; it does not ask questions.

library(ragR)

# 1. Define which files to ingest ------------------------------------------------

example_paths <- c(
  "data-raw/STAT_8670_Syllabus.txt"   # adjust or add more paths if needed
)

existing_paths <- example_paths[file.exists(example_paths)]

if (length(existing_paths) == 0L) {
  stop(
    "No data files found. Please check that the paths exist under data-raw/ ",
    call. = FALSE
  )
}

cat("Ingesting the following files:\n")
print(existing_paths)

# 2. Choose chunking strategy ----------------------------------------------------
# Options: "character" or "sentence"
chunking_strategy <- "sentence"

# 3. Run ingestion --------------------------------------------------------------

collection_name <- "default"

# Build args robustly (handles whichever arg name you used in ingest_documents)
ingest_args <- list(
  paths           = existing_paths,
  collection      = collection_name,
  chunk_size      = 500,
  chunk_overlap   = 50,
  embedding_model = "text-embedding-3-small",
  verbose         = TRUE
)

fmls <- names(formals(ragR::ingest_documents))

if ("chunking_strategy" %in% fmls) {
  ingest_args$chunking_strategy <- chunking_strategy
} else if ("chunking" %in% fmls) {
  ingest_args$chunking <- chunking_strategy
} else if ("chunking_method" %in% fmls) {
  ingest_args$chunking_method <- chunking_strategy
} else {
  message(
    "NOTE: ingest_documents() does not appear to expose a chunking strategy argument.\n",
    "      Proceeding without passing chunking_strategy."
  )
}

ingestion_result <- do.call(ragR::ingest_documents, ingest_args)

cat("\nIngestion finished.\n")
print(ingestion_result)
cat("\nCollection used:", collection_name, "\n")
cat("Chunking strategy used:", chunking_strategy, "\n")
