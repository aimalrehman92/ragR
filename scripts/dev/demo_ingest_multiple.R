# scripts/dev/demo_ingest_multiple.R
#
# Purpose:
#   Ingest multiple files into the vector store one-by-one (no HTTP API).
#
# Usage (from project root):
#   Rscript scripts/dev/demo_ingest_multiple.R
#
# Notes:
#   - Set `chunking_strategy` to "character" or "sentence".
#   - Ingests each file separately so logs/prints are clearer.

library(ragR)

# ---------------- User settings ----------------

collection_name   <- "default"

# Choose one: "character" or "sentence"
chunking_strategy <- "sentence"

# Only used for character chunking:
chunk_size        <- 500L
chunk_overlap     <- 50L

embedding_model   <- "text-embedding-3-small"

example_paths <- c(
  "data-raw/happy_essay.txt",
  "data-raw/STAT_8670_Syllabus.txt"
)

# -----------------------------------------------

existing_paths <- example_paths[file.exists(example_paths)]

if (length(existing_paths) == 0L) {
  stop(
    "No example files found. Please check that the paths exist under data-raw/ ",
    call. = FALSE
  )
}

cat("Ingesting files one-by-one:\n")
print(existing_paths)
cat("\nCollection:", collection_name, "\n")
cat("Chunking strategy:", chunking_strategy, "\n\n")

all_results <- list()

for (p in existing_paths) {
  cat("------------------------------------------------------------\n")
  cat("Ingesting file:", p, "\n")

  res <- ingest_documents(
    paths             = p,
    collection        = collection_name,
    chunk_size        = chunk_size,
    chunk_overlap     = chunk_overlap,
    chunking_strategy = chunking_strategy,
    embedding_model   = embedding_model,
    verbose           = TRUE
  )

  print(res)
  all_results[[p]] <- res
}

cat("\n✅ Finished ingesting", length(existing_paths), "file(s).\n")
cat("Collection used:", collection_name, "\n")
cat("Chunking strategy used:", chunking_strategy, "\n")
