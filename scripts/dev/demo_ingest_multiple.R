# scripts/dev/demo_ingest.R
#
# Purpose:
#   Ingest EACH file one-by-one into the vector store using ragR.
#
# Usage (from project root):
#   Rscript scripts/dev/demo_ingest.R
#
# Notes:
#   - Add or modify files inside file_paths below.
#   - Each file is ingested individually (separate ingest_documents() calls).
#   - All files go to the same collection unless you change it.

library(ragR)

# 1. Define list of files to ingest ---------------------------------------------

file_paths <- c(
  "data-raw/STAT_8581_Syllabus.txt",
  "data-raw/STAT_8670_Syllabus.txt"   # Add more if needed
)

# Keep only files that actually exist
existing_paths <- file_paths[file.exists(file_paths)]

if (length(existing_paths) == 0L) {
  stop(
    "No example files found. Please check paths under data-raw/",
    call. = FALSE
  )
}

cat("Found", length(existing_paths), "file(s) to ingest.\n\n")

# 2. Collection to use ----------------------------------------------------------

collection_name <- "default"

# 3. Ingest each file separately ------------------------------------------------

for (path in existing_paths) {
  cat("------------------------------------------------------------\n")
  cat("Ingesting file:", path, "\n")

  result <- ingest_documents(
    paths           = path,     # ONE file per iteration
    collection      = collection_name,
    chunk_size      = 500,
    chunk_overlap   = 50,
    embedding_model = "text-embedding-3-small",
    verbose         = TRUE
  )

  cat("\nIngestion finished for:", path, "\n")
  print(result)
  cat("------------------------------------------------------------\n\n")
}

cat("ALL FILES INGESTED.\n")
cat("Collection used:", collection_name, "\n")
