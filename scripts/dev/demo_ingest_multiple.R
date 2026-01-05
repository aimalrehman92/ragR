# scripts/dev/demo_ingest_multiple.R
#
# Purpose:
#   Ingest EACH file one-by-one into the vector store using ragR.
#
# Usage (from project root):
#   Rscript scripts/dev/demo_ingest_multiple.R
#
# Notes:
#   - Add or modify files inside file_paths below.
#   - Each file is ingested individually (separate ingest_documents() calls).
#   - Choose chunking strategy via `chunking_strategy`.

library(ragR)

# 1. Define list of files to ingest ---------------------------------------------

file_paths <- c(
  "data-raw/STAT_8581_Syllabus.txt",
  "data-raw/STAT_8670_Syllabus.txt"
)

existing_paths <- file_paths[file.exists(file_paths)]

if (length(existing_paths) == 0L) {
  stop(
    "No files found. Please check paths under data-raw/",
    call. = FALSE
  )
}

cat("Found", length(existing_paths), "file(s) to ingest.\n\n")

# 2. Choose chunking strategy ----------------------------------------------------
# Options: "character" or "sentence"
chunking_strategy <- "character"

# 3. Collection to use ----------------------------------------------------------

collection_name <- "default"

# Detect which chunking arg name exists in ingest_documents()
fmls <- names(formals(ragR::ingest_documents))

get_chunking_arg_name <- function() {
  if ("chunking_strategy" %in% fmls) return("chunking_strategy")
  if ("chunking" %in% fmls) return("chunking")
  if ("chunking_method" %in% fmls) return("chunking_method")
  return(NA_character_)
}

chunk_arg_name <- get_chunking_arg_name()

if (is.na(chunk_arg_name)) {
  message(
    "NOTE: ingest_documents() does not appear to expose a chunking strategy argument.\n",
    "      Proceeding without passing chunking_strategy."
  )
}

# 4. Ingest each file separately ------------------------------------------------

for (path in existing_paths) {
  cat("------------------------------------------------------------\n")
  cat("Ingesting file:", path, "\n")

  ingest_args <- list(
    paths           = path,  # ONE file per iteration
    collection      = collection_name,
    chunk_size      = 500,
    chunk_overlap   = 50,
    embedding_model = "text-embedding-3-small",
    verbose         = TRUE
  )

  if (!is.na(chunk_arg_name)) {
    ingest_args[[chunk_arg_name]] <- chunking_strategy
  }

  result <- do.call(ragR::ingest_documents, ingest_args)

  cat("\nIngestion finished for:", path, "\n")
  print(result)
  cat("------------------------------------------------------------\n\n")
}

cat("ALL FILES INGESTED.\n")
cat("Collection used:", collection_name, "\n")
cat("Chunking strategy used:", chunking_strategy, "\n")
