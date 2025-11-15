# scripts/dev/demo_rag.R
#
# Minimal example of how the ragR package is intended to be used
# as a library (without the HTTP API).
#
# Usage (from project root):
#   Rscript scripts/dev/demo_rag.R
#
# NOTE:
# - This is a skeleton. The functions currently stop with
#   "not implemented yet" and will be filled in later.

library(ragR)

# 1. Ingest some example documents -----------------------------------------

example_paths <- c(
  "data-raw/example1.pdf",
  "data-raw/example2.txt"
)

ingestion_result <- ingest_documents(
  paths          = example_paths,
  collection     = "demo_collection",
  chunk_size     = 500,
  chunk_overlap  = 50,
  embedding_model = "text-embedding-3-small",
  verbose        = TRUE
)

print(ingestion_result)

# 2. Ask a question via the RAG pipeline -----------------------------------

question <- "What are the key points discussed in the example documents?"

rag_result <- query_rag(
  question        = question,
  collection      = "demo_collection",
  top_k           = 4,
  embedding_model = "text-embedding-3-small",
  chat_model      = "gpt-4o-mini"
)

cat("\nAnswer:\n")
cat(rag_result$answer, "\n")

cat("\nRetrieved context (first few rows):\n")
print(utils::head(rag_result$retrieved))
