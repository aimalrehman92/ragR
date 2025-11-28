# scripts/dev/demo_rag.R
#
# Purpose:
#   Demonstrate how to query the RAG pipeline using an already-populated
#   vector store (without using the HTTP API).
#
# Usage (from project root):
#   Rscript scripts/dev/demo_rag.R
#
# Prerequisite:
#   - Run scripts/dev/demo_ingest.R at least once to create the collection.

library(ragR)

# 1. Define the question and collection -----------------------------------------

collection_name <- "demo_collection"

question <- "What is this essay about? Summarize it in one or two sentences."

cat("Asking question via RAG:\n")
cat("  ", question, "\n\n")

# 2. Call the RAG pipeline ------------------------------------------------------

rag_result <- query_rag(
  question        = question,
  collection      = collection_name,
  top_k           = 4,
  embedding_model = "text-embedding-3-small",
  chat_model      = "gpt-4o-mini"
)

# 3. Display the answer ---------------------------------------------------------

cat("Answer:\n")
cat(rag_result$answer, "\n\n")

# 4. Optionally show retrieved context -----------------------------------------

cat("Retrieved context (first few rows):\n")
print(utils::head(rag_result$retrieved))
