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

collection_name <- "default"

question <- "What is the withdrawal deadline for STAT 8581?"

cat("Asking question via RAG:\n")
cat("  ", question, "\n\n")

# 2. Measure RAG time -----------------------------------------------------------

t_start <- proc.time()   # start timer

rag_result <- query_rag(
  question        = question,
  collection      = collection_name,
  top_k           = 5,
  embedding_model = "text-embedding-3-small",
  chat_model      = "gpt-4o-mini"
)

t_end <- proc.time()     # end timer
t_elapsed <- t_end - t_start
rag_time_sec <- as.numeric(t_elapsed["elapsed"])

# 3. Display the answer ---------------------------------------------------------

cat("Answer:\n")
cat(rag_result$answer, "\n\n")

# 4. Display retrieved context --------------------------------------------------

cat("Retrieved context (first few rows):\n")
print(utils::head(rag_result$retrieved))

# 5. Display RAG timing ---------------------------------------------------------

cat("\n---------------------------------------------\n")
cat("RAG pipeline completed in:", round(rag_time_sec, 3), "seconds\n")
cat("---------------------------------------------\n")
