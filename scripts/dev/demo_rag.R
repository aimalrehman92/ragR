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

# ---------------- User settings (edit these) ----------------------------------

collection <- "default"

questions <- c(
  "What is the attendance policy for STAT 8670?"
)

top_k <- 5L

embedding_model <- "text-embedding-3-small"
chat_model      <- "gpt-4o-mini"

temperature       <- 0
max_output_tokens <- 300L

score_threshold <- 0

# Example system prompt (used inside build_rag_prompt)
system_prompt <- "You are a helpful academic course assistant.\n\n"

# ------------------------------------------------------------------------------

cat("Asking questions via RAG:\n")
for (q in questions) cat("  ", q, "\n")
cat("\n")

cat("Settings:\n")
cat("  Collection       :", collection, "\n")
cat("  top_k            :", top_k, "\n")
cat("  embedding_model  :", embedding_model, "\n")
cat("  chat_model       :", chat_model, "\n")
cat("  temperature      :", temperature, "\n")
cat("  max_output_tokens:", max_output_tokens, "\n")
cat("  score_threshold  :", score_threshold, "\n\n")

for (question in questions) {
  cat("------------------------------------------------------------\n")
  cat("Q: ", question, "\n\n", sep = "")

  rag_result <- query_rag(
    question          = question,
    collection        = collection,
    top_k             = top_k,
    embedding_model   = embedding_model,
    chat_model        = chat_model,
    temperature       = temperature,
    max_output_tokens = max_output_tokens,
    score_threshold   = score_threshold,
    system_prompt     = system_prompt
  )

  cat("Answer:\n")
  cat(rag_result$answer, "\n\n")

  cat("Retrieved context (first few rows):\n")
  print(utils::head(rag_result$retrieved))
  cat("\n")
}
