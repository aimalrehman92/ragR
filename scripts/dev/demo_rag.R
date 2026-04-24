# scripts/dev/demo_rag.R
#
# Purpose:
#   Demonstrate how to query the RAG pipeline using an already-populated
#   vector store, without using the HTTP API and without writing to any DB.
#
# Usage (from project root):
#   Rscript scripts/dev/demo_rag.R
#
# Prerequisite:
#   - Run scripts/dev/demo_ingest.R at least once to create/populate the collection.

library(ragR)

# ---------------- User settings (edit these) ---------------------------------

collection_name <- "default"  # must match your ingestion collection

# Add as many questions as you want to demo in one run:
questions <- c(
  "What is anatomy?",
  "What is microscopic anatomy also called?",
  "What are the two main approaches to studying anatomy?",
  "What is the anatomical position?",
  "What do coronal planes divide the body into?",
  "What is the difference between proximal and distal?"
)

# Retrieval + model settings
top_k           <- 5L
score_threshold <- 0           # keep chunks with score >= this; 0 disables filtering

embedding_model <- "text-embedding-3-small"
chat_model      <- "gpt-4o-mini"

# Generation settings
temperature       <- 0
max_output_tokens <- 300L

# User-defined API-level system prompt
system_prompt <- "You are a helpful assistant."

# How many retrieved rows to print as a preview
preview_n <- 5L

# ------------------------------------------------------------------------------

cat("=== ragR Demo: RAG Query Only ===\n\n")

cat("Settings:\n")
cat("  Collection        :", collection_name, "\n")
cat("  top_k             :", top_k, "\n")
cat("  score_threshold   :", score_threshold, "\n")
cat("  embedding_model   :", embedding_model, "\n")
cat("  chat_model        :", chat_model, "\n")
cat("  temperature       :", temperature, "\n")
cat("  max_output_tokens :", max_output_tokens, "\n")
cat("  system_prompt     :", system_prompt, "\n")
cat("  questions         :", length(questions), "\n\n")

for (q in questions) {
  cat("------------------------------------------------------------\n")
  cat("Q: ", q, "\n\n", sep = "")

  rag_args <- list(
    question          = q,
    collection        = collection_name,
    top_k             = as.integer(top_k),
    embedding_model   = embedding_model,
    chat_model        = chat_model,
    temperature       = temperature,
    max_output_tokens = as.integer(max_output_tokens),
    score_threshold   = score_threshold,
    system_prompt     = system_prompt
  )

  res <- do.call(query_rag, rag_args)

  cat("Answer:\n")
  cat(res$answer, "\n\n")

  cat("Retrieved context preview:\n")
  print(utils::head(res$retrieved, as.integer(preview_n)))
  cat("\n")
}

cat("Done.\n")