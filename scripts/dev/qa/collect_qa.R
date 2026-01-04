# scripts/dev/qa/collect_qa.R
#
# Purpose:
#   Collect Q/A interactions using ragR (library mode) and append them to qa_log.rds.
#
# Usage (from project root):
#   Rscript scripts/dev/qa/collect_qa.R
#
# Notes:
#   - Make sure you've already ingested docs into `collection_name`.
#   - This script appends new rows to an existing QA log on disk.

library(ragR)

# ----------------------------- Config ----------------------------------------

qa_log_path     <- "db/qa_log.rds"
collection_name <- "default"  # <-- CHANGE THIS to match your ingestion collection
top_k           <- 2L
embedding_model <- "text-embedding-3-small"
chat_model      <- "gpt-4o-mini"

# Add as many questions as you want to log in one run:
questions <- c(
  "What is the topic of this essay?"
  # "Summarize the essay in two sentences.",
  # "What are the key points mentioned?"
)

# ----------------------------- Run -------------------------------------------

# 1) Load existing QA log (or create empty if missing)
qa_log <- load_qa_log(qa_log_path)

cat("Loaded QA log with", nrow(qa_log), "rows from:", qa_log_path, "\n")
cat("Collection:", collection_name, "\n\n")

# 2) Ask + log each question
for (q in questions) {
  cat("Q:", q, "\n")

  res <- query_rag(
    question        = q,
    collection      = collection_name,
    top_k           = top_k,
    embedding_model = embedding_model,
    chat_model      = chat_model
  )

  qa_log <- log_rag_interaction(
    qa_log          = qa_log,
    question        = q,
    rag_result      = res,
    collection      = collection_name,
    chat_model      = chat_model,
    embedding_model = embedding_model
  )

  cat("A:", res$answer, "\n")
  cat("----\n")
}

# 3) Save updated log once
save_qa_log(qa_log, qa_log_path)

cat("✔ QA logged successfully. QA log now contains", nrow(qa_log), "rows.\n")
cat("Saved to:", qa_log_path, "\n")
