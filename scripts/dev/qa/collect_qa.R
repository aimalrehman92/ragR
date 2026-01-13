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

# Retrieval + model settings (exposed)
top_k           <- 5L
score_threshold <- 0           # keep chunks with score >= this (0 disables filtering)

embedding_model <- "text-embedding-3-small"
chat_model      <- "gpt-4o-mini"

# Generation settings (exposed)
temperature       <- 0.5
max_output_tokens <- 300L

# User-defined system prompt (exposed)
system_prompt <- "You are a course assistant and you answer in a rude manner."

# Add as many questions as you want to log in one run:
questions <- c(
  "What is the attendance policy for STAT 8670?",
  "What is the attendance policy for STAT 8581?",
  "Who is the professor for STAT 8670?",
  "Is it the final exam or project for STAT 8670?"
)

# ----------------------------- Run -------------------------------------------

# 1) Load existing QA log (or create empty if missing)
qa_log <- load_qa_log(qa_log_path)

cat("Loaded QA log with", nrow(qa_log), "rows from:", qa_log_path, "\n")
cat("Settings:\n")
cat("  Collection        :", collection_name, "\n")
cat("  top_k             :", top_k, "\n")
cat("  score_threshold   :", score_threshold, "\n")
cat("  embedding_model   :", embedding_model, "\n")
cat("  chat_model        :", chat_model, "\n")
cat("  temperature       :", temperature, "\n")
cat("  max_output_tokens :", max_output_tokens, "\n")
cat("  system_prompt     :", system_prompt, "\n\n")

# 2) Ask + log each question
for (q in questions) {
  cat("Q:", q, "\n")

  # Build args explicitly so it’s obvious what’s being used
  rag_args <- list(
    question         = q,
    collection       = collection_name,
    top_k            = as.integer(top_k),
    embedding_model  = embedding_model,
    chat_model       = chat_model,
    temperature      = temperature,
    max_output_tokens = as.integer(max_output_tokens),
    score_threshold  = score_threshold,
    system_prompt    = system_prompt
  )

  res <- do.call(query_rag, rag_args)

  # Log interaction (schema is owned by qa_logging.R; we keep it aligned by
  # passing what the logger currently accepts).
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
