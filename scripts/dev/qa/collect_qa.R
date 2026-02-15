# scripts/dev/qa/collect_qa.R
#
# Purpose:
#   Collect Q/A interactions using ragR (library mode) and append them to qa_log.rds,
#   including the final prompt that was sent to the chat model.
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
system_prompt <- "You are a helpful assistant."

# Add as many questions as you want to log in one run:
questions <- c(
"What is the difference between gross anatomy and microscopic anatomy?",
"What are the two main approaches used to study gross anatomy and how do they differ?",
"What defines the anatomical position of the human body?",
"Why does bone appear white and air appear dark on an X-ray image?",
"Do we get Groundhog Day off?",
"Where is my nearest Apple store?"
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

  # Logger now also stores rag_result$prompt into qa_log as `prompt_final`.
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
