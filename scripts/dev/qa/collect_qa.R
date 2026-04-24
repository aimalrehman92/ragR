# scripts/dev/qa/collect_qa.R
#
# Purpose:
#   Collect Q/A interactions using ragR (library mode) and append them to qa_log.rds,
#   including the final grounded RAG prompt sent as the user/content prompt.
#
# Usage (from project root):
#   Rscript scripts/dev/qa/collect_qa.R
#
# Notes:
#   - Make sure you've already ingested docs into `collection_name`.
#   - This script appends new rows to an existing QA log on disk.
#   - `system_prompt` is passed as an API-level system message to the chat model;
#     it is not inserted into the constructed RAG prompt.

library(ragR)

# ----------------------------- Config ----------------------------------------

qa_log_path     <- "db/qa_log.rds"
collection_name <- "default"  # <-- CHANGE THIS to match your ingestion collection

# Retrieval + model settings
top_k           <- 5L
score_threshold <- 0           # keep chunks with score >= this; 0 disables filtering

embedding_model <- "text-embedding-3-small"
chat_model      <- "gpt-4o-mini"

# Generation settings
temperature       <- 0
max_output_tokens <- 10000L

# User-defined API-level system prompt
system_prompt <- "You are a helpful assistant. You answer fully and truthfully."

# Add as many questions as you want to log in one run:
questions <- c(
  "What is anatomy?",
  "What is microscopic anatomy also called?",
  "What are the two main approaches to studying anatomy?",
  "What is the anatomical position?",
  "What do coronal planes divide the body into?",
  "What is the difference between proximal and distal?",
  "Why does bone appear white on an X-ray?",
  "What is the main difference between X-rays and gamma rays?",
  "What are the two main subdivisions of the skeleton?",
  "What are the three types of cartilage?",
  "What do sagittal planes divide the body into?",
  "What is the median sagittal plane?",
  "What do transverse planes divide the body into?",
  "What do anterior and posterior describe in anatomy?",
  "What do medial and lateral describe in anatomy?",
  "What do superficial and deep describe in anatomy?",
  "What is ultrasound?",
  "What does Doppler ultrasound measure?",
  "In which plane are CT images typically obtained?",
  "What is the main tissue property used in MRI to generate images?",
  "What is the most commonly used radionuclide in nuclear medicine?",
  "What is the most commonly used PET radionuclide mentioned in the document?",
  "What are the two main categories of joints?",
  "What separates the skeletal elements in a synovial joint?",
  "What does the synovial membrane produce?",
  "What type of movement do hinge joints permit?",
  "What type of movement do pivot joints permit?",
  "What are the two layers of the skin?",
  "What are the two general categories of fascia?",
  "What are the three types of muscle tissue found in the body?"
)

# ----------------------------- Run -------------------------------------------

# 1) Load existing QA log, or create empty log if missing
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
cat("  system_prompt     :", system_prompt, "\n")
cat("  questions         :", length(questions), "\n\n")

# 2) Ask and log each question
for (q in questions) {
  cat("Q:", q, "\n")

  rag_args <- list(
    question          = q,
    collection        = collection_name,
    top_k             = as.integer(top_k),
    embedding_model   = embedding_model,
    chat_model        = chat_model,
    temperature       = temperature,
    max_output_tokens = if (is.null(max_output_tokens)) NULL else as.integer(max_output_tokens),
    score_threshold   = score_threshold,
    system_prompt     = system_prompt
  )

  res <- do.call(query_rag, rag_args)

  # Logger stores rag_result$prompt in qa_log as `prompt_final`.
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

cat("QA logged successfully. QA log now contains", nrow(qa_log), "rows.\n")
cat("Saved to:", qa_log_path, "\n")