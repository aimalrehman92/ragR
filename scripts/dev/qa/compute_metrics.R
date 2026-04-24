# scripts/dev/qa/compute_metrics.R
#
# Purpose:
#   Compute LLM-scored RAGAS metrics from the saved QA log and persist
#   QA metrics to disk.
#
# Usage (from project root):
#   Rscript scripts/dev/qa/compute_metrics.R

library(ragR)

# ----------------------------- Config ----------------------------------------

qa_log_path     <- "db/qa_log.rds"
qa_metrics_path <- "db/qa_metrics.rds"

judge_model     <- "gpt-4o-mini"
embedding_model <- "text-embedding-3-small"

answer_relevance_strictness <- 3L

# ----------------------------- Run -------------------------------------------

qa_log <- load_qa_log(qa_log_path)

cat("Loaded QA log:", qa_log_path, "\n")
cat("Rows:", nrow(qa_log), "\n\n")

if (nrow(qa_log) == 0L) {
  cat("QA log is empty; saving empty QA metrics.\n")

  qa_metrics <- qa_metrics_empty()
  save_qa_metrics(qa_metrics, qa_metrics_path)

  cat("Saved empty qa_metrics to:", qa_metrics_path, "\n")
  quit(save = "no", status = 0)
}

cat("Computing LLM-scored RAGAS metrics...\n")
cat("  judge_model                 :", judge_model, "\n")
cat("  embedding_model             :", embedding_model, "\n")
cat("  answer_relevance_strictness :", answer_relevance_strictness, "\n\n")

qa_metrics <- compute_ragas_metrics_llm(
  qa_log,
  judge_model = judge_model,
  embedding_model = embedding_model,
  answer_relevance_strictness = as.integer(answer_relevance_strictness)
)

# -------------------------- Sanity checks ------------------------------------

required_cols <- c(
  "qa_id",
  "context_precision",
  "context_recall",
  "answer_relevance",
  "faithfulness",
  "ragas_overall"
)

missing_cols <- setdiff(required_cols, names(qa_metrics))

if (length(missing_cols) > 0L) {
  stop(
    "Computed qa_metrics is missing required columns: ",
    paste(missing_cols, collapse = ", "),
    call. = FALSE
  )
}

# ----------------------------- Save ------------------------------------------

save_qa_metrics(qa_metrics, qa_metrics_path)

cat("Saved qa_metrics to:", qa_metrics_path, "\n")
cat("Rows:", nrow(qa_metrics), "\n\n")

# ----------------------------- Output ----------------------------------------

cat("Preview:\n")
print(utils::head(qa_metrics))

cat("\nMean RAGAS metrics:\n")

metric_cols <- c(
  "context_precision",
  "context_recall",
  "answer_relevance",
  "faithfulness",
  "ragas_overall"
)

metric_means <- colMeans(
  qa_metrics[, metric_cols, drop = FALSE],
  na.rm = TRUE
)

print(round(metric_means, 4))

invisible(qa_metrics)