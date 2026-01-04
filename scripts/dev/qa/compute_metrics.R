# scripts/dev/qa/compute_metrics.R
#
# Purpose:
#   Compute RAGAS metrics from the saved QA log and persist QA metrics to disk.
#   Supports both:
#     - "Actual" (LLM-based) metrics: compute_ragas_metrics_llm()
#     - Approximate (proxy) metrics: compute_ragas_metrics_approx()
#
# Usage (from project root):
#   Rscript scripts/dev/qa/compute_metrics.R

library(ragR)

# ----------------------------- Config ----------------------------------------

qa_log_path     <- "db/qa_log.rds"
qa_metrics_path <- "db/qa_metrics.rds"

# Choose which metric implementation to use:
#   TRUE  -> LLM-based "actual" metrics (requires OPENAI_API_KEY etc.)
#   FALSE -> deterministic lexical approximations (no LLM calls)
USE_LLM_METRICS <- FALSE

# ----------------------------- Run -------------------------------------------

qa_log <- load_qa_log(qa_log_path)

cat("Loaded QA log:", qa_log_path, "\n")
cat("Rows:", nrow(qa_log), "\n\n")

if (nrow(qa_log) == 0L) {
  cat("QA log is empty -> saving empty QA metrics.\n")
  qa_metrics <- qa_metrics_empty()
  save_qa_metrics(qa_metrics, qa_metrics_path)

  cat("✔ Saved empty qa_metrics to:", qa_metrics_path, "\n")
  quit(save = "no", status = 0)
}

cat("Computing RAGAS metrics using:",
    if (USE_LLM_METRICS) "LLM (actual)" else "Approx (proxy)",
    "implementation...\n\n"
)

qa_metrics <- if (USE_LLM_METRICS) {
  compute_ragas_metrics_llm(qa_log)
} else {
  compute_ragas_metrics_approx(qa_log)
}

# Basic sanity checks
required_cols <- c(
  "qa_id", "context_precision", "context_recall",
  "answer_relevance", "faithfulness", "ragas_overall"
)
missing_cols <- setdiff(required_cols, names(qa_metrics))
if (length(missing_cols) > 0L) {
  stop(
    "Computed qa_metrics is missing required columns: ",
    paste(missing_cols, collapse = ", "),
    call. = FALSE
  )
}

save_qa_metrics(qa_metrics, qa_metrics_path)

cat("✔ Saved qa_metrics to:", qa_metrics_path, "\n")
cat("Rows:", nrow(qa_metrics), "\n\n")

cat("Preview (first 6 rows):\n")
print(utils::head(qa_metrics))

invisible(qa_metrics)
