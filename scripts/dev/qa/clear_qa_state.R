# scripts/dev/qa/clear_qa_state.R
#
# Purpose:
#   Demonstrate how to clear the QA log and QA metrics from disk,
#   effectively resetting the RAGAS evaluation state.
#
# Usage (from project root):
#   Rscript scripts/dev/qa/clear_qa_state.R

library(ragR)

qa_log_path     <- "db/qa_log.rds"
qa_metrics_path <- "db/qa_metrics.rds"

cat("Clearing QA log and QA metrics...\n")

clear_qa_log(qa_log_path)
clear_qa_metrics(qa_metrics_path)

cat("Done.\n")

# Optional: verify
qa_log     <- load_qa_log(qa_log_path)
qa_metrics <- load_qa_metrics(qa_metrics_path)

cat("QA log rows after clear:     ", nrow(qa_log), "\n")
cat("QA metrics rows after clear: ", nrow(qa_metrics), "\n")
