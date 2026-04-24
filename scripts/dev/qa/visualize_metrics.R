# scripts/dev/qa/visualize_metrics.R
#
# Purpose:
#   Read saved QA metrics, compute a summary table, and write:
#     - CSV summary to reports/ragas/ragas_summary.csv
#     - PNG bar chart to reports/ragas/ragas_means.png
#
# Usage (from project root):
#   Rscript scripts/dev/qa/visualize_metrics.R

library(ragR)

# ----------------------------- Config ----------------------------------------

qa_metrics_path <- "db/qa_metrics.rds"
output_dir      <- file.path("reports", "ragas")
summary_csv     <- file.path(output_dir, "ragas_summary.csv")
plot_png        <- file.path(output_dir, "ragas_means.png")

# ----------------------------- Run -------------------------------------------

qa_metrics <- load_qa_metrics(qa_metrics_path)

cat("Loaded QA metrics:", qa_metrics_path, "\n")
cat("Rows:", nrow(qa_metrics), "\n\n")

if (nrow(qa_metrics) == 0L) {
  stop(
    "qa_metrics is empty. Run scripts/dev/qa/run_eval_pipeline.R first and ensure QA log has rows.",
    call. = FALSE
  )
}

summary_tbl <- summarize_ragas(qa_metrics)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

utils::write.csv(summary_tbl, summary_csv, row.names = FALSE)

plot_ragas_means(
  summary_df  = summary_tbl,
  output_path = plot_png
)

cat("Wrote summary CSV:", summary_csv, "\n")
cat("Wrote plot PNG    :", plot_png, "\n\n")

cat("Summary:\n")
print(summary_tbl)

invisible(list(
  summary = summary_tbl,
  csv = summary_csv,
  plot = plot_png
))