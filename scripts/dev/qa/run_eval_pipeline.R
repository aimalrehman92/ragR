# scripts/dev/qa/run_eval_pipeline.R
#
# Purpose:
#   End-to-end evaluation pipeline:
#     1) Compute QA metrics from saved QA log -> db/qa_metrics.rds
#     2) Summarize + plot -> reports/ragas/*
#
# Usage (from project root):
#   Rscript scripts/dev/qa/run_eval_pipeline.R
#
# Notes:
#   - Set USE_LLM_METRICS = TRUE to compute "actual" (LLM-based) metrics.
#   - Set USE_LLM_METRICS = FALSE to compute lexical approximations.
#   - Set SEED to an integer for more reproducible LLM-judge scoring (if supported by model).

library(ragR)

# ----------------------------- Config ----------------------------------------

# Choose which metric implementation to use:
USE_LLM_METRICS <- TRUE # TRUE = LLM-based ("actual"), FALSE = approximations

# Optional: seed for judge model calls (set NULL to disable)
SEED <- 42L  # e.g., 42L; set to NULL to disable

qa_log_path     <- "db/qa_log.rds"
qa_metrics_path <- "db/qa_metrics.rds"

output_dir  <- file.path("reports", "ragas")
summary_csv <- file.path(output_dir, "ragas_summary.csv")
plot_png    <- file.path(output_dir, "ragas_means.png")

# ----------------------------- Helpers ---------------------------------------

stop_if_missing <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("File not found: %s", path), call. = FALSE)
  }
}

# ----------------------------- Run -------------------------------------------

cat("=== ragR Evaluation Pipeline ===\n")
cat("QA log path     :", qa_log_path, "\n")
cat("QA metrics path :", qa_metrics_path, "\n")
cat("Output dir      :", output_dir, "\n")
cat("Mode            :", if (USE_LLM_METRICS) "LLM (actual) metrics" else "Approx (proxy) metrics", "\n")
cat("Seed            :", if (USE_LLM_METRICS) as.character(SEED) else "(n/a)", "\n\n")

# 1) Load QA log
stop_if_missing(qa_log_path)
qa_log <- load_qa_log(qa_log_path)

cat("Loaded QA log:", qa_log_path, "\n")
cat("Rows:", nrow(qa_log), "\n\n")

if (nrow(qa_log) == 0L) {
  stop("qa_log is empty. Collect Q/A interactions first.", call. = FALSE)
}

# Enforce ground truth presence when running canonical LLM metrics
if (USE_LLM_METRICS) {
  if (!("answer_reference" %in% names(qa_log))) {
    stop("QA log is missing 'answer_reference'. Populate ground truth before running LLM RAGAS.", call. = FALSE)
  }

  gt <- qa_log$answer_reference
  gt_missing <- is.na(gt) | !nzchar(trimws(as.character(gt)))

  if (any(gt_missing)) {
    bad_ids <- qa_log$qa_id[gt_missing]
    stop(
      sprintf(
        "Ground truth missing/empty for %d row(s). Example qa_id: %s. Populate answer_reference before running canonical RAGAS.",
        sum(gt_missing),
        paste(utils::head(bad_ids, 10), collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

# 2) Compute metrics
cat("Computing metrics...\n")

qa_metrics <- if (USE_LLM_METRICS) {
  compute_ragas_metrics_llm(qa_log, seed = SEED)
} else {
  compute_ragas_metrics_approx(qa_log)
}

if (!tibble::is_tibble(qa_metrics)) {
  stop("Metric computation did not return a tibble.", call. = FALSE)
}

save_qa_metrics(qa_metrics, qa_metrics_path)

cat("✔ Saved QA metrics:", qa_metrics_path, "\n")
cat("Rows:", nrow(qa_metrics), "\n\n")

# 3) Summarize + save CSV
summary_tbl <- summarize_ragas(qa_metrics)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(summary_tbl, summary_csv, row.names = FALSE)
cat("✔ Wrote summary CSV:", summary_csv, "\n")

# 4) Plot + save PNG
plot_ragas_means(
  summary_df  = summary_tbl,
  output_path = plot_png,
  width       = 1200,
  height      = 700
)
cat("✔ Wrote plot PNG    :", plot_png, "\n\n")

cat("Summary preview:\n")
print(summary_tbl)

cat("\nDone.\n")

invisible(list(
  mode       = if (USE_LLM_METRICS) "llm" else "approx",
  seed       = if (USE_LLM_METRICS) SEED else NULL,
  qa_log     = qa_log,
  qa_metrics = qa_metrics,
  summary    = summary_tbl,
  paths      = list(
    qa_log_path     = qa_log_path,
    qa_metrics_path = qa_metrics_path,
    summary_csv     = summary_csv,
    plot_png        = plot_png
  )
))
