# scripts/dev/qa/run_eval_pipeline.R
#
# Purpose:
#   End-to-end evaluation pipeline:
#     1) Compute QA metrics from saved QA log -> db/qa_metrics.rds
#     2) Summarize + plot -> reports/ragas/*
#
# Usage (from project root):
#   Rscript scripts/dev/qa/run_eval_pipeline.R

library(ragR)
library(ggplot2)

# ----------------------------- Config ----------------------------------------

USE_LLM_METRICS <- TRUE  # TRUE = LLM-based ("actual"), FALSE = approximations
SEED <- 42L              # set to NULL to disable

qa_log_path     <- "db/qa_log.rds"
qa_metrics_path <- "db/qa_metrics.rds"

output_dir  <- file.path("reports", "ragas")
summary_csv <- file.path(output_dir, "ragas_summary.csv")
plot_png    <- file.path(output_dir, "ragas_means.png")

# Plot styling
R_LOGO_BLUE <- "#276DC3"   # close to R-logo blue
PLOT_W_PX   <- 1200
PLOT_H_PX   <- 700
PLOT_DPI    <- 150

# ----------------------------- Helpers ---------------------------------------

stop_if_missing <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("File not found: %s", path), call. = FALSE)
  }
}

plot_ragas_means_pretty <- function(summary_df, output_path) {
  # Expect columns: metric, mean, sd (from summarize_ragas())
  if (!all(c("metric", "mean", "sd") %in% names(summary_df))) {
    stop("summary_df must contain columns: metric, mean, sd", call. = FALSE)
  }

  df <- summary_df
  df$metric <- as.character(df$metric)

  # Mean ± SD, clamped to [0, 1] since RAGAS scores are in [0, 1]
  df$ymin <- pmax(0, df$mean - df$sd)
  df$ymax <- pmin(1, df$mean + df$sd)

  p <- ggplot(df, aes(x = metric, y = mean)) +
    geom_col(fill = R_LOGO_BLUE, width = 0.7) +
    geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2, linewidth = 0.8, color = "black") +
    coord_cartesian(ylim = c(0, 1)) +
    labs(
      title = "RAGAS Metric Means (± SD)",
      x = "Metric",
      y = "Mean score"
    ) +
    theme_minimal(base_size = 18) +
    theme(
      plot.title = element_text(color = "black", size = 22, face = "bold"),
      axis.title.x = element_text(color = "black", size = 18),
      axis.title.y = element_text(color = "black", size = 18),
      axis.text.x  = element_text(color = "black", size = 14, angle = 30, hjust = 1),
      axis.text.y  = element_text(color = "black", size = 14),
      panel.grid.minor = element_blank()
    )

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

  ggplot2::ggsave(
    filename = output_path,
    plot     = p,
    width    = PLOT_W_PX / PLOT_DPI,
    height   = PLOT_H_PX / PLOT_DPI,
    dpi      = PLOT_DPI,
    units    = "in"
  )
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

# 4) Plot + save PNG (pretty, Mean ± SD)
plot_ragas_means_pretty(
  summary_df  = summary_tbl,
  output_path = plot_png
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
