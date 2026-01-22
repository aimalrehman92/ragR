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
library(ggplot2)

# ----------------------------- Config ----------------------------------------

qa_metrics_path <- "db/qa_metrics.rds"
output_dir      <- file.path("reports", "ragas")
summary_csv     <- file.path(output_dir, "ragas_summary.csv")
plot_png        <- file.path(output_dir, "ragas_means.png")

# Plot styling
R_LOGO_BLUE <- "#276DC3"   # close to R-logo blue
PLOT_W_PX   <- 1200
PLOT_H_PX   <- 700
PLOT_DPI    <- 150

# ----------------------------- Helpers ---------------------------------------

plot_ragas_means_pretty <- function(summary_df, output_path) {
  if (!all(c("metric", "mean", "sd") %in% names(summary_df))) {
    stop("summary_df must contain columns: metric, mean, sd", call. = FALSE)
  }

  df <- summary_df
  df$metric <- as.character(df$metric)

  # Mean ± SD, clamped to [0, 1]
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

qa_metrics <- load_qa_metrics(qa_metrics_path)

cat("Loaded QA metrics:", qa_metrics_path, "\n")
cat("Rows:", nrow(qa_metrics), "\n\n")

if (nrow(qa_metrics) == 0L) {
  stop(
    "qa_metrics is empty. Run scripts/dev/qa/run_eval_pipeline.R first (and ensure QA log has rows).",
    call. = FALSE
  )
}

# Summarize metrics (mean/sd/min/max per metric)
summary_tbl <- summarize_ragas(qa_metrics)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Save summary CSV
utils::write.csv(summary_tbl, summary_csv, row.names = FALSE)

# Save plot (pretty, Mean ± SD)
plot_ragas_means_pretty(
  summary_df  = summary_tbl,
  output_path = plot_png
)

cat("✔ Wrote summary CSV:", summary_csv, "\n")
cat("✔ Wrote plot PNG    :", plot_png, "\n\n")

cat("Summary preview:\n")
print(summary_tbl)

invisible(list(summary = summary_tbl, csv = summary_csv, plot = plot_png))
