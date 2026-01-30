# scripts/dev/qa/run_eval_pipeline.R
#
# Purpose:
#   End-to-end evaluation pipeline (now with plots):
#     1) Load QA log (db/qa_log.rds)
#     2) Compute ACTUAL (LLM) metrics -> db/qa_metrics.rds
#     3) Compute APPROX (proxy) metrics -> db/qa_metrics_approx.rds
#     4) Summarize + write CSVs
#     5) Plot:
#        - Bar graph (Mean ± SD) for ACTUAL only
#        - Grouped bar graph (Mean ± SD) comparing ACTUAL vs APPROX
#
# Usage (from project root):
#   Rscript scripts/dev/qa/run_eval_pipeline.R

library(ragR)

# ----------------------------- Config ----------------------------------------

SEED <- 42L              # set to NULL to disable (for ACTUAL/LLM metrics)

qa_log_path           <- "db/qa_log.rds"
qa_metrics_path       <- "db/qa_metrics.rds"          # ACTUAL
qa_metrics_approx_path <- "db/qa_metrics_approx.rds"  # APPROX

output_dir            <- file.path("reports", "ragas")
summary_actual_csv    <- file.path(output_dir, "ragas_summary_actual.csv")
summary_approx_csv    <- file.path(output_dir, "ragas_summary_approx.csv")
summary_joined_csv    <- file.path(output_dir, "ragas_summary_actual_vs_approx.csv")

plot_actual_png       <- file.path(output_dir, "ragas_means_actual.png")
plot_compare_png      <- file.path(output_dir, "ragas_means_actual_vs_approx.png")

# Plot styling (match your previous “pretty” settings, but BASE R; no ggplot2 dependency)
R_LOGO_BLUE <- "#276DC3"   # close to R-logo blue
APPROX_GRAY <- "#9E9E9E"
PLOT_W_PX   <- 1400
PLOT_H_PX   <- 800

# ----------------------------- Helpers ---------------------------------------

stop_if_missing <- function(path) {
  if (!file.exists(path)) stop(sprintf("File not found: %s", path), call. = FALSE)
}

assert_has_ground_truth <- function(qa_log) {
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

clamp01 <- function(x) pmax(0, pmin(1, x))

# Base R plot: single series (Mean ± SD)
plot_bar_means_sd <- function(summary_df, output_path, title, bar_col) {
  stopifnot(all(c("metric", "mean", "sd") %in% names(summary_df)))

  df <- summary_df
  df$metric <- as.character(df$metric)

  means <- as.numeric(df$mean)
  sds   <- as.numeric(df$sd)

  ymin <- clamp01(means - sds)
  ymax <- clamp01(means + sds)

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  grDevices::png(filename = output_path, width = PLOT_W_PX, height = PLOT_H_PX, res = 150)

  op <- par(no.readonly = TRUE)
  on.exit({ par(op); dev.off() }, add = TRUE)

  par(mar = c(10, 6, 4, 2) + 0.1)  # bottom, left, top, right

  x <- barplot(
    height    = means,
    names.arg = df$metric,
    col       = bar_col,
    border    = NA,
    ylim      = c(0, 1),
    las       = 2,
    cex.names = 1.2,
    cex.axis  = 1.2,
    cex.lab   = 1.3,
    cex.main  = 1.4
  )

  # error bars (Mean ± SD)
  arrows(
    x0 = x, y0 = ymin,
    x1 = x, y1 = ymax,
    angle = 90, code = 3, length = 0.05,
    lwd = 2, col = "black"
  )

  title(main = title, col.main = "black", font.main = 2)
  mtext("Metric", side = 1, line = 8, col = "black", cex = 1.3)
  mtext("Mean score", side = 2, line = 3.5, col = "black", cex = 1.3)
}

# Base R plot: grouped series (ACTUAL vs APPROX), Mean ± SD
plot_grouped_means_sd <- function(actual_df, approx_df, output_path, title) {
  stopifnot(all(c("metric", "mean", "sd") %in% names(actual_df)))
  stopifnot(all(c("metric", "mean", "sd") %in% names(approx_df)))

  a <- actual_df
  p <- approx_df
  a$metric <- as.character(a$metric)
  p$metric <- as.character(p$metric)

  # Align rows by metric name (keep ACTUAL order)
  p <- p[match(a$metric, p$metric), , drop = FALSE]

  means_a <- as.numeric(a$mean)
  sds_a   <- as.numeric(a$sd)
  means_p <- as.numeric(p$mean)
  sds_p   <- as.numeric(p$sd)

  ymin_a <- clamp01(means_a - sds_a); ymax_a <- clamp01(means_a + sds_a)
  ymin_p <- clamp01(means_p - sds_p); ymax_p <- clamp01(means_p + sds_p)

  mat_means <- rbind(ACTUAL = means_a, APPROX = means_p)

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  grDevices::png(filename = output_path, width = PLOT_W_PX, height = PLOT_H_PX, res = 150)

  op <- par(no.readonly = TRUE)
  on.exit({ par(op); dev.off() }, add = TRUE)

  par(mar = c(10, 6, 4, 6) + 0.1)  # extra right margin for legend

  x <- barplot(
    height    = mat_means,
    beside    = TRUE,
    names.arg = a$metric,
    col       = c(R_LOGO_BLUE, APPROX_GRAY),
    border    = NA,
    ylim      = c(0, 1),
    las       = 2,
    cex.names = 1.2,
    cex.axis  = 1.2,
    cex.lab   = 1.3,
    cex.main  = 1.4
  )

  # x is a matrix: rows = groups (2), cols = metrics
  arrows(
    x0 = x[1, ], y0 = ymin_a,
    x1 = x[1, ], y1 = ymax_a,
    angle = 90, code = 3, length = 0.05,
    lwd = 2, col = "black"
  )
  arrows(
    x0 = x[2, ], y0 = ymin_p,
    x1 = x[2, ], y1 = ymax_p,
    angle = 90, code = 3, length = 0.05,
    lwd = 2, col = "black"
  )

  title(main = title, col.main = "black", font.main = 2)
  mtext("Metric", side = 1, line = 8, col = "black", cex = 1.3)
  mtext("Mean score", side = 2, line = 3.5, col = "black", cex = 1.3)

  legend(
    "topright",
    inset = c(-0.18, 0),
    legend = c("ACTUAL (LLM)", "APPROX (proxy)"),
    fill   = c(R_LOGO_BLUE, APPROX_GRAY),
    border = NA,
    bty    = "n",
    cex    = 1.1
  )
}

# ----------------------------- Run -------------------------------------------

cat("=== ragR Evaluation Pipeline ===\n")
cat("QA log path            :", qa_log_path, "\n")
cat("QA metrics path (ACTUAL):", qa_metrics_path, "\n")
cat("QA metrics path (APPROX):", qa_metrics_approx_path, "\n")
cat("Output dir             :", output_dir, "\n")
cat("Seed (ACTUAL/LLM)      :", if (is.null(SEED)) "(disabled)" else as.character(SEED), "\n\n")

# 1) Load QA log
stop_if_missing(qa_log_path)
qa_log <- load_qa_log(qa_log_path)

cat("Loaded QA log:", qa_log_path, "\n")
cat("Rows:", nrow(qa_log), "\n\n")

if (nrow(qa_log) == 0L) stop("qa_log is empty. Collect Q/A interactions first.", call. = FALSE)

# Ground-truth required for ACTUAL (LLM) pipeline in your setup
assert_has_ground_truth(qa_log)

# 2) Compute ACTUAL metrics
cat("Computing ACTUAL (LLM) metrics...\n")
qa_metrics_actual <- compute_ragas_metrics_llm(qa_log, seed = SEED)
if (!tibble::is_tibble(qa_metrics_actual)) stop("ACTUAL metric computation did not return a tibble.", call. = FALSE)
save_qa_metrics(qa_metrics_actual, qa_metrics_path)
cat("✔ Saved ACTUAL QA metrics:", qa_metrics_path, "\n")
cat("Rows:", nrow(qa_metrics_actual), "\n\n")

# 3) Compute APPROX metrics
cat("Computing APPROX (proxy) metrics...\n")
qa_metrics_approx <- compute_ragas_metrics_approx(qa_log)
if (!tibble::is_tibble(qa_metrics_approx)) stop("APPROX metric computation did not return a tibble.", call. = FALSE)
save_qa_metrics(qa_metrics_approx, qa_metrics_approx_path)
cat("✔ Saved APPROX QA metrics:", qa_metrics_approx_path, "\n")
cat("Rows:", nrow(qa_metrics_approx), "\n\n")

# 4) Summarize + save CSVs
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

summary_actual <- summarize_ragas(qa_metrics_actual)
summary_approx <- summarize_ragas(qa_metrics_approx)

utils::write.csv(summary_actual, summary_actual_csv, row.names = FALSE)
utils::write.csv(summary_approx, summary_approx_csv, row.names = FALSE)

# Joined summary for paper/report convenience
joined <- merge(
  summary_actual[, c("metric", "mean", "sd")],
  summary_approx[, c("metric", "mean", "sd")],
  by = "metric",
  suffixes = c("_actual", "_approx"),
  all = TRUE
)

# Keep a stable metric order (actual summary order)
joined <- joined[match(summary_actual$metric, joined$metric), , drop = FALSE]
utils::write.csv(joined, summary_joined_csv, row.names = FALSE)

cat("✔ Wrote summary CSV (ACTUAL):", summary_actual_csv, "\n")
cat("✔ Wrote summary CSV (APPROX):", summary_approx_csv, "\n")
cat("✔ Wrote joined summary CSV   :", summary_joined_csv, "\n\n")

# 5) Plots
plot_bar_means_sd(
  summary_df   = summary_actual,
  output_path  = plot_actual_png,
  title        = "RAGAS Metric Means (ACTUAL, Mean ± SD)",
  bar_col      = R_LOGO_BLUE
)
cat("✔ Wrote ACTUAL plot PNG      :", plot_actual_png, "\n")

plot_grouped_means_sd(
  actual_df   = summary_actual,
  approx_df   = summary_approx,
  output_path = plot_compare_png,
  title       = "RAGAS Metric Means: ACTUAL vs APPROX (Mean ± SD)"
)
cat("✔ Wrote comparison plot PNG  :", plot_compare_png, "\n\n")

cat("ACTUAL summary:\n")
print(summary_actual)

cat("\nAPPROX summary:\n")
print(summary_approx)

cat("\nDone.\n")

invisible(list(
  seed            = SEED,
  qa_log          = qa_log,
  qa_metrics_actual = qa_metrics_actual,
  qa_metrics_approx  = qa_metrics_approx,
  summary_actual  = summary_actual,
  summary_approx  = summary_approx,
  paths = list(
    qa_log_path            = qa_log_path,
    qa_metrics_path        = qa_metrics_path,
    qa_metrics_approx_path = qa_metrics_approx_path,
    summary_actual_csv     = summary_actual_csv,
    summary_approx_csv     = summary_approx_csv,
    summary_joined_csv     = summary_joined_csv,
    plot_actual_png        = plot_actual_png,
    plot_compare_png       = plot_compare_png
  )
))
