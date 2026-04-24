# scripts/dev/qa/run_eval_loop.R
#
# Purpose:
#   End-to-end LLM-based RAGAS evaluation pipeline:
#     1) Load QA log (db/qa_log.rds)
#     2) Compute LLM-scored RAGAS metrics for N iterations
#     3) Save per-iteration metrics and summaries
#     4) Save final metrics to db/qa_metrics.rds
#     5) Plot mean ± SD for each iteration
#
# Usage (from project root):
#   Rscript scripts/dev/qa/run_eval_loop.R

library(ragR)

# ----------------------------- Config ----------------------------------------

SEED <- 42L              # set to NULL to disable seed forwarding
N    <- 3L               # number of iterations

qa_log_path     <- "db/qa_log.rds"
qa_metrics_path <- "db/qa_metrics.rds"

output_dir <- file.path("reports", "ragas")

judge_model     <- "gpt-4o-mini"
embedding_model <- "text-embedding-3-small"

answer_relevance_strictness <- 3L

# Plot styling
R_LOGO_BLUE <- "#276DC3"
PLOT_W_PX   <- 1400
PLOT_H_PX   <- 800

# ----------------------------- Helpers ---------------------------------------

stop_if_missing <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("File not found: %s", path), call. = FALSE)
  }
}

assert_has_ground_truth <- function(qa_log) {
  if (!("answer_reference" %in% names(qa_log))) {
    stop(
      "QA log is missing 'answer_reference'. Populate ground truth before running LLM RAGAS.",
      call. = FALSE
    )
  }

  gt <- qa_log$answer_reference
  gt_missing <- is.na(gt) | !nzchar(trimws(as.character(gt)))

  if (any(gt_missing)) {
    bad_ids <- qa_log$qa_id[gt_missing]

    stop(
      sprintf(
        "Ground truth missing/empty for %d row(s). Example qa_id: %s. Populate answer_reference before running LLM RAGAS.",
        sum(gt_missing),
        paste(utils::head(bad_ids, 10), collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

clamp01 <- function(x) {
  pmax(0, pmin(1, x))
}

plot_bar_means_sd <- function(summary_df, output_path, title, bar_col) {
  stopifnot(all(c("metric", "mean", "sd") %in% names(summary_df)))

  df <- summary_df
  df$metric <- as.character(df$metric)

  means <- as.numeric(df$mean)
  sds   <- as.numeric(df$sd)

  if (length(means) == 0L || all(is.na(means))) {
    stop("summary_df$mean contains no valid values to plot.", call. = FALSE)
  }

  ymin <- clamp01(means - sds)
  ymax <- clamp01(means + sds)

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  grDevices::png(filename = output_path, width = PLOT_W_PX, height = PLOT_H_PX, res = 150)

  op <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::par(op)
    grDevices::dev.off()
  }, add = TRUE)

  graphics::par(mar = c(10, 6, 4, 2) + 0.1)

  x <- graphics::barplot(
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

  graphics::arrows(
    x0 = x,
    y0 = ymin,
    x1 = x,
    y1 = ymax,
    angle = 90,
    code = 3,
    length = 0.05,
    lwd = 2,
    col = "black"
  )

  graphics::title(main = title, col.main = "black", font.main = 2)
  graphics::mtext("Metric", side = 1, line = 8, col = "black", cex = 1.3)
  graphics::mtext("Mean score", side = 2, line = 3.5, col = "black", cex = 1.3)
}

# ----------------------------- Run -------------------------------------------

cat("=== ragR LLM Evaluation Pipeline ===\n")
cat("QA log path       :", qa_log_path, "\n")
cat("QA metrics path   :", qa_metrics_path, "\n")
cat("Output dir        :", output_dir, "\n")
cat("Judge model       :", judge_model, "\n")
cat("Embedding model   :", embedding_model, "\n")
cat("Seed              :", if (is.null(SEED)) "(disabled)" else as.character(SEED), "\n")
cat("Iterations        :", N, "\n\n")

# 1) Load QA log
stop_if_missing(qa_log_path)
qa_log <- load_qa_log(qa_log_path)

cat("Loaded QA log:", qa_log_path, "\n")
cat("Rows:", nrow(qa_log), "\n\n")

if (nrow(qa_log) == 0L) {
  stop("qa_log is empty. Collect Q/A interactions first.", call. = FALSE)
}

# Ground truth is required in this evaluation setup.
assert_has_ground_truth(qa_log)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

all_metrics   <- vector("list", N)
all_summaries <- vector("list", N)

for (i in seq_len(N)) {
  cat("============================================================\n")
  cat(sprintf("ITERATION %d\n", i))
  cat("============================================================\n")

  iter_seed <- if (is.null(SEED)) NULL else SEED + i - 1L

  iter_metrics_path <- file.path(output_dir, sprintf("qa_metrics_actual_iter_%02d.rds", i))
  iter_summary_csv  <- file.path(output_dir, sprintf("ragas_summary_actual_iter_%02d.csv", i))
  iter_plot_png     <- file.path(output_dir, sprintf("ragas_means_actual_iter_%02d.png", i))

  cat("Computing LLM-scored RAGAS metrics...\n")
  cat("  iteration seed:", if (is.null(iter_seed)) "(disabled)" else as.character(iter_seed), "\n\n")

  qa_metrics_actual <- compute_ragas_metrics_llm(
    qa_log,
    judge_model = judge_model,
    seed = iter_seed,
    embedding_model = embedding_model,
    answer_relevance_strictness = as.integer(answer_relevance_strictness)
  )

  if (!tibble::is_tibble(qa_metrics_actual)) {
    stop("Metric computation did not return a tibble.", call. = FALSE)
  }

  save_qa_metrics(qa_metrics_actual, iter_metrics_path)

  summary_actual <- summarize_ragas(qa_metrics_actual)
  utils::write.csv(summary_actual, iter_summary_csv, row.names = FALSE)

  plot_bar_means_sd(
    summary_df  = summary_actual,
    output_path = iter_plot_png,
    title       = sprintf("RAGAS Metric Means (LLM-scored, Mean ± SD) - Iteration %d", i),
    bar_col     = R_LOGO_BLUE
  )

  cat("Saved iteration metrics:", iter_metrics_path, "\n")
  cat("Wrote iteration summary:", iter_summary_csv, "\n")
  cat("Wrote iteration plot   :", iter_plot_png, "\n\n")

  cat(sprintf("Summary: Iteration %d\n", i))
  print(summary_actual)
  cat("\n")

  all_metrics[[i]]   <- qa_metrics_actual
  all_summaries[[i]] <- summary_actual
}

# Save final iteration to the standard package path.
save_qa_metrics(qa_metrics_actual, qa_metrics_path)

final_summary_csv <- file.path(output_dir, "ragas_summary_actual.csv")
final_plot_png    <- file.path(output_dir, "ragas_means_actual.png")

utils::write.csv(summary_actual, final_summary_csv, row.names = FALSE)

plot_bar_means_sd(
  summary_df  = summary_actual,
  output_path = final_plot_png,
  title       = "RAGAS Metric Means (LLM-scored, Mean ± SD)",
  bar_col     = R_LOGO_BLUE
)

cat("Final metrics saved to:", qa_metrics_path, "\n")
cat("Final summary saved to:", final_summary_csv, "\n")
cat("Final plot saved to   :", final_plot_png, "\n")
cat("\nDone.\n")

invisible(list(
  seed = SEED,
  N = N,
  qa_log = qa_log,
  final_qa_metrics = qa_metrics_actual,
  all_metrics = all_metrics,
  all_summaries = all_summaries,
  paths = list(
    qa_log_path = qa_log_path,
    qa_metrics_path = qa_metrics_path,
    output_dir = output_dir,
    final_summary_csv = final_summary_csv,
    final_plot_png = final_plot_png
  )
))