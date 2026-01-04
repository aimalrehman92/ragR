# R/ragas_report.R

#' Generate a RAGAS performance report (mode-aware)
#'
#' Loads a QA log from disk, computes metrics (approx or llm), writes:
#' - metrics RDS (qa_metrics_path)
#' - summary CSV (ragas_summary.csv)
#' - means bar plot PNG (ragas_means.png)
#'
#' @param qa_log_path Path to QA log RDS (default: "db/qa_log.rds").
#' @param qa_metrics_path Path to metrics RDS output (default: "db/qa_metrics.rds").
#' @param output_dir Output directory for CSV/PNG (default: "reports/ragas").
#' @param mode `"approx"` or `"llm"`. If NULL, uses option `ragR.ragas_mode`.
#' @param judge_model Judge model used when `mode="llm"` (default: "gpt-4o-mini").
#'
#' @return A list with: status, n_qa, qa_metrics_path, summary_csv_path, plot_path.
#' @export
generate_ragas_report <- function(
  qa_log_path     = "db/qa_log.rds",
  qa_metrics_path = "db/qa_metrics.rds",
  output_dir      = "reports/ragas",
  mode            = NULL,
  judge_model     = "gpt-4o-mini"
) {
  qa_log <- load_qa_log(qa_log_path)

  metrics <- compute_ragas_metrics(qa_log, mode = mode, judge_model = judge_model)
  summary <- summarize_ragas(metrics)

  # save metrics
  dir.create(dirname(qa_metrics_path), showWarnings = FALSE, recursive = TRUE)
  saveRDS(metrics, qa_metrics_path)

  # write summary CSV
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  summary_csv_path <- file.path(output_dir, "ragas_summary.csv")
  utils::write.csv(summary, summary_csv_path, row.names = FALSE)

  # plot means
  plot_path <- file.path(output_dir, "ragas_means.png")
  plot_ragas_means(summary, plot_path)

  list(
    status           = "ok",
    n_qa             = nrow(qa_log),
    qa_metrics_path  = qa_metrics_path,
    summary_csv_path = summary_csv_path,
    plot_path        = plot_path
  )
}

#' Generate RAGAS report using approximations
#' @inheritParams generate_ragas_report
#' @export
generate_ragas_report_approx <- function(
  qa_log_path     = "db/qa_log.rds",
  qa_metrics_path = "db/qa_metrics.rds",
  output_dir      = "reports/ragas"
) {
  generate_ragas_report(
    qa_log_path     = qa_log_path,
    qa_metrics_path = qa_metrics_path,
    output_dir      = output_dir,
    mode            = "approx"
  )
}

#' Generate RAGAS report using LLM scoring
#' @inheritParams generate_ragas_report
#' @export
generate_ragas_report_llm <- function(
  qa_log_path     = "db/qa_log.rds",
  qa_metrics_path = "db/qa_metrics.rds",
  output_dir      = "reports/ragas",
  judge_model     = "gpt-4o-mini"
) {
  generate_ragas_report(
    qa_log_path     = qa_log_path,
    qa_metrics_path = qa_metrics_path,
    output_dir      = output_dir,
    mode            = "llm",
    judge_model     = judge_model
  )
}
