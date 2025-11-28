#' Generate a RAGAS-style performance report
#'
#' This function loads the QA log from disk, computes RAGAS-style metrics,
#' and writes a summary table and a bar chart image to the local file
#' system. It is intended to support external front-ends that want to
#' display a performance report without recomputing everything in R.
#'
#' @param qa_log_path Character scalar; path to the QA log RDS file.
#'   Defaults to `"db/qa_log.rds"`.
#' @param qa_metrics_path Character scalar; path to the QA metrics RDS
#'   file that will be written. Defaults to `"db/qa_metrics.rds"`.
#' @param output_dir Character scalar; directory where the summary table
#'   (CSV) and bar chart image (PNG) will be written. Defaults to
#'   `"reports/ragas"`.
#'
#' @return A list with elements:
#'   - `status`: `"ok"` or `"empty"`
#'   - `n_qa`: number of QA interactions in the log
#'   - `qa_metrics_path`: path to the saved metrics RDS file (if any)
#'   - `summary_csv_path`: path to the summary CSV file (if any)
#'   - `plot_path`: path to the bar chart PNG file (if any)
#' @export
generate_ragas_report <- function(
  qa_log_path     = "db/qa_log.rds",
  qa_metrics_path = "db/qa_metrics.rds",
  output_dir      = "reports/ragas"
) {
  # 1. Load QA log
  qa_log <- load_qa_log(qa_log_path)
  n_qa   <- nrow(qa_log)

  if (n_qa == 0L) {
    return(list(
      status           = "empty",
      n_qa             = 0L,
      qa_metrics_path  = NA_character_,
      summary_csv_path = NA_character_,
      plot_path        = NA_character_
    ))
  }

  # 2. Compute metrics and summary
  qa_metrics <- compute_ragas_metrics(qa_log)
  summary_tbl <- summarize_ragas(qa_metrics)

  # 3. Ensure output directories exist
  metrics_dir <- dirname(qa_metrics_path)
  if (!dir.exists(metrics_dir)) {
    dir.create(metrics_dir, recursive = TRUE, showWarnings = FALSE)
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # 4. Save metrics as RDS
  save_qa_metrics(qa_metrics, qa_metrics_path)

  # 5. Save summary as CSV
  summary_csv_path <- file.path(output_dir, "ragas_summary.csv")
  utils::write.csv(summary_tbl, file = summary_csv_path, row.names = FALSE)

  # 6. Save bar chart of mean metrics as PNG
  plot_path <- file.path(output_dir, "ragas_means.png")
  grDevices::png(filename = plot_path, width = 800, height = 600)
  on.exit(grDevices::dev.off(), add = TRUE)

  plot_ragas_means(qa_metrics)

  list(
    status           = "ok",
    n_qa             = n_qa,
    qa_metrics_path  = qa_metrics_path,
    summary_csv_path = summary_csv_path,
    plot_path        = plot_path
  )
}
