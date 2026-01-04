#' API handler: compute RAGAS metrics and summary from saved QA log
#'
#' @param path Character scalar; path to the QA log RDS file. Defaults to
#'   `"db/qa_log.rds"`.
#' @param mode `"approx"` or `"llm"`. If NULL, uses option `ragR.ragas_mode`.
#'
#' @return A list with `status`, `n_qa`, `metrics`, `summary`.
#' @export
api_ragas_handler <- function(path = "db/qa_log.rds", mode = NULL) {
  qa_log <- load_qa_log(path)

  metrics <- compute_ragas_metrics(qa_log, mode = mode)
  summary <- summarize_ragas(metrics)

  list(
    status  = "ok",
    n_qa    = nrow(qa_log),
    metrics = metrics,
    summary = summary
  )
}

#' API handler: generate RAGAS performance report
#'
#' @param qa_log_path Character scalar; path to the QA log RDS file.
#'   Defaults to `"db/qa_log.rds"`.
#' @param qa_metrics_path Character scalar; path to the QA metrics RDS file.
#'   Defaults to `"db/qa_metrics.rds"`.
#' @param output_dir Character scalar; directory where outputs are written.
#'   Defaults to `"reports/ragas"`.
#' @param mode `"approx"` or `"llm"`. If NULL, uses option `ragR.ragas_mode`.
#'
#' @return A list: `status`, `n_qa`, `qa_metrics_path`, `summary_csv_path`, `plot_path`.
#' @export
api_ragas_report_handler <- function(
  qa_log_path     = "db/qa_log.rds",
  qa_metrics_path = "db/qa_metrics.rds",
  output_dir      = "reports/ragas",
  mode            = NULL
) {
  generate_ragas_report(
    qa_log_path     = qa_log_path,
    qa_metrics_path = qa_metrics_path,
    output_dir      = output_dir,
    mode            = mode
  )
}


#' API handler: clear the entire vector store
#'
#' This handler wipes the R-native vector store by calling
#' [vectorstore_clear_all()], removing all collections and stored
#' embeddings.
#'
#' @return A list with elements `status` and `message`, suitable for
#'   JSON serialization.
#' @export
api_clear_all_handler <- function() {
  vectorstore_clear_all()

  list(
    status  = "ok",
    message = "Vector store cleared: all collections removed."
  )
}

