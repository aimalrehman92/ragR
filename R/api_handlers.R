#' API handler: compute RAGAS metrics and summary from saved QA log
#'
#' @param path Character scalar; path to the QA log RDS file. Defaults to
#'   `"db/qa_log.rds"`.
#' @param judge_model Character scalar; judge model used for LLM-scored metrics.
#'   Defaults to `"gpt-4o-mini"`.
#'
#' @return A list with `status`, `n_qa`, `metrics`, `summary`.
#' @export
api_ragas_handler <- function(
  path = "db/qa_log.rds",
  judge_model = "gpt-4o-mini"
) {
  qa_log <- load_qa_log(path)

  if (nrow(qa_log) == 0L) {
    metrics <- qa_metrics_empty()
    summary <- summarize_ragas(metrics)

    return(list(
      status  = "ok",
      message = "QA log is empty. No RAGAS metrics computed.",
      n_qa    = 0L,
      metrics = metrics,
      summary = summary
    ))
  }

  metrics <- compute_ragas_metrics(
    qa_log,
    judge_model = judge_model
  )

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
#' @param judge_model Character scalar; judge model used for LLM-scored metrics.
#'   Defaults to `"gpt-4o-mini"`.
#'
#' @return A list with `status`, `n_qa`, `qa_metrics_path`,
#'   `summary_csv_path`, and `plot_path`.
#' @export
api_ragas_report_handler <- function(
  qa_log_path     = "db/qa_log.rds",
  qa_metrics_path = "db/qa_metrics.rds",
  output_dir      = "reports/ragas",
  judge_model     = "gpt-4o-mini"
) {
  qa_log <- load_qa_log(qa_log_path)

  if (nrow(qa_log) == 0L) {
    return(list(
      status  = "ok",
      message = "QA log is empty. No RAGAS report generated.",
      n_qa    = 0L
    ))
  }

  generate_ragas_report(
    qa_log_path     = qa_log_path,
    qa_metrics_path = qa_metrics_path,
    output_dir      = output_dir,
    judge_model     = judge_model
  )
}


#' API handler: clear RAGAS/QA evaluation files
#'
#' Clears the saved QA log, QA metrics file, and generated RAGAS report files.
#'
#' @param qa_log_path Character scalar; path to the QA log RDS file.
#' @param qa_metrics_path Character scalar; path to the QA metrics RDS file.
#' @param output_dir Character scalar; directory containing RAGAS report files.
#'
#' @return A list with `status` and `message`.
#' @export
api_ragas_clear_handler <- function(
  qa_log_path     = "db/qa_log.rds",
  qa_metrics_path = "db/qa_metrics.rds",
  output_dir      = "reports/ragas"
) {
  clear_qa_log(qa_log_path)

  if (file.exists(qa_metrics_path)) {
    unlink(qa_metrics_path)
  }

  if (dir.exists(output_dir)) {
    unlink(output_dir, recursive = TRUE, force = TRUE)
  }

  list(
    status  = "ok",
    message = "QA log, QA metrics, and RAGAS report files cleared."
  )
}


#' API handler: clear the entire vector store
#'
#' This handler wipes the R-native vector store by calling
#' [vectorstore_clear_all()], removing all collections and stored embeddings.
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