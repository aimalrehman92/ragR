#' Save a QA log to disk
#'
#' This helper saves a QA log tibble (as produced by [log_rag_interaction()])
#' to an RDS file. By default, it writes to `db/qa_log.rds` inside the
#' project directory.
#'
#' @param qa_log A tibble created by [qa_log_empty()] and populated by
#'   [log_rag_interaction()].
#' @param path Character scalar; path to the RDS file. Defaults to
#'   `"db/qa_log.rds"`.
#'
#' @return Invisibly, the path to the saved file.
#' @export
save_qa_log <- function(qa_log, path = "db/qa_log.rds") {
  if (!tibble::is_tibble(qa_log)) {
    stop("qa_log must be a tibble.", call. = FALSE)
  }

  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }

  saveRDS(qa_log, file = path)
  invisible(path)
}

#' Load a QA log from disk
#'
#' This helper loads a QA log tibble from an RDS file. If the file does
#' not exist, it returns an empty QA log created by [qa_log_empty()].
#'
#' @param path Character scalar; path to the RDS file. Defaults to
#'   `"db/qa_log.rds"`.
#'
#' @return A tibble containing the QA log.
#' @export
load_qa_log <- function(path = "db/qa_log.rds") {
  if (!file.exists(path)) {
    return(qa_log_empty())
  }

  obj <- readRDS(path)

  if (!tibble::is_tibble(obj)) {
    stop("Object loaded from ", path, " is not a tibble.", call. = FALSE)
  }

  obj
}

#' Save QA metrics to disk
#'
#' This helper saves a QA metrics tibble (as produced by
#' [compute_ragas_metrics()]) to an RDS file. By default, it writes to
#' `db/qa_metrics.rds` inside the project directory.
#'
#' @param qa_metrics A tibble created by [compute_ragas_metrics()].
#' @param path Character scalar; path to the RDS file. Defaults to
#'   `"db/qa_metrics.rds"`.
#'
#' @return Invisibly, the path to the saved file.
#' @export
save_qa_metrics <- function(qa_metrics, path = "db/qa_metrics.rds") {
  if (!tibble::is_tibble(qa_metrics)) {
    stop("qa_metrics must be a tibble.", call. = FALSE)
  }

  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }

  saveRDS(qa_metrics, file = path)
  invisible(path)
}

#' Load QA metrics from disk
#'
#' This helper loads QA metrics from an RDS file. If the file does not
#' exist, it returns an empty metrics tibble created by
#' [qa_metrics_empty()].
#'
#' @param path Character scalar; path to the RDS file. Defaults to
#'   `"db/qa_metrics.rds"`.
#'
#' @return A tibble containing the QA metrics.
#' @export
load_qa_metrics <- function(path = "db/qa_metrics.rds") {
  if (!file.exists(path)) {
    return(qa_metrics_empty())
  }

  obj <- readRDS(path)

  if (!tibble::is_tibble(obj)) {
    stop("Object loaded from ", path, " is not a tibble.", call. = FALSE)
  }

  obj
}


#' Clear the QA log on disk
#'
#' This helper overwrites the QA log file with an empty QA log (as
#' created by [qa_log_empty()]). It is intended for starting a fresh
#' evaluation session.
#'
#' @param path Character scalar; path to the QA log RDS file. Defaults
#'   to `"db/qa_log.rds"`.
#'
#' @return Invisibly, the path to the saved file.
#' @export
clear_qa_log <- function(path = "db/qa_log.rds") {
  log <- qa_log_empty()
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }
  saveRDS(log, file = path)
  invisible(path)
}

#' Clear QA metrics on disk
#'
#' This helper overwrites the QA metrics file with an empty metrics
#' tibble (as created by [qa_metrics_empty()]).
#'
#' @param path Character scalar; path to the QA metrics RDS file.
#'   Defaults to `"db/qa_metrics.rds"`.
#'
#' @return Invisibly, the path to the saved file.
#' @export
clear_qa_metrics <- function(path = "db/qa_metrics.rds") {
  metrics <- qa_metrics_empty()
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }
  saveRDS(metrics, file = path)
  invisible(path)
}
