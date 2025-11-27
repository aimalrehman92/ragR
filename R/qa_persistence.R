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
