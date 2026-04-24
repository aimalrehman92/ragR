# R/utils.R

#' Load RAG configuration
#'
#' Placeholder for a configuration loader. In the future, this can read
#' a YAML file and merge it with environment variables and defaults.
#'
#' @param path Optional path to a YAML config file. Currently unused.
#'
#' @return A list of configuration values. Currently returns an empty list.
#' @export
load_rag_config <- function(path = NULL) {
  if (!is.null(path) && (!is.character(path) || length(path) != 1L)) {
    stop("path must be NULL or a single character string.", call. = FALSE)
  }

  list()
}

#' Get an environment variable or fail with a clear error
#'
#' @param name Name of the environment variable.
#'
#' @return The value of the environment variable as a character scalar.
#' @keywords internal
get_env_or_stop <- function(name) {
  if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
    stop("name must be a non-empty character string.", call. = FALSE)
  }

  value <- Sys.getenv(name, unset = NA_character_)

  if (is.na(value) || identical(value, "")) {
    stop(sprintf("Environment variable '%s' is not set.", name), call. = FALSE)
  }

  value
}