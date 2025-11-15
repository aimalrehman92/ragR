# R/utils.R

#' Load RAG configuration
#'
#' Placeholder for a configuration loader. In the future, this can read
#' a YAML file (e.g., from inst/config) and merge it with environment
#' variables and defaults.
#'
#' @param path Optional path to a YAML config file. If NULL, a default
#'   location may be used in the future.
#'
#' @return A list of configuration values.
#' @export
load_rag_config <- function(path = NULL) {
  stop("load_rag_config() not implemented yet.")
}

#' Get an environment variable or fail with a clear error
#'
#' @param name Name of the environment variable.
#'
#' @return The value of the environment variable as a character scalar.
#' @keywords internal
get_env_or_stop <- function(name) {
  value <- Sys.getenv(name, unset = NA_character_)
  if (is.na(value) || identical(value, "")) {
    stop(sprintf("Environment variable '%s' is not set.", name), call. = FALSE)
  }
  value
}
