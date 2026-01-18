# R/embeddings_openai.R

#' Get embeddings from OpenAI
#'
#' Calls the OpenAI embeddings endpoint to obtain vector representations for
#' one or more input texts. Used by both ingestion and query-time retrieval.
#'
#' @param texts Character vector of texts to embed.
#' @param model Character scalar; embedding model name (e.g., "text-embedding-3-small").
#' @param api_key OpenAI API key. If NULL, uses the OPENAI_API_KEY environment variable.
#'
#' @return A numeric matrix with one row per input text.
#' @export
get_openai_embeddings <- function(
  texts,
  model   = "text-embedding-3-small",
  api_key = NULL
) {
  if (!is.character(texts)) {
    stop("texts must be a character vector.", call. = FALSE)
  }
  if (length(texts) == 0L) {
    stop("texts must have length >= 1.", call. = FALSE)
  }
  if (!is.character(model) || length(model) != 1L || !nzchar(model)) {
    stop("model must be a single non-empty character string.", call. = FALSE)
  }

  if (is.null(api_key)) {
    api_key <- get_env_or_stop("OPENAI_API_KEY")
  }

  body <- list(
    model = model,
    input = as.list(texts)
  )

  req <- httr2::request("https://api.openai.com/v1/embeddings") |>
    httr2::req_headers(
      Authorization  = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(body)

  resp <- httr2::req_perform(req)

  status <- httr2::resp_status(resp)
  if (status >= 300) {
    msg <- NULL
    try({
      parsed_err <- httr2::resp_body_json(resp, simplifyVector = TRUE)
      if (!is.null(parsed_err$error$message)) msg <- parsed_err$error$message
    }, silent = TRUE)

    stop(
      "OpenAI embeddings request failed with status ", status, ". ",
      if (!is.null(msg)) paste0("Server message: ", msg),
      call. = FALSE
    )
  }

  parsed <- httr2::resp_body_json(resp, simplifyVector = FALSE)

  if (is.null(parsed$data) || length(parsed$data) != length(texts)) {
    stop("Unexpected response format from OpenAI embeddings API.", call. = FALSE)
  }

  emb_list <- lapply(parsed$data, function(d) {
    as.numeric(unlist(d$embedding, use.names = FALSE))
  })

  emb_mat <- do.call(rbind, emb_list)
  storage.mode(emb_mat) <- "double"

  if (is.null(dim(emb_mat))) {
    emb_mat <- matrix(emb_mat, nrow = 1)
  }

  emb_mat
}


#' Generate a chat completion from OpenAI
#'
#' Used by the RAG pipeline to generate the final answer from a constructed prompt.
#'
#' @param prompt Character scalar; the final prompt (includes question + context).
#' @param model Character scalar; chat model name (e.g., "gpt-4o-mini").
#' @param system_message Optional system message. If NULL, a default is used.
#' @param temperature Numeric; sampling temperature (0 = deterministic).
#' @param max_output_tokens Integer; maximum tokens to generate. Sent as `max_tokens`.
#' @param seed Optional integer. If provided (and supported by the underlying
#'   model/provider), it is sent as `seed` to encourage reproducible outputs.
#' @param api_key OpenAI API key. If NULL, uses the OPENAI_API_KEY environment variable.
#'
#' @return Character scalar containing the model's answer.
#' @export
generate_openai_chat <- function(
  prompt,
  model             = "gpt-4o-mini",
  system_message    = NULL,
  temperature       = 0,
  max_output_tokens = 512L,
  seed              = NULL,
  api_key           = NULL
) {
  if (!is.character(prompt) || length(prompt) != 1L) {
    stop("prompt must be a single character string.", call. = FALSE)
  }
  if (!is.character(model) || length(model) != 1L || !nzchar(model)) {
    stop("model must be a single non-empty character string.", call. = FALSE)
  }

  if (!is.numeric(temperature) || length(temperature) != 1L || is.na(temperature) || temperature < 0) {
    stop("temperature must be a single numeric value >= 0.", call. = FALSE)
  }

  if (!is.numeric(max_output_tokens) || length(max_output_tokens) != 1L) {
    stop("max_output_tokens must be a single numeric value.", call. = FALSE)
  }
  max_output_tokens <- as.integer(max_output_tokens)
  if (is.na(max_output_tokens) || max_output_tokens <= 0L) {
    stop("max_output_tokens must be a positive integer.", call. = FALSE)
  }

  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1L || is.na(seed)) {
      stop("seed must be NULL or a single non-NA numeric value.", call. = FALSE)
    }
    seed <- as.integer(seed)
    if (is.na(seed) || seed < 0L) {
      stop("seed must be a non-negative integer.", call. = FALSE)
    }
  }

  if (is.null(api_key)) {
    api_key <- get_env_or_stop("OPENAI_API_KEY")
  }

  if (is.null(system_message)) {
    system_message <- paste(
      "You are a helpful assistant that answers based only on the provided context.",
      "If the context is insufficient, say \"I don't know based on the provided context.\""
    )
  }

  body <- list(
    model = model,
    messages = list(
      list(role = "system", content = system_message),
      list(role = "user",   content = prompt)
    ),
    temperature = temperature,
    max_tokens  = max_output_tokens
  )

  if (!is.null(seed)) {
    body$seed <- seed
  }

  req <- httr2::request("https://api.openai.com/v1/chat/completions") |>
    httr2::req_headers(
      Authorization  = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(body)

  resp <- httr2::req_perform(req)

  status <- httr2::resp_status(resp)
  if (status >= 300) {
    msg <- NULL
    try({
      parsed_err <- httr2::resp_body_json(resp, simplifyVector = TRUE)
      if (!is.null(parsed_err$error$message)) msg <- parsed_err$error$message
    }, silent = TRUE)

    stop(
      "OpenAI chat completion request failed with status ", status, ". ",
      if (!is.null(msg)) paste0("Server message: ", msg),
      call. = FALSE
    )
  }

  parsed <- httr2::resp_body_json(resp, simplifyVector = FALSE)

  choices <- parsed$choices
  if (is.null(choices) || length(choices) == 0L) {
    stop("Unexpected response format from OpenAI chat API: no choices.", call. = FALSE)
  }

  message_obj <- choices[[1]]$message
  if (is.list(message_obj) && !is.null(message_obj$content)) {
    return(as.character(message_obj$content))
  }

  stop("OpenAI chat API returned an unexpected message structure.", call. = FALSE)
}
