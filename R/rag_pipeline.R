# R/rag_pipeline.R

#' Query the RAG pipeline
#'
#' Takes a user question, embeds it, retrieves relevant chunks from a vector store,
#' constructs a prompt, and calls an LLM to generate an answer.
#'
#' @param question Character scalar.
#' @param collection Vector store collection name.
#' @param top_k Integer; number of chunks to retrieve.
#' @param embedding_model Character; embedding model for the question.
#' @param chat_model Character; chat model for answer generation.
#' @param temperature Numeric; sampling temperature for the chat model.
#' @param max_output_tokens Integer or NULL; maximum output tokens for the chat model.
#' @param score_threshold Numeric; minimum similarity score to keep a retrieved chunk.
#'   Applied only if `retrieved` includes a `score` column.
#' @param system_prompt Character; system instruction prepended to the prompt.
#'
#' @return A list with:
#'   \item{answer}{Model-generated answer}
#'   \item{retrieved}{Tibble of retrieved chunks (post-filtering)}
#'   \item{prompt}{Final prompt sent to the model}
#'   \item{model}{Model used}
#' @export
query_rag <- function(
  question,
  collection        = "default",
  top_k             = 4L,
  embedding_model   = "text-embedding-3-small",
  chat_model        = "gpt-4o-mini",
  temperature       = 0,
  max_output_tokens = NULL,
  score_threshold   = 0,
  system_prompt     = ""
) {
  if (!is.character(question) || length(question) != 1L) {
    stop("question must be a single character string.", call. = FALSE)
  }
  if (!is.numeric(top_k) || length(top_k) != 1L || top_k <= 0) {
    stop("top_k must be a positive integer.", call. = FALSE)
  }
  if (!is.numeric(score_threshold) || length(score_threshold) != 1L) {
    stop("score_threshold must be a single numeric value.", call. = FALSE)
  }
  if (!is.numeric(temperature) || length(temperature) != 1L) {
    stop("temperature must be a single numeric value.", call. = FALSE)
  }
  if (!is.null(max_output_tokens)) {
    if (!is.numeric(max_output_tokens) || length(max_output_tokens) != 1L || max_output_tokens <= 0) {
      stop("max_output_tokens must be a single positive integer or NULL.", call. = FALSE)
    }
    max_output_tokens <- as.integer(max_output_tokens)
  }

  # 1) Embed the question
  q_emb_mat <- get_openai_embeddings(
    texts = question,
    model = embedding_model
  )

  # get_openai_embeddings() returns a matrix (1 x d); convert to numeric vector
  query_embedding <- as.numeric(q_emb_mat[1, ])

  # 2) Query the vector store
  retrieved <- vectorstore_query(
    collection      = collection,
    query_embedding = query_embedding,
    top_k           = as.integer(top_k)
  )

  if (!is.data.frame(retrieved) || nrow(retrieved) == 0L) {
    stop("vectorstore_query() returned no results.", call. = FALSE)
  }

  # 2b) Optional score threshold filter (only if 'score' exists)
  if ("score" %in% names(retrieved)) {
    retrieved <- retrieved[!is.na(retrieved$score) & retrieved$score >= score_threshold, , drop = FALSE]
  }

  if (!is.data.frame(retrieved) || nrow(retrieved) == 0L) {
    stop("No retrieved chunks passed score_threshold.", call. = FALSE)
  }

  # 3) Build prompt (now supports user-provided system_prompt)
  prompt <- build_rag_prompt(
    question       = question,
    retrieved      = retrieved,
    system_prompt  = system_prompt
  )

  # 4) Call chat model
  # NOTE: generate_openai_chat() must support these args; if your implementation
  # uses different names, tell me its signature and I’ll align it.
  answer <- generate_openai_chat(
    prompt            = prompt,
    model             = chat_model,
    system_message    = system_prompt,
    temperature       = temperature,
    max_output_tokens = max_output_tokens
  )


  # 5) Return structured result
  list(
    answer    = answer,
    retrieved = retrieved,
    prompt    = prompt,
    model     = chat_model
  )
}

#' Build a RAG prompt from question and retrieved chunks
#'
#' @param question Character scalar.
#' @param retrieved A tibble/data frame of retrieved chunks.
#' @param system_prompt Character; system instruction prepended to the prompt.
#'
#' @return Character scalar: the constructed prompt.
#' @keywords internal
build_rag_prompt <- function(
  question,
  retrieved,
  system_prompt = ""
) {
  if (!is.character(question) || length(question) != 1L) {
    stop("question must be a single character string.", call. = FALSE)
  }
  if (!is.data.frame(retrieved) || nrow(retrieved) == 0L) {
    stop("retrieved must be a non-empty data frame.", call. = FALSE)
  }
  if (!is.character(system_prompt) || length(system_prompt) != 1L) {
    stop("system_prompt must be a single character string.", call. = FALSE)
  }

  # Find the column that holds chunk text
  text_col <- NULL
  if ("document" %in% names(retrieved)) {
    text_col <- "document"
  } else if ("text" %in% names(retrieved)) {
    text_col <- "text"
  } else {
    stop(
      "retrieved must have a 'document' or 'text' column containing chunk text.",
      call. = FALSE
    )
  }

  contexts <- retrieved[[text_col]]

  context_block <- paste0(
    "Chunk ", seq_along(contexts), ":\n",
    contexts,
    collapse = "\n\n"
  )

  # Ensure system prompt ends with a blank line if provided
  sys <- ""
  if (nzchar(trimws(system_prompt))) {
    sys <- paste0(system_prompt, "\n\n")
  }

  Prompt <- paste0(
    sys,
    "You are given the following context passages:\n\n",
    context_block,
    "\n\nUsing ONLY the information in the context above, answer the question below.\n",
    "If the context does not contain the answer, say \"I don't know based on the provided context.\"\n\n",
    "Question:\n",
    question,
    "\n\nAnswer:"
  )

  Prompt
}
