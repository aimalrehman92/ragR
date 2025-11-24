# R/rag_pipeline.R

#' Query the RAG pipeline
#'
#' Takes a user question, embeds it, retrieves relevant chunks from a vector store,
#' constructs a prompt, and calls an LLM to generate an answer.
#'
#' @param question Character scalar.
#' @param collection Vector store collection name.
#' @param top_k Number of chunks to retrieve.
#' @param embedding_model Embedding model for the question.
#' @param chat_model Chat model for answer generation.
#'
#' @return A list with:
#'   \item{answer}{Model-generated answer}
#'   \item{retrieved}{Tibble of retrieved chunks}
#'   \item{prompt}{Final prompt sent to the model}
#'   \item{model}{Model used}
#' @export
query_rag <- function(
  question,
  collection      = "default",
  top_k           = 4L,
  embedding_model = "text-embedding-3-small",
  chat_model      = "gpt-4o-mini"
) {
  if (!is.character(question) || length(question) != 1L) {
    stop("question must be a single character string.", call. = FALSE)
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
    top_k           = top_k
  )

  # Expect a data frame with at least a text/document column
  if (!is.data.frame(retrieved) || nrow(retrieved) == 0L) {
    stop("vectorstore_query() returned no results.", call. = FALSE)
  }

  # 3) Build prompt
  prompt <- build_rag_prompt(question, retrieved)

  # 4) Call chat model
  answer <- generate_openai_chat(
    prompt      = prompt,
    model       = chat_model,
    temperature = 0
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
#'
#' @return Character scalar: the constructed prompt.
#' @keywords internal
build_rag_prompt <- function(question, retrieved) {
  if (!is.character(question) || length(question) != 1L) {
    stop("question must be a single character string.", call. = FALSE)
  }

  if (!is.data.frame(retrieved) || nrow(retrieved) == 0L) {
    stop("retrieved must be a non-empty data frame.", call. = FALSE)
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

  prompt <- paste0(
    "You are given the following context passages:\n\n",
    context_block,
    "\n\nUsing ONLY the information in the context above, answer the question below.\n",
    "If the context does not contain the answer, say \"I don't know based on the provided context.\"\n\n",
    "Question:\n",
    question,
    "\n\nAnswer:"
  )

  prompt
}
