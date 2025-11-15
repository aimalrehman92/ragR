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
  stop("query_rag() not implemented yet.")
}

#' Build RAG prompt
#'
#' @param question User question.
#' @param retrieved Tibble of retrieved chunks.
#'
#' @return Character scalar containing full prompt.
#' @keywords internal
build_rag_prompt <- function(question, retrieved) {
  stop("build_rag_prompt() not implemented yet.")
}
