# inst/api/pr_chat.R

library(jsonlite)
library(ragR)

#* Ask a question using the RAG pipeline
#*
#* This endpoint sends a question to the RAG pipeline, returns the model answer,
#* and logs the Q/A interaction into the QA log.
#*
#* Example JSON body:
#* {
#*   "question": "What does the compliance document say about refunds?",
#*   "collection": "default",
#*   "top_k": 4
#* }
#*
#* @post /chat
#* @serializer json
function(req) {
  if (is.null(req$postBody) || !nzchar(req$postBody)) {
    stop("Request body must be non-empty JSON.", call. = FALSE)
  }

  body <- tryCatch(
    jsonlite::fromJSON(req$postBody, simplifyVector = FALSE),
    error = function(e) {
      stop("Request body must be valid JSON.", call. = FALSE)
    }
  )

  ragR::api_chat_handler(body)
}