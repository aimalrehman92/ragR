library(jsonlite)
library(ragR)

#* Ask a question using the RAG pipeline
#*
#* This endpoint sends a question to the RAG pipeline and returns
#* the model's answer along with (optionally) retrieved context.
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
  # Parse JSON body into an R list
  body <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)

  # Delegate to the package-level API handler
  result <- ragR::api_chat_handler(body)

  return(result)
}
