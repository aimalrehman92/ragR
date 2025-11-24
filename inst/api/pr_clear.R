# inst/api/pr_clear.R

library(jsonlite)
library(ragR)

#* Clear a vector store collection ("memory")
#*
#* This endpoint deletes a collection from the underlying vector store.
#*
#* Example JSON body:
#* {
#*   "collection": "demo_collection"
#* }
#* If omitted, "default" is used.
#*
#* @post /clear
#* @serializer json
function(req) {
  body <- NULL
  if (nzchar(req$postBody)) {
    body <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
  }

  result <- ragR::api_clear_handler(body)

  return(result)
}
