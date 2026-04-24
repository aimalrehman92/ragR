# inst/api/pr_clear.R

library(ragR)

#* Clear a vector store collection ("memory")
#*
#* This endpoint deletes a collection from the underlying vector store.
#*
#* Example JSON body:
#* {
#*   "collection": "demo_collection"
#* }
#*
#* If omitted, "default" is used.
#*
#* @post /clear
#* @serializer json
function(req) {
  body <- NULL

  if (!is.null(req$postBody) && nzchar(req$postBody)) {
    body <- tryCatch(
      jsonlite::fromJSON(req$postBody, simplifyVector = FALSE),
      error = function(e) {
        stop("Request body must be valid JSON.", call. = FALSE)
      }
    )
  }

  ragR::api_clear_handler(body)
}