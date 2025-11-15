# inst/api/pr_ingest.R

library(jsonlite)
library(ragR)

#* Ingest documents into the vector store
#*
#* This endpoint triggers the data ingestion pipeline. It expects a JSON body
#* with file paths and optional configuration (e.g., collection name).
#*
#* Example JSON body:
#* {
#*   "paths": ["data-raw/doc1.pdf", "data-raw/notes.txt"],
#*   "collection": "default",
#*   "chunk_size": 500,
#*   "chunk_overlap": 50
#* }
#*
#* @post /ingest
#* @serializer json
function(req) {
  # Parse JSON body into an R list
  body <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)

  # Delegate to the package-level API handler
  result <- ragR::api_ingest_handler(body)

  return(result)
}
