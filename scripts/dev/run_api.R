# scripts/dev/run_api.R

library(plumber)

# Create a new router
api <- plumber::pr()

# Load the endpoints manually
api$handle("POST", "/ingest", source("inst/api/pr_ingest.R")[[1]])
api$handle("POST", "/chat",   source("inst/api/pr_chat.R")[[1]])
api$handle("POST", "/clear",  source("inst/api/pr_clear.R")[[1]])

# New: RAGAS metrics endpoint (GET)
api$handle("GET", "/ragas", function(req, res) {
  # Call the package-level handler; plumber will JSON-encode the list
  ragR::api_ragas_handler()
})

# Run the API
plumber::pr_run(api, host = "0.0.0.0", port = 8000)
