library(plumber)

api <- plumber::pr()

api$handle("POST", "/ingest", source("inst/api/pr_ingest.R")[[1]])
api$handle("POST", "/chat",   source("inst/api/pr_chat.R")[[1]])
api$handle("POST", "/clear",  source("inst/api/pr_clear.R")[[1]])

# New: clear entire vector store (all collections)
api$handle("POST", "/clear_all", function(req, res) {
  ragR::api_clear_all_handler()
})

api$handle("GET", "/ragas", function(req, res) {
  ragR::api_ragas_handler()
})

api$handle("POST", "/ragas/report", function(req, res) {
  ragR::api_ragas_report_handler()
})

api$handle("POST", "/ragas/clear", function(req, res) {
  ragR::api_ragas_clear_handler()
})

plumber::pr_run(api, host = "0.0.0.0", port = 8000)
