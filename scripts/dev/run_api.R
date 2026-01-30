# scripts/dev/run_api.R

library(plumber)

# ---------------- USER CHOICE -------------------------------------------------
# Choose ONE:
#   "approx" = lexical proxy metrics (fast, deterministic, no API calls)
#   "llm"    = LLM-judged metrics (requires OPENAI_API_KEY; slower/cost)
RAGAS_MODE <- "approx"
options(ragR.ragas_mode = RAGAS_MODE)
# -----------------------------------------------------------------------------

api <- plumber::pr()

# ---- CORS filter -------------------------------------------------------------
api$filter("cors", function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")

  if (identical(req$REQUEST_METHOD, "OPTIONS")) {
    res$status <- 200
    return(list())
  }

  forward()
})
# -----------------------------------------------------------------------------

# NOTE: /ingest removed (no ingestion API)
api$handle("POST", "/chat",  source("inst/api/pr_chat.R")[[1]])
api$handle("POST", "/clear", source("inst/api/pr_clear.R")[[1]])

api$handle("POST", "/clear_all", function(req, res) {
  ragR::api_clear_all_handler()
})

# RAGAS: per-QA metrics (mode comes from options())
api$handle("GET", "/ragas", function(req, res) {
  ragR::api_ragas_handler(mode = getOption("ragR.ragas_mode", "approx"))
})

# RAGAS: report (mode comes from options())
api$handle("POST", "/ragas/report", function(req, res) {
  ragR::api_ragas_report_handler(mode = getOption("ragR.ragas_mode", "approx"))
})

api$handle("POST", "/ragas/clear", function(req, res) {
  ragR::api_ragas_clear_handler()
})

plumber::pr_run(api, host = "0.0.0.0", port = 8000)
