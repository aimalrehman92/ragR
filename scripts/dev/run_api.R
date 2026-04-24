# scripts/dev/run_api.R
#
# Purpose:
#   Run the minimal ragR Plumber API for chatbot and RAGAS evaluation frontends.
#
# Usage, from project root:
#   Rscript scripts/dev/run_api.R

library(plumber)
library(ragR)

# ---------------- USER SETTINGS ----------------------------------------------

JUDGE_MODEL <- "gpt-4o-mini"

HOST <- "0.0.0.0"
PORT <- 8000L

# ------------------------------------------------------------------------------

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
# NOTE: document ingestion is handled through scripts/dev/demo_ingest.R.

api$handle("POST", "/chat",  source("inst/api/pr_chat.R")[[1]])
api$handle("POST", "/clear", source("inst/api/pr_clear.R")[[1]])

api$handle("POST", "/clear_all", function(req, res) {
  ragR::api_clear_all_handler()
})

# RAGAS: per-QA LLM-scored metrics
api$handle("GET", "/ragas", function(req, res) {
  ragR::api_ragas_handler(
    judge_model = JUDGE_MODEL
  )
})

# RAGAS: LLM-scored report
api$handle("POST", "/ragas/report", function(req, res) {
  ragR::api_ragas_report_handler(
    judge_model = JUDGE_MODEL
  )
})

# RAGAS: clear QA log, QA metrics, and report files
api$handle("POST", "/ragas/clear", function(req, res) {
  ragR::api_ragas_clear_handler()
})

cat("Starting ragR API...\n")
cat("  Host        :", HOST, "\n")
cat("  Port        :", PORT, "\n")
cat("  Judge model :", JUDGE_MODEL, "\n")
cat("  Docs        :", paste0("http://127.0.0.1:", PORT, "/__docs__/"), "\n\n")

plumber::pr_run(api, host = HOST, port = PORT)