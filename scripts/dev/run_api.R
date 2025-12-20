# scripts/dev/run_api.R

library(plumber)

if (!requireNamespace("ragR", quietly = TRUE)) {
  library(devtools)
  devtools::load_all()
}
# Create a new router
api <- plumber::pr()


# Load the endpoints manually
api$handle("POST", "/ingest", source("inst/api/pr_ingest.R")[[1]])
api$handle("POST", "/chat",   source("inst/api/pr_chat.R")[[1]])
api$handle("POST", "/clear",  source("inst/api/pr_clear.R")[[1]])


# ---- CORS filter (works with older plumber versions) ------------------------
api$filter("cors", function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")

  # Handle preflight OPTIONS request
  if (identical(req$REQUEST_METHOD, "OPTIONS")) {
    res$status <- 200
    return(list())
  }

  # Continue to the next filter/endpoint
  forward()
})
# -----------------------------------------------------------------------------


# Ingestion endpoint
api$handle(
  "POST",
  "/ingest",
  source("inst/api/pr_ingest.R")[[1]]
)

# Chat endpoint
api$handle(
  "POST",
  "/chat",
  source("inst/api/pr_chat.R")[[1]]
)

# Clear one collection (your existing behavior in pr_clear.R)
api$handle(
  "POST",
  "/clear",
  source("inst/api/pr_clear.R")[[1]]
)

# Clear ALL collections (vectorstore_clear_all())
api$handle(
  "POST",
  "/clear_all",
  function(req, res) {
    ragR::api_clear_all_handler()
  }
)

# RAGAS: per-QA metrics (JSON)
api$handle(
  "GET",
  "/ragas",
  function(req, res) {
    ragR::api_ragas_handler()
  }
)

# RAGAS: compute report (metrics + summary + plot files)
api$handle(
  "POST",
  "/ragas/report",
  function(req, res) {
    ragR::api_ragas_report_handler()
  }
)

# RAGAS: clear QA log + metrics + report files
api$handle(
  "POST",
  "/ragas/clear",
  function(req, res) {
    ragR::api_ragas_clear_handler()
  }
)

# ---- NEW: serve static RAGAS plots from reports/ragas ----------------------
# This exposes files like reports/ragas/ragas_means.png at:
#   http://127.0.0.1:8000/ragas-plots/ragas_means.png
api <- plumber::pr_static(
  api,
  path = "/ragas-plots",   # URL prefix
  dir  = "reports/ragas"   # local directory where PNGs are saved
)
# -----------------------------------------------------------------------------



# Run the API
plumber::pr_run(api, host = "0.0.0.0", port = 8000)
