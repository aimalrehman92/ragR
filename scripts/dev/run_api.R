# scripts/dev/run_api.R

# Simple development runner for the ragR API.
# Usage (from project root):
#   Rscript scripts/dev/run_api.R
#
# This will start a plumber server on http://localhost:8000
# exposing the /ingest and /chat endpoints.

library(plumber)

api <- plumber::pr("inst/api/plumber.R")

plumber::pr_run(api, host = "0.0.0.0", port = 8000)
