# inst/api/pr_ragas.R

library(ragR)

#* Compute RAGAS metrics from the saved QA log
#*
#* Computes LLM-scored RAGAS-style metrics from the logged Q/A interactions.
#* The metrics can run with or without `answer_reference`; if no reference
#* answer is available, the model answer is used where needed.
#*
#* @get /ragas
#* @serializer json
function(req, res) {
  ragR::api_ragas_handler()
}


#* Generate and save a RAGAS performance report
#*
#* Computes LLM-scored RAGAS-style metrics, saves metrics to disk, writes a
#* summary CSV, and generates a chart.
#*
#* @post /ragas/report
#* @serializer json
function(req, res) {
  ragR::api_ragas_report_handler()
}


#* Clear QA log, QA metrics, and RAGAS report files
#*
#* @post /ragas/clear
#* @serializer json
function(req, res) {
  ragR::api_ragas_clear_handler()
}