# scripts/dev/qa/export_for_ragas_python.R  (or just run interactively)

qa <- readRDS("db/qa_log.rds")

# Keep only what Python RAGAS needs
qa_out <- qa[, c("qa_id","question","answer_model","answer_reference","retrieved_texts")]

# Rename to RAGAS-friendly names
names(qa_out) <- c("qa_id","question","answer","ground_truth","contexts")

# Ensure contexts is list-of-character per row (yours already is, but enforce)
qa_out$contexts <- lapply(qa_out$contexts, function(x) as.character(unlist(x, use.names = FALSE)))

# Write JSON Lines (one JSON object per row)
dir.create("db", showWarnings = FALSE)
con <- file("db/qa_for_ragas_python.jsonl", open = "wt")
for (i in seq_len(nrow(qa_out))) {
  cat(jsonlite::toJSON(qa_out[i, ], auto_unbox = TRUE), "\n", file = con, sep = "")
}
close(con)

# Optional: also export your R summary so Python can compare
# (you already have ragas_summary.csv, just upload it)
