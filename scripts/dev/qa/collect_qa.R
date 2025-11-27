# scripts/dev/collect_qa.R

library(ragR)
library(tibble)

# 1️⃣ Load existing QA log (or create empty log if none exists)
qa_log <- load_qa_log("db/qa_log.rds")

# 2️⃣ Ask a question using RAG
question_text <- "What is the topic of this essay?"

res <- query_rag(
  question        = question_text,
  collection      = "demo_ingest_test",
  top_k           = 2,
  embedding_model = "text-embedding-3-small",
  chat_model      = "gpt-4o-mini"
)

# 3️⃣ Log this interaction
qa_log <- log_rag_interaction(
  qa_log          = qa_log,
  question        = question_text,
  rag_result      = res,
  collection      = "demo_ingest_test",
  chat_model      = "gpt-4o-mini",
  embedding_model = "text-embedding-3-small"
)

# 4️⃣ Save the updated log
save_qa_log(qa_log, "db/qa_log.rds")

cat("✔ QA logged successfully. QA log now contains", nrow(qa_log), "rows.\n")
