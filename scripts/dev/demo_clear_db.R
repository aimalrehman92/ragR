# scripts/dev/demo_clear_db.R

library(ragR)

cat("Clearing entire vector store...\n")
vectorstore_clear_all()
cat("Done.\n")
