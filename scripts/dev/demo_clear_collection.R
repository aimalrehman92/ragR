# scripts/dev/demo_clear_collection.R

library(ragR)

collection_name <- "demo_collection"

cat("Clearing collection:", collection_name, "\n")

vectorstore_delete_collection(collection_name)

cat("Collection cleared.\n")
