#' ragR: Retrieval-Augmented Generation + RAGAS Evaluation in R
#'
#' @description
#' The **ragR** package provides a complete Retrieval-Augmented Generation
#' (RAG) system implemented in R. It includes:
#'
#' - A data ingestion pipeline for PDF, text, and Word documents
#' - An R-native vector store for chunk embeddings
#' - A configurable RAG pipeline that uses OpenAI embeddings and chat models
#' - Persistent QA logging for reproducible evaluation
#' - RAGAS-style metrics (context precision/recall, relevance, faithfulness)
#' - A Plumber-based HTTP API suitable for chatbot frontends
#'
#' @section Architecture:
#'
#' The package is organized into several modules:
#'
#' - **Ingestion (`ingestion_files.R`)**  
#'   Extracts text from PDF/TXT/DOCX files, chunks it, and prepares data
#'   for embedding and storage.
#'
#' - **Embeddings & Chat (`embeddings_openai.R`)**  
#'   Wraps the OpenAI REST API for embeddings (e.g.
#'   `"text-embedding-3-small"`) and chat completion models (e.g.
#'   `"gpt-4o-mini"`). Reads the API key from `OPENAI_API_KEY`.
#'
#' - **Vector Store (`vectorstore_interface.R`)**  
#'   Implements an R-native vector store using tibbles and numeric matrices,
#'   with functions to add, query, and clear stored embeddings.
#'
#' - **RAG Pipeline (`rag_pipeline.R`)**  
#'   Given a user question, the pipeline:
#'   1. Computes an embedding for the question  
#'   2. Retrieves top-k similar chunks from the vector store  
#'   3. Builds a context-aware prompt  
#'   4. Calls the OpenAI chat model to generate an answer
#'
#' - **QA Logging & Persistence (`qa_persistence.R`)**  
#'   Stores each interaction in `db/qa_log.rds`, including question, model
#'   answer, retrieved chunks, model names, and timestamps. Provides helpers
#'   to load, save, and clear logs.
#'
#' - **RAGAS Metrics & Summary (`ragas_metrics.R`, `ragas_summary.R`)**  
#'   Computes simple RAGAS-style metrics (context precision/recall, answer
#'   relevance, faithfulness, and an overall score) and summarizes them
#'   across all QA pairs.
#'
#' - **Reports (`ragas_report.R`)**  
#'   Generates a performance report by:
#'   - Computing metrics from the QA log  
#'   - Saving `db/qa_metrics.rds`  
#'   - Writing `reports/ragas/ragas_summary.csv`  
#'   - Writing a bar chart of mean metrics to
#'     `reports/ragas/ragas_means.png`
#'
#' - **API Handlers (`api_handlers.R`)**  
#'   Functions that connect the core logic to HTTP endpoints. These are
#'   called by the Plumber router defined in `inst/api/`.
#'
#' @section HTTP API:
#'
#' When `scripts/dev/run_api.R` is executed, a Plumber server exposes:
#'
#' - `POST /chat`  
#'   Runs the RAG pipeline for a user question and logs the result into
#'   `db/qa_log.rds`.
#'
#' - `POST /ingest`  
#'   Ingests one or more documents into the vector store.
#'
#' - `POST /clear`  
#'   Clears the vector store.
#'
#' - `GET /ragas`  
#'   Returns RAGAS-style metrics for each logged QA pair.
#'
#' - `POST /ragas/report`  
#'   Computes metrics from the QA log, saves the metrics and report
#'   artifacts to disk, and returns a small summary object.
#'
#' - `POST /ragas/clear`  
#'   Clears the QA log, QA metrics, and report files.
#'
#' @section Usage:
#'
#' Programmatic usage inside R:
#'
#' ```r
#' library(ragR)
#'
#' # Run a RAG query
#' res <- query_rag(
#'   question        = "What is this essay about?",
#'   collection      = "demo_ingest_test",
#'   top_k           = 2,
#'   embedding_model = "text-embedding-3-small",
#'   chat_model      = "gpt-4o-mini"
#' )
#'
#' cat(res$answer)
#' ```
#'
#' As a backend service:
#'
#' ```r
#' # From the project root
#' source("scripts/dev/run_api.R")
#' # Visit http://127.0.0.1:8000/__docs__/ for interactive docs
#' ```
#'
#' @section Author:
#' Muhammad Aimal Rehman
#'
#' @keywords RAG NLP OpenAI RAGAS chatbot embeddings
"_PACKAGE"
