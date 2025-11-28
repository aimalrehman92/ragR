# ragR: Retrieval-Augmented Generation + RAGAS Evaluation in R

`ragR` is an R package and backend system that implements:

- **RAG pipeline** (Retrieval-Augmented Generation)
- **RAGAS-style evaluation metrics**
- **Persistent QA logging**
- **An R-native vector store**
- **OpenAI-based embedding + chat generation**
- **A Plumber API backend** for chatbot and evaluation frontends

This repository is designed for two modes of use:

1. **As an R library** — usable via `library(ragR)`
2. **As a backend service** — accessible from a frontend chatbot via HTTP API

The package is intended for research, coursework, and practical RAG evaluation workflows.


## 🚀 Features

### 🔍 1. Data Ingestion Pipeline
- Supports PDF, TXT, and DOCX
- Extracts raw text using `pdftools` / `readtext`
- Splits text into overlapping chunks
- Generates OpenAI embeddings for each chunk
- Stores chunks + embeddings in an R-native vector store (`db/vectorstore.rds`)
- Includes a clear/reset function for wiping the vector store

---

### 🤖 2. Retrieval-Augmented Generation (RAG) Pipeline
- Embeds user queries using OpenAI (`text-embedding-3-small`)
- Retrieves nearest chunks with a simple cosine-similarity search
- Constructs RAG prompts using retrieved context
- Generates answers using GPT models (e.g., `gpt-4o-mini`)
- Fully accessible via the `/chat` API endpoint

---

### 🧪 3. Persistent QA Logging System
All `/chat` interactions are logged automatically into: \
db/qa_log.rds 


Each entry includes:
- question
- model answer
- retrieved chunk IDs and texts
- embedding + chat model names
- timestamp

This log powers the RAGAS evaluation step.

---

### 📈 4. RAGAS-Style Evaluation Metrics
The package computes:

- **Context Precision**
- **Context Recall**
- **Answer Relevance**
- **Faithfulness**
- **RAGAS Overall score**

Metrics are saved to: \
db/qa_metrics.rds \
reports/ragas/ragas_summary.csv \
reports/ragas/ragas_means.png \


---

### 🌐 5. Plumber API Backend
`ragR` exposes a clean backend API for any chatbot UI.

| Endpoint          | Method | Description                             |
|-------------------|--------|-----------------------------------------|
| `/chat`           | POST   | Ask a question (RAG + save to QA log)   |
| `/ingest`         | POST   | Ingest one or more documents            |
| `/clear`          | POST   | Clear the vector store                  |
| `/ragas`          | GET    | Return per-QA RAGAS metrics             |
| `/ragas/report`   | POST   | Compute + save RAGAS summary & image    |
| `/ragas/clear`    | POST   | Clear QA log + metrics + report files   |

The backend can be run locally via:

```r
source("scripts/dev/run_api.R")
```
---

## 📁 Project Structure

```text
ragR/
├── DESCRIPTION
├── NAMESPACE
├── README.md
├── .Rbuildignore
├── .gitignore
├── R/
│   ├── api_handlers.R
│   ├── embeddings_openai.R
│   ├── ingestion_files.R
│   ├── rag_pipeline.R
│   ├── ragas_metrics.R
│   ├── utils.R
│   ├── vectorstore_interface.R
│   └── vectorstore_chroma.R
├── data-raw/
│   ├── happy_essay.pdf
│   └── happy_essay.txt
├── db/
│   ├── vectorstore.rds
│   ├── qa_log.rds
│   └── qa_metrics.rds
├── inst/
│   └── api/
│       ├── plumber.R
│       ├── pr_chat.R
│       ├── pr_clear.R
│       ├── pr_ingest.R
│       ├── pr_ragas_report.R
│       └── pr_ragas_clear.R
├── reports/
│   └── ragas/
│       ├── ragas_summary.csv
│       └── ragas_means.png
├── renv/
│   ├── activate.R
│   ├── settings.json
│   └── .gitignore
├── renv.lock
├── scripts/
│   └── dev/
│       ├── demo_rag.R
│       ├── run_api.R
│       └── collect_qa.R
└── tests/
    ├── testthat.R
    └── testthat/
        ├── test_chunking.R
        ├── test_vectorstore.R
        ├── test_ragas.R
        └── test_logging.R

---

## ⚙️ Installation and Setup

### 1. Clone the repository

```bash
git clone https://github.com/YOUR_USERNAME/ragR.git
cd ragR
```

### 2. Restore the isolated project environment (renv)

The project uses renv to ensure reproducibility.

In R:

```
install.packages("renv")
renv::restore()
```

This installs all package dependencies exactly as locked in renv.lock.

### 3. Load the package during development

```
library(devtools)
devtools::load_all()
```
```load_all()``` loads the package without installing it — ideal for development.

### 4. Set your OpenAI API key

```
usethis::edit_r_environ("user")
```

Add:

```
OPENAI_API_KEY=your_key_here
```
Then restart R.
The embedding and chat functions will automatically read this key.

```
source("scripts/dev/run_api.R")
```

This will launch:

- API base URL: http://127.0.0.1:8000
- Interactive docs (Swagger): http://127.0.0.1:8000/__docs__/

---

## 📝 Usage Examples

Below are examples for using `ragR` both:

- **Directly from R**, and  
- **As an HTTP backend** called from a UI/frontend.

---

## 💬 1. Chat / RAG Query (R-side)

Ask a question using the RAG pipeline:

```r
library(ragR)

result <- query_rag(
  question        = "What is this essay about?",
  collection      = "demo_ingest_test",
  top_k           = 2,
  embedding_model = "text-embedding-3-small",
  chat_model      = "gpt-4o-mini"
)

cat(result$answer)
```

This returns a list with:

- answer
- retrieved chunks
- prompt
- context
- chunk IDs and metadata

## 🌐 2. Chat API Example (frontend ⇄ backend)

If the backend is running:

```
library(httr2)

req_body <- list(
  question = "Summarize the document in one sentence."
)

resp <- request("http://127.0.0.1:8000/chat") |>
  req_method("POST") |>
  req_body_json(req_body) |>
  req_perform()

resp_body_json(resp, simplifyVector = TRUE)
```

The JSON response includes:

- status
- question
- answer
- qa_id
- retrieved (IDs + text)

The backend also logs the interaction automatically into: \
db/qa_log.rds

## 📥 3. Ingest File into Vector Store (API-side)

```
library(httr2)

req_body <- list(
  paths = ["data-raw/myfile.pdf"],
  collection = "demo",
  chunk_size = 500,
  chunk_overlap = 50
)

resp <- request("http://127.0.0.1:8000/ingest") |>
  req_method("POST") |>
  req_body_json(req_body) |>
  req_perform()

resp_body_json(resp)
```

## 📈 4. Compute RAGAS Metrics (API-side)

```
library(httr2)

resp <- request("http://127.0.0.1:8000/ragas/report") |>
  req_method("POST") |>
  req_perform()

resp_body_json(resp, simplifyVector = TRUE)
```

This triggers:

- loading db/qa_log.rds
- computing RAGAS-style metrics
- saving:

    - db/qa_metrics.rds
    - reports/ragas/ragas_summary.csv
    - reports/ragas/ragas_means.png

The response includes the summary table and file paths.

## 🧹 5. Clear QA Log + Metrics + Reports (API-side)

```
library(httr2)

resp <- request("http://127.0.0.1:8000/ragas/clear") |>
  req_method("POST") |>
  req_perform()

resp_body_json(resp, simplifyVector = TRUE)
```

This resets:

- db/qa_log.rds
- db/qa_metrics.rds
- deletes:
    - reports/ragas/ragas_summary.csv
    - reports/ragas/ragas_means.png

## 🔍 6. RAGAS Evaluation from R (R-side)

```
library(ragR)

qa_log <- load_qa_log("db/qa_log.rds")
qa_metrics <- compute_ragas_metrics(qa_log)

summary_tbl <- summarize_ragas(qa_metrics)
print(summary_tbl)

plot_ragas_means(qa_metrics)
```

## 🧪 7. Testing

The project contains unit tests for chunking, vectorstore, logging, and RAGAS metrics.

Run:

```
devtools::test()
```

Run full package check:

```
devtools::check()
```

Expected output:

0 errors ✔ | 0 warnings ✔ | 3 notes ✖

___

## 🔌 API Endpoints

`ragR` exposes a Plumber-based HTTP API for use by any frontend  
(Shiny, JS web app, Python UI, desktop GUI, etc.).

Base URL when running locally: \
http://127.0.0.1:8000


Interactive Swagger Docs: \
http://127.0.0.1:8000/__docs__/



---

## 1. `POST /ingest`

Trigger ingestion of one or multiple files.

### **Request Body (JSON)**

```json
{
  "paths": ["data-raw/mydoc.pdf"],
  "collection": "default",
  "chunk_size": 500,
  "chunk_overlap": 50
}
```

### Response

```
{
  "status": "ok",
  "n_chunks": 12,
  "collection": "default"
}
```

### POST /chat

Send a question to the RAG pipeline.
Backend retrieves relevant chunks, constructs a prompt, calls OpenAI, and logs the Q/A.

### Request Body

{
  "question": "What is the document about?"
}

### Response

{
  "status": "ok",
  "qa_id": 7,
  "question": "What is the document about?",
  "answer": "This document discusses ...",
  "retrieved": [
    {"id": "demo_001", "text": "..."},
    {"id": "demo_002", "text": "..."}
  ]
}

The interaction is appended to: \
db/qa_log.rds

### POST /clear

Clear the vector store (memory reset).

### Response

{
  "status": "ok",
  "message": "Vector store cleared."
}

### POST /ragas/report

Compute RAGAS metrics for all stored Q/A pairs.

Generates:

- db/qa_metrics.rds
- reports/ragas/ragas_summary.csv
- reports/ragas/ragas_means.png

### Response

{
  "status": "ok",
  "n_qa": 12,
  "qa_metrics_path": "db/qa_metrics.rds",
  "summary_csv_path": "reports/ragas/ragas_summary.csv",
  "plot_path": "reports/ragas/ragas_means.png"
}

### POST /ragas/clear

Delete stored logs and RAGAS results.

### Response

{
  "status": "ok",
  "message": "QA log and RAGAS files cleared."
}

### Error Handling Format

All endpoints follow this error JSON format:

{
  "status": "error",
  "message": "Detailed error message."
}

### Dependency: OpenAI API Key

The backend requires: \
OPENAI_API_KEY=sk-xxxx

Stored in: \
```
~/.Renviron     (user-level)
```

The backend will not start without it.
