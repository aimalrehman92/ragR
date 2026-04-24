# ragR: Retrieval-Augmented Generation and RAGAS Evaluation in R

`ragR` is an R package for building and evaluating Retrieval-Augmented Generation (RAG) workflows in R.

It provides:

- Document ingestion and chunking
- OpenAI-based embeddings and chat generation
- An R-native vector store backed by local RDS files
- A configurable RAG pipeline
- Persistent Q/A logging
- LLM-scored RAGAS-style evaluation metrics
- A minimal Plumber API backend for chatbot and evaluation frontends

The package is currently suitable for research, coursework, experimentation, and arXiv-level reproducibility. It is not yet prepared for CRAN submission.

---

## Features

### 1. Data ingestion

`ragR` can ingest document files, split them into chunks, generate embeddings, and store the chunks and embeddings in a local R-native vector store.

The vector store is saved locally as:

```text
db/vectorstore.rds
```

Supported file types depend on the available R and system dependencies. Plain text ingestion is the simplest path. PDF ingestion may require the `pdftools` R package and the Poppler system library.

Ingestion is currently handled through development scripts, not through the HTTP API.

---

### 2. Retrieval-Augmented Generation

For a user question, the RAG pipeline:

1. Embeds the question using an OpenAI embedding model.
2. Retrieves the most similar chunks from the local vector store.
3. Builds a grounded RAG prompt using the retrieved context.
4. Sends the grounded prompt to an OpenAI chat model.
5. Returns the answer, retrieved chunks, and final prompt.

The user-facing RAG function is:

```r
query_rag()
```

The API-level `system_prompt` is passed as a system message to the chat model. It is not inserted into the constructed RAG prompt.

---

### 3. Persistent Q/A logging

`ragR` can log RAG interactions into:

```text
db/qa_log.rds
```

Each logged row includes:

- `qa_id`
- `question`
- `prompt_final`
- `answer_model`
- `answer_reference`
- `collection`
- `retrieved_ids`
- `retrieved_texts`
- `chat_model`
- `embedding_model`
- `timestamp`

The `answer_reference` column is optional. It can be filled later for controlled evaluation experiments. If it is absent or empty, the RAGAS metric functions can fall back to `answer_model` where needed.

---

### 4. LLM-scored RAGAS-style metrics

`ragR` computes LLM-scored RAGAS-style metrics:

- Context Precision
- Context Recall
- Answer Relevance
- Faithfulness
- RAGAS Overall

The main metric functions are:

```r
compute_ragas_metrics()
compute_ragas_metrics_llm()
```

In the current package version, RAGAS evaluation is LLM-scored. Deterministic approximation metrics are not included in the package.

Metrics can be saved to:

```text
db/qa_metrics.rds
```

Reports can be written to:

```text
reports/ragas/
```

---

### 5. Minimal Plumber API backend

The package includes a minimal Plumber API for a chatbot/evaluation frontend.

Current API exposure:

| Endpoint | Method | Purpose |
|---|---:|---|
| `/chat` | POST | Ask a RAG question and log the interaction |
| `/ragas` | GET | Compute and return RAGAS metrics and summary |
| `/ragas/report` | POST | Compute metrics and save report artifacts |
| `/ragas/clear` | POST | Clear QA log, metrics, and report files |
| `/clear` | POST | Clear one vector-store collection |
| `/clear_all` | POST | Clear the entire vector store |

There is no ingestion endpoint in the current minimal API. Ingestion is handled through `scripts/dev/demo_ingest.R`.

---

## Project structure

```text
ragR/
├── DESCRIPTION
├── NAMESPACE
├── README.md
├── .Rbuildignore
├── .gitignore
├── R/
│   ├── api_handlers.R
│   ├── embeddings_dummy.R
│   ├── embeddings_openai.R
│   ├── ingestion_files.R
│   ├── qa_logging.R
│   ├── qa_persistence.R
│   ├── rag_pipeline.R
│   ├── ragR_overview.R
│   ├── ragas_metrics.R
│   ├── ragas_report.R
│   ├── ragas_summary.R
│   ├── utils.R
│   └── vectorstore_interface.R
├── inst/
│   └── api/
│       ├── plumber.R
│       ├── pr_chat.R
│       ├── pr_clear.R
│       └── pr_ragas.R
├── scripts/
│   └── dev/
│       ├── demo_ingest.R
│       ├── demo_rag.R
│       ├── run_api.R
│       └── qa/
│           ├── collect_qa.R
│           ├── compute_metrics.R
│           ├── run_eval_loop.R
│           ├── run_eval_pipeline.R
│           └── visualize_metrics.R
├── man/
└── tests/
```

Local runtime artifacts are usually stored in:

```text
db/
reports/ragas/
data-raw/
```

These folders are development/runtime folders and are typically excluded from package builds.

---

## Installation and setup

### 1. Clone the repository

```bash
git clone https://github.com/YOUR_USERNAME/ragR.git
cd ragR
```

Replace `YOUR_USERNAME` with the actual GitHub username or organization name.

---

### 2. Install development tools

For local development, install `devtools` if needed:

```r
install.packages("devtools")
```

Then load the package during development:

```r
devtools::load_all()
```

If you prefer to install the package locally:

```r
devtools::install()
```

---

### 3. Optional: restore the `renv` environment

If the project uses `renv`, you may restore the project environment with:

```r
install.packages("renv")
renv::restore()
```

Note: depending on your operating system, restoring all dependencies may require additional system libraries. In particular, PDF ingestion through `pdftools` may require Poppler.

---

## OpenAI API key

`ragR` reads the OpenAI API key from the environment variable:

```text
OPENAI_API_KEY
```

You can add it to your user-level `.Renviron` file:

```r
usethis::edit_r_environ("user")
```

Add:

```text
OPENAI_API_KEY=your_key_here
```

Then restart R.

---

## Usage from R

### 1. Ingest documents

Edit:

```text
scripts/dev/demo_ingest.R
```

Set the input files:

```r
input_paths <- c(
  "data-raw/my_document.txt"
)
```

Then run from the project root:

```bash
Rscript scripts/dev/demo_ingest.R
```

This creates or updates:

```text
db/vectorstore.rds
```

For PDF ingestion, use a PDF path such as:

```r
input_paths <- c(
  "data-raw/my_document.pdf"
)
```

PDF ingestion may require `pdftools` and Poppler.

---

### 2. Ask RAG questions without the API

Run:

```bash
Rscript scripts/dev/demo_rag.R
```

Or call the package function directly:

```r
library(ragR)

res <- query_rag(
  question          = "What is this document about?",
  collection        = "default",
  top_k             = 5L,
  embedding_model   = "text-embedding-3-small",
  chat_model        = "gpt-4o-mini",
  temperature       = 0,
  max_output_tokens = 300L,
  system_prompt     = "You are a helpful assistant."
)

cat(res$answer)
```

The result includes:

- `answer`
- `retrieved`
- `prompt`
- model metadata

---

### 3. Collect Q/A logs

To ask multiple questions and append the interactions to the QA log, edit and run:

```bash
Rscript scripts/dev/qa/collect_qa.R
```

This writes to:

```text
db/qa_log.rds
```

---

### 4. Compute RAGAS metrics from R

```r
library(ragR)

qa_log <- load_qa_log("db/qa_log.rds")

qa_metrics <- compute_ragas_metrics(
  qa_log,
  judge_model = "gpt-4o-mini",
  embedding_model = "text-embedding-3-small"
)

save_qa_metrics(qa_metrics, "db/qa_metrics.rds")

summary_tbl <- summarize_ragas(qa_metrics)
print(summary_tbl)
```

To create a plot:

```r
plot_ragas_means(
  summary_df  = summary_tbl,
  output_path = "reports/ragas/ragas_means.png"
)
```

---

### 5. Generate a RAGAS report from R

```r
library(ragR)

generate_ragas_report(
  qa_log_path     = "db/qa_log.rds",
  qa_metrics_path = "db/qa_metrics.rds",
  output_dir      = "reports/ragas",
  judge_model     = "gpt-4o-mini"
)
```

This writes:

```text
db/qa_metrics.rds
reports/ragas/ragas_summary.csv
reports/ragas/ragas_means.png
```

---

## Running the API backend

From the project root:

```bash
Rscript scripts/dev/run_api.R
```

The local backend runs at:

```text
http://127.0.0.1:8000
```

Interactive Swagger docs are available at:

```text
http://127.0.0.1:8000/__docs__/
```

---

## API examples

### POST `/chat`

Ask a RAG question and log the interaction.

Example request body:

```json
{
  "question": "What is the document about?",
  "collection": "default",
  "top_k": 5
}
```

Example R call:

```r
library(httr2)

resp <- request("http://127.0.0.1:8000/chat") |>
  req_method("POST") |>
  req_body_json(list(
    question = "What is the document about?",
    collection = "default",
    top_k = 5
  )) |>
  req_perform()

resp_body_json(resp, simplifyVector = TRUE)
```

The interaction is appended to:

```text
db/qa_log.rds
```

---

### GET `/ragas`

Compute RAGAS metrics from the saved QA log and return the metrics and summary.

```r
library(httr2)

resp <- request("http://127.0.0.1:8000/ragas") |>
  req_method("GET") |>
  req_perform()

resp_body_json(resp, simplifyVector = TRUE)
```

This endpoint can operate even when `answer_reference` is missing. In that case, the package falls back to `answer_model` where needed.

---

### POST `/ragas/report`

Compute RAGAS metrics and save report artifacts.

```r
library(httr2)

resp <- request("http://127.0.0.1:8000/ragas/report") |>
  req_method("POST") |>
  req_perform()

resp_body_json(resp, simplifyVector = TRUE)
```

This writes:

```text
db/qa_metrics.rds
reports/ragas/ragas_summary.csv
reports/ragas/ragas_means.png
```

---

### POST `/ragas/clear`

Clear the QA log, QA metrics, and generated report files.

```r
library(httr2)

resp <- request("http://127.0.0.1:8000/ragas/clear") |>
  req_method("POST") |>
  req_perform()

resp_body_json(resp, simplifyVector = TRUE)
```

---

### POST `/clear`

Clear one vector-store collection.

Example request body:

```json
{
  "collection": "default"
}
```

If `collection` is omitted, the handler may fall back to the default collection depending on the package-side handler.

---

### POST `/clear_all`

Clear the entire vector store across all collections.

```r
library(httr2)

resp <- request("http://127.0.0.1:8000/clear_all") |>
  req_method("POST") |>
  req_perform()

resp_body_json(resp, simplifyVector = TRUE)
```

---

## Development scripts

The `scripts/dev/` folder contains scripts for local experimentation and reproducibility.

Important scripts:

```text
scripts/dev/demo_ingest.R
scripts/dev/demo_rag.R
scripts/dev/run_api.R
scripts/dev/qa/collect_qa.R
scripts/dev/qa/compute_metrics.R
scripts/dev/qa/run_eval_pipeline.R
scripts/dev/qa/run_eval_loop.R
scripts/dev/qa/visualize_metrics.R
```

Some QA scripts are intended for controlled experiments where `answer_reference` has already been populated. The API workflow does not require `answer_reference`.

---

## Testing

Run package tests with:

```r
devtools::test()
```

Run package checks with:

```r
devtools::check()
```

For the current arXiv-ready version, the goal is to keep the package clean, loadable, and reproducible. Full CRAN-readiness is planned for a later stage.

---

## Notes on generated/local files

The following folders/files are usually local runtime artifacts and should not be committed unless intentionally needed for reproducibility:

```text
db/
reports/ragas/
data-raw/
```

Typical generated files include:

```text
db/vectorstore.rds
db/qa_log.rds
db/qa_metrics.rds
reports/ragas/ragas_summary.csv
reports/ragas/ragas_means.png
```

---

## Authors

- Muhammad Aimal Rehman
- Zhili Lu
- Chi-Kuang Yeh

---

## License

GPL-3
