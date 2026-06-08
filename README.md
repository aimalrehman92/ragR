# ragR: Retrieval-Augmented Generation and RAGAS Evaluation in R

`ragR` is an R package for building, running, logging, and evaluating
Retrieval-Augmented Generation (RAG) workflows in R.

It provides:

- document ingestion and chunking utilities,
- OpenAI-based embeddings and chat generation,
- an R-native vector store backed by local RDS files,
- a configurable RAG pipeline,
- persistent question-answer logging,
- LLM-scored RAGAS-style evaluation metrics, and
- a minimal Plumber API backend for chatbot and evaluation frontends.

The package is designed for research, coursework, experimentation, and
reproducible RAG evaluation workflows.

---

## Features

### Document ingestion

`ragR` can ingest documents, split text into chunks, generate embeddings, and
store chunks and embeddings in a local R-native vector store.

Supported file types depend on the available R and system dependencies. Plain
text ingestion is the simplest path. PDF ingestion may require the `pdftools` R
package and the Poppler system library.

Runtime vector-store files are created locally by the user, commonly at paths
such as:

```text
db/vectorstore.rds
```

These files are not part of the installed package.

### Retrieval-Augmented Generation

For a user question, the RAG pipeline:

1. embeds the question using an OpenAI embedding model,
2. retrieves the most similar chunks from the local vector store,
3. builds a grounded RAG prompt using the retrieved context,
4. sends the grounded prompt to an OpenAI chat model, and
5. returns the answer, retrieved chunks, and final prompt.

The primary user-facing RAG function is:

```r
query_rag()
```

The API-level `system_prompt` is passed as a system message to the chat model.
It is not inserted into the constructed RAG prompt.

### Persistent Q/A logging

`ragR` can log RAG interactions into a local RDS file, commonly:

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

The `answer_reference` column is optional. It can be filled later for controlled
evaluation experiments. If it is absent or empty, the RAGAS metric functions can
fall back to `answer_model` where needed.

### LLM-scored RAGAS-style metrics

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

In the current package version, RAGAS evaluation is LLM-scored. Deterministic
approximation metrics are not included.

Metric values may be `NA` when a score is not computable, when required inputs
are unavailable, or when external LLM/API scoring fails.

Metrics can be saved locally, commonly to:

```text
db/qa_metrics.rds
```

Reports can be written to a user-created directory, commonly:

```text
reports/ragas/
```

### Minimal Plumber API backend

The package includes a minimal Plumber API for chatbot and evaluation
frontends.

Current API exposure:

| Endpoint | Method | Purpose |
|---|---:|---|
| `/chat` | POST | Ask a RAG question and log the interaction |
| `/ragas` | GET | Compute and return RAGAS metrics and summary |
| `/ragas/report` | POST | Compute metrics and save report artifacts |
| `/ragas/clear` | POST | Clear QA log, metrics, and report files |
| `/clear` | POST | Clear one vector-store collection |
| `/clear_all` | POST | Clear the entire vector store |

There is no ingestion endpoint in the current minimal API.

---

## Installation

### From GitHub

```r
install.packages("remotes")
remotes::install_github("aimalrehman92/ragR")
```

### Local development installation

Clone the repository:

```bash
git clone https://github.com/aimalrehman92/ragR.git
cd ragR
```

Install development tools if needed:

```r
install.packages("devtools")
```

Load the package during development:

```r
devtools::load_all()
```

Or install it locally:

```r
devtools::install()
```

### Optional: restore the development environment

The GitHub repository may include development environment files. If using
`renv`, restore the environment with:

```r
install.packages("renv")
renv::restore()
```

Depending on your operating system, some dependencies may require additional
system libraries. In particular, PDF ingestion through `pdftools` may require
Poppler.

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

Most package tests use synthetic data and do not require an OpenAI API key.
Functions that call OpenAI models require a valid API key and internet access.

---

## Usage from R

### Ingest documents

```r
library(ragR)

input_paths <- c("data-raw/my_document.txt")

ingest_documents(
  paths = input_paths,
  collection = "default",
  vectorstore_path = "db/vectorstore.rds",
  embedding_model = "text-embedding-3-small"
)
```

This creates or updates a local vector store, commonly:

```text
db/vectorstore.rds
```

For PDF ingestion, use a PDF path such as:

```r
input_paths <- c("data-raw/my_document.pdf")
```

PDF ingestion may require `pdftools` and Poppler.

### Ask RAG questions

```r
library(ragR)

res <- query_rag(
  question = "What is this document about?",
  collection = "default",
  top_k = 5L,
  embedding_model = "text-embedding-3-small",
  chat_model = "gpt-4o-mini",
  temperature = 0,
  max_output_tokens = 300L,
  system_prompt = "You are a helpful assistant."
)

cat(res$answer)
```

The result includes:

- `answer`
- `retrieved`
- `prompt`
- model metadata

### Save and load Q/A logs

```r
library(ragR)

qa_log <- qa_log_empty()

# `res` is the result returned by query_rag()
qa_log <- log_rag_interaction(
  qa_log = qa_log,
  question = "What is this document about?",
  rag_result = res,
  collection = "default",
  chat_model = "gpt-4o-mini",
  embedding_model = "text-embedding-3-small"
)

save_qa_log(qa_log, path = "db/qa_log.rds")

qa_log <- load_qa_log(path = "db/qa_log.rds")
```

### Compute RAGAS metrics

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

### Generate a RAGAS report

```r
library(ragR)

generate_ragas_report(
  qa_log_path = "db/qa_log.rds",
  qa_metrics_path = "db/qa_metrics.rds",
  output_dir = "reports/ragas",
  judge_model = "gpt-4o-mini"
)
```

This writes local report artifacts such as:

```text
db/qa_metrics.rds
reports/ragas/ragas_summary.csv
reports/ragas/ragas_means.png
```

---

## Running the API backend

The installed package includes Plumber API files under `inst/api/`. To run the
API from an installed package:

```r
library(plumber)

api_file <- system.file("api", "plumber.R", package = "ragR")

pr <- plumber::pr(api_file)
plumber::pr_run(pr, host = "127.0.0.1", port = 8000)
```

The local backend runs at:

```text
http://127.0.0.1:8000
```

Interactive Swagger documentation is available at:

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

The interaction is appended to the configured Q/A log.

### GET `/ragas`

Compute RAGAS metrics from the saved Q/A log and return the metrics and summary.

```r
library(httr2)

resp <- request("http://127.0.0.1:8000/ragas") |>
  req_method("GET") |>
  req_perform()

resp_body_json(resp, simplifyVector = TRUE)
```

This endpoint can operate even when `answer_reference` is missing. In that case,
the package falls back to `answer_model` where needed.

### POST `/ragas/report`

Compute RAGAS metrics and save report artifacts.

```r
library(httr2)

resp <- request("http://127.0.0.1:8000/ragas/report") |>
  req_method("POST") |>
  req_perform()

resp_body_json(resp, simplifyVector = TRUE)
```

### POST `/ragas/clear`

Clear the Q/A log, Q/A metrics, and generated report files.

```r
library(httr2)

resp <- request("http://127.0.0.1:8000/ragas/clear") |>
  req_method("POST") |>
  req_perform()

resp_body_json(resp, simplifyVector = TRUE)
```

### POST `/clear`

Clear one vector-store collection.

Example request body:

```json
{
  "collection": "default"
}
```

If `collection` is omitted, the handler may fall back to the default collection
depending on the package-side handler.

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

The GitHub repository includes development scripts under `scripts/dev/` for
local experimentation and reproducibility. These scripts are intended for source
repository workflows and are not part of the installed package build.

Important development scripts include:

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

Some Q/A scripts are intended for controlled experiments where
`answer_reference` has already been populated. The API workflow does not require
`answer_reference`.

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
├── inst/
│   └── api/
├── man/
├── tests/
└── scripts/
    └── dev/
```

Local runtime artifacts are commonly stored in user-created folders such as:

```text
db/
reports/ragas/
data-raw/
```

These folders are development/runtime folders and are typically excluded from
package builds.

---

## Testing

Run package tests with:

```r
devtools::test()
```

Run package checks with:

```r
devtools::check(args = "--as-cran")
```

The package tests use synthetic data and avoid requiring an OpenAI API key or
internet access.

---

## Notes on generated/local files

The following folders/files are local runtime artifacts and should not be
committed unless intentionally needed for reproducibility:

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

## License

GPL-3
