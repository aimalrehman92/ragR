# inst/api/plumber.R

#* @apiTitle ragR API
#* @apiDescription RAG and RAGAS backend in R (OpenAI + Chroma)

# Get the directory where THIS file lives
api_dir <- dirname(sys.frame(1)$ofile)

source(file.path(api_dir, "pr_ingest.R"))
source(file.path(api_dir, "pr_chat.R"))
source(file.path(api_dir, "pr_clear.R"))
