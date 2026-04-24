# inst/api/plumber.R

#* @apiTitle ragR API
#* @apiDescription RAG and LLM-scored RAGAS backend in R

# Get the directory where this file lives.
api_dir <- dirname(sys.frame(1)$ofile)

# RAG chatbot endpoints
source(file.path(api_dir, "pr_chat.R"))

# Clear/reset endpoints
source(file.path(api_dir, "pr_clear.R"))

# RAGAS evaluation endpoints
source(file.path(api_dir, "pr_ragas.R"))