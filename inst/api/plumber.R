# inst/api/plumber.R

#* @apiTitle ragR API
#* @apiDescription RAG and RAGAS backend in R (OpenAI + Chroma)

library(plumber)

# Create a new Plumber router and load endpoint files
pr <- pr() |>
  pr_set_serializer(serializer_json()) |>
  pr_load("pr_ingest.R") |>
  pr_load("pr_chat.R")
  # Later we can add:
  # |> pr_load("pr_clear.R")
  # |> pr_load("pr_ragas.R")

pr
