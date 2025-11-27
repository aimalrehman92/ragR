test_that("qa_metrics_empty returns an empty tibble with expected columns", {
  m <- qa_metrics_empty()

  expect_true(tibble::is_tibble(m))
  expect_equal(nrow(m), 0L)

  expected_cols <- c(
    "qa_id",
    "context_precision",
    "context_recall",
    "answer_relevance",
    "faithfulness",
    "ragas_overall"
  )

  expect_equal(names(m), expected_cols)
})

test_that("compute_ragas_metrics returns empty tibble for empty qa_log", {
  empty_log <- qa_log_empty()

  m <- compute_ragas_metrics(empty_log)

  expect_true(tibble::is_tibble(m))
  expect_equal(nrow(m), 0L)
})

test_that("compute_ragas_metrics computes finite scores for a simple qa_log", {
  # Build a tiny synthetic QA log with one row
  qa_log <- qa_log_empty()

  qa_log <- dplyr::bind_rows(
    qa_log,
    tibble::tibble(
      qa_id            = 1L,
      question         = "What fruit is mentioned?",
      answer_model     = "The text talks about apples and bananas.",
      answer_reference = NA_character_,
      collection       = "demo",
      retrieved_ids    = list(c("c1", "c2")),
      retrieved_texts  = list(c(
        "This chunk mentions apples.",
        "This chunk mentions bananas and cherries."
      )),
      chat_model       = "gpt-4o-mini",
      embedding_model  = "text-embedding-3-small",
      timestamp        = Sys.time()
    )
  )

  m <- compute_ragas_metrics(qa_log)

  expect_true(tibble::is_tibble(m))
  expect_equal(nrow(m), 1L)
  expect_equal(m$qa_id[1], 1L)

  # Metrics should be numeric and between 0 and 1
  numeric_cols <- c(
    "context_precision",
    "context_recall",
    "answer_relevance",
    "faithfulness",
    "ragas_overall"
  )

  for (col in numeric_cols) {
    expect_true(is.numeric(m[[col]]))
    expect_true(all(m[[col]] >= 0))
    expect_true(all(m[[col]] <= 1))
  }
})
