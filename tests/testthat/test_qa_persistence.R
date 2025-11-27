test_that("save_qa_log and load_qa_log round-trip correctly", {
  # Build a tiny QA log
  log <- qa_log_empty()
  log <- dplyr::bind_rows(
    log,
    tibble::tibble(
      qa_id            = 1L,
      question         = "Test question?",
      answer_model     = "Test answer.",
      answer_reference = NA_character_,
      collection       = "demo",
      retrieved_ids    = list("c1"),
      retrieved_texts  = list("Some chunk text."),
      chat_model       = "gpt-4o-mini",
      embedding_model  = "text-embedding-3-small",
      timestamp        = Sys.time()
    )
  )

  tmp <- tempfile(fileext = ".rds")

  save_qa_log(log, path = tmp)
  log2 <- load_qa_log(path = tmp)

  expect_true(tibble::is_tibble(log2))
  expect_equal(nrow(log2), 1L)
  expect_equal(log2$qa_id[1], 1L)
  expect_equal(log2$question[1], "Test question?")
})

test_that("save_qa_metrics and load_qa_metrics round-trip correctly", {
  metrics <- qa_metrics_empty()
  metrics <- dplyr::bind_rows(
    metrics,
    tibble::tibble(
      qa_id             = 1L,
      context_precision = 0.5,
      context_recall    = 0.4,
      answer_relevance  = 0.6,
      faithfulness      = 0.5,
      ragas_overall     = 0.5
    )
  )

  tmp <- tempfile(fileext = ".rds")

  save_qa_metrics(metrics, path = tmp)
  metrics2 <- load_qa_metrics(path = tmp)

  expect_true(tibble::is_tibble(metrics2))
  expect_equal(nrow(metrics2), 1L)
  expect_equal(metrics2$qa_id[1], 1L)
  expect_equal(metrics2$ragas_overall[1], 0.5)
})

test_that("load_qa_log and load_qa_metrics return empty tibbles if files do not exist", {
  tmp <- tempfile(fileext = ".rds")  # does not exist yet

  log <- load_qa_log(path = tmp)
  metrics <- load_qa_metrics(path = tmp)

  expect_true(tibble::is_tibble(log))
  expect_equal(nrow(log), 0L)

  expect_true(tibble::is_tibble(metrics))
  expect_equal(nrow(metrics), 0L)
})
