# tests/testthat/test_qa_persistence.R

testthat::test_that("save_qa_log and load_qa_log round-trip correctly", {
  timestamp <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")

  log <- qa_log_empty()
  log <- dplyr::bind_rows(
    log,
    tibble::tibble(
      qa_id = 1L,
      question = "Test question?",
      prompt_final = "Prompt text.",
      answer_model = "Test answer.",
      answer_reference = NA_character_,
      collection = "demo",
      retrieved_ids = list(c("c1")),
      retrieved_texts = list(c("Some chunk text.")),
      chat_model = "gpt-4o-mini",
      embedding_model = "text-embedding-3-small",
      timestamp = timestamp
    )
  )

  tmp <- tempfile(fileext = ".rds")

  save_qa_log(log, path = tmp)
  log2 <- load_qa_log(path = tmp)

  testthat::expect_true(file.exists(tmp))
  testthat::expect_true(tibble::is_tibble(log2))
  testthat::expect_equal(nrow(log2), 1L)
  testthat::expect_equal(names(log2), names(log))

  testthat::expect_equal(log2$qa_id[[1]], 1L)
  testthat::expect_equal(log2$question[[1]], "Test question?")
  testthat::expect_equal(log2$prompt_final[[1]], "Prompt text.")
  testthat::expect_equal(log2$answer_model[[1]], "Test answer.")
  testthat::expect_true(is.na(log2$answer_reference[[1]]))
  testthat::expect_equal(log2$collection[[1]], "demo")
  testthat::expect_equal(log2$retrieved_ids[[1]], c("c1"))
  testthat::expect_equal(log2$retrieved_texts[[1]], c("Some chunk text."))
  testthat::expect_equal(log2$chat_model[[1]], "gpt-4o-mini")
  testthat::expect_equal(log2$embedding_model[[1]], "text-embedding-3-small")
  testthat::expect_equal(log2$timestamp[[1]], timestamp)
})

testthat::test_that("save_qa_metrics and load_qa_metrics round-trip correctly", {
  metrics <- qa_metrics_empty()
  metrics <- dplyr::bind_rows(
    metrics,
    tibble::tibble(
      qa_id = 1L,
      context_precision = 0.5,
      context_recall = 0.4,
      answer_relevance = 0.6,
      faithfulness = 0.5,
      ragas_overall = 0.5
    )
  )

  tmp <- tempfile(fileext = ".rds")

  save_qa_metrics(metrics, path = tmp)
  metrics2 <- load_qa_metrics(path = tmp)

  testthat::expect_true(file.exists(tmp))
  testthat::expect_true(tibble::is_tibble(metrics2))
  testthat::expect_equal(nrow(metrics2), 1L)
  testthat::expect_equal(names(metrics2), names(metrics))

  testthat::expect_equal(metrics2$qa_id[[1]], 1L)
  testthat::expect_equal(metrics2$context_precision[[1]], 0.5)
  testthat::expect_equal(metrics2$context_recall[[1]], 0.4)
  testthat::expect_equal(metrics2$answer_relevance[[1]], 0.6)
  testthat::expect_equal(metrics2$faithfulness[[1]], 0.5)
  testthat::expect_equal(metrics2$ragas_overall[[1]], 0.5)
})

testthat::test_that("load_qa_log and load_qa_metrics return empty tibbles if files do not exist", {
  missing_log_path <- tempfile(fileext = ".rds")
  missing_metrics_path <- tempfile(fileext = ".rds")

  testthat::expect_false(file.exists(missing_log_path))
  testthat::expect_false(file.exists(missing_metrics_path))

  log <- load_qa_log(path = missing_log_path)
  metrics <- load_qa_metrics(path = missing_metrics_path)

  testthat::expect_true(tibble::is_tibble(log))
  testthat::expect_equal(nrow(log), 0L)
  testthat::expect_equal(names(log), names(qa_log_empty()))

  testthat::expect_true(tibble::is_tibble(metrics))
  testthat::expect_equal(nrow(metrics), 0L)
  testthat::expect_equal(names(metrics), names(qa_metrics_empty()))
})
