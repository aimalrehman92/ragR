# tests/testthat/test_qa_logging.R

testthat::test_that("qa_log_empty returns an empty tibble with expected columns", {
  log <- ragR::qa_log_empty()

  expected_cols <- c(
    "qa_id",
    "question",
    "prompt_final",
    "answer_model",
    "answer_reference",
    "collection",
    "retrieved_ids",
    "retrieved_texts",
    "chat_model",
    "embedding_model",
    "timestamp"
  )

  testthat::expect_true(tibble::is_tibble(log))
  testthat::expect_equal(nrow(log), 0L)
  testthat::expect_equal(names(log), expected_cols)
})

testthat::test_that("log_rag_interaction appends one row with expected fields", {
  qa_log <- ragR::qa_log_empty()

  timestamp <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")

  rag_result <- list(
    answer = "Hello",
    prompt = "SYSTEM...\n\nCONTEXT...\n\nQuestion...\n\nAnswer:",
    retrieved = tibble::tibble(
      collection = c("default", "default"),
      id = c("doc_0001", "doc_0002"),
      text = c("Chunk A", "Chunk B"),
      score = c(0.2, 0.1),
      metadata = list(
        list(path = "x"),
        list(path = "y")
      )
    )
  )

  out <- ragR::log_rag_interaction(
    qa_log = qa_log,
    question = "Test question?",
    rag_result = rag_result,
    collection = "default",
    chat_model = "gpt-4o-mini",
    embedding_model = "text-embedding-3-small",
    timestamp = timestamp
  )

  testthat::expect_true(tibble::is_tibble(out))
  testthat::expect_equal(nrow(out), 1L)

  testthat::expect_equal(out$qa_id[[1]], 1L)
  testthat::expect_equal(out$question[[1]], "Test question?")
  testthat::expect_equal(out$prompt_final[[1]], rag_result$prompt)
  testthat::expect_equal(out$answer_model[[1]], "Hello")
  testthat::expect_true(is.na(out$answer_reference[[1]]))
  testthat::expect_equal(out$collection[[1]], "default")
  testthat::expect_equal(out$chat_model[[1]], "gpt-4o-mini")
  testthat::expect_equal(out$embedding_model[[1]], "text-embedding-3-small")
  testthat::expect_equal(out$timestamp[[1]], timestamp)

  testthat::expect_true(is.list(out$retrieved_ids))
  testthat::expect_true(is.list(out$retrieved_texts))
  testthat::expect_equal(out$retrieved_ids[[1]], c("doc_0001", "doc_0002"))
  testthat::expect_equal(out$retrieved_texts[[1]], c("Chunk A", "Chunk B"))
})
