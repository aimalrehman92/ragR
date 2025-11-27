test_that("qa_log_empty returns an empty tibble with expected columns", {
  log <- qa_log_empty()

  expect_true(tibble::is_tibble(log))
  expect_equal(nrow(log), 0L)

  expected_cols <- c(
    "qa_id",
    "question",
    "answer_model",
    "answer_reference",
    "collection",
    "retrieved_ids",
    "retrieved_texts",
    "chat_model",
    "embedding_model",
    "timestamp"
  )

  expect_equal(names(log), expected_cols)
})

test_that("log_rag_interaction initializes a new log and assigns qa_id = 1", {
  # Fake RAG result, mimicking query_rag() output shape
  rag_result <- list(
    answer    = "This is a test answer.",
    retrieved = tibble::tibble(
      id   = c("chunk1", "chunk2"),
      text = c("First chunk text.", "Second chunk text.")
    )
  )

  log <- log_rag_interaction(
    qa_log         = NULL,
    question       = "What is life?",
    rag_result     = rag_result,
    collection     = "demo_ingest_test",
    chat_model     = "gpt-4o-mini",
    embedding_model = "text-embedding-3-small"
  )

  expect_true(tibble::is_tibble(log))
  expect_equal(nrow(log), 1L)
  expect_equal(log$qa_id[1], 1L)
  expect_equal(log$question[1], "What is life?")
  expect_equal(log$answer_model[1], "This is a test answer.")
  expect_equal(log$collection[1], "demo_ingest_test")

  # retrieved_ids and retrieved_texts should be list-columns with length 1
  expect_true(is.list(log$retrieved_ids))
  expect_true(is.list(log$retrieved_texts))
  expect_equal(length(log$retrieved_ids[[1]]), 2L)
  expect_equal(length(log$retrieved_texts[[1]]), 2L)

  # timestamp should be POSIXct
  expect_s3_class(log$timestamp, "POSIXct")
})

test_that("log_rag_interaction appends rows and increments qa_id", {
  rag_result1 <- list(
    answer    = "First answer.",
    retrieved = tibble::tibble(
      id   = "chunk1",
      text = "Chunk one."
    )
  )

  rag_result2 <- list(
    answer    = "Second answer.",
    retrieved = tibble::tibble(
      id   = "chunk2",
      text = "Chunk two."
    )
  )

  log <- qa_log_empty()

  log <- log_rag_interaction(
    qa_log         = log,
    question       = "Q1?",
    rag_result     = rag_result1,
    collection     = "demo_ingest_test",
    chat_model     = "gpt-4o-mini",
    embedding_model = "text-embedding-3-small"
  )

  log <- log_rag_interaction(
    qa_log         = log,
    question       = "Q2?",
    rag_result     = rag_result2,
    collection     = "demo_ingest_test",
    chat_model     = "gpt-4o-mini",
    embedding_model = "text-embedding-3-small"
  )

  expect_equal(nrow(log), 2L)
  expect_equal(log$qa_id, c(1L, 2L))
  expect_equal(log$question, c("Q1?", "Q2?"))
  expect_equal(log$answer_model, c("First answer.", "Second answer."))
})
