test_that("summarize_ragas works with empty qa_metrics", {
  empty_metrics <- qa_metrics_empty()
  summary_tbl <- summarize_ragas(empty_metrics)

  # Should return a tibble with 0 rows
  expect_true(tibble::is_tibble(summary_tbl))
  expect_equal(nrow(summary_tbl), 0L)

  # Should have correct column names
  expect_equal(
    names(summary_tbl),
    c("metric", "mean", "sd", "min", "max")
  )
})

test_that("summarize_ragas returns correct structure for synthetic metrics", {
  # Build small synthetic qa_metrics tibble
  qa_metrics <- tibble::tibble(
    qa_id             = c(1L, 2L),
    context_precision = c(0.5, 1.0),
    context_recall    = c(0.3, 0.6),
    answer_relevance  = c(0.4, 0.2),
    faithfulness      = c(0.5, 1.0),
    ragas_overall     = c(0.425, 0.7)
  )

  summary_tbl <- summarize_ragas(qa_metrics)

  # Should return 5 rows — one for each metric
  expect_true(tibble::is_tibble(summary_tbl))
  expect_equal(nrow(summary_tbl), 5L)

  # Correct columns
  expect_equal(
    names(summary_tbl),
    c("metric", "mean", "sd", "min", "max")
  )

  # Each metric name must be present
  expected_metrics <- c(
    "context_precision", "context_recall",
    "answer_relevance", "faithfulness", "ragas_overall"
  )
  expect_setequal(summary_tbl$metric, expected_metrics)

  # Values should be numeric
  expect_true(all(sapply(summary_tbl$mean, is.numeric)))
  expect_true(all(sapply(summary_tbl$sd, is.numeric)))
  expect_true(all(sapply(summary_tbl$min, is.numeric)))
  expect_true(all(sapply(summary_tbl$max, is.numeric)))

  # Means check (rough but meaningful)
  cp_mean <- mean(qa_metrics$context_precision)
  expect_equal(
    summary_tbl$mean[summary_tbl$metric == "context_precision"],
    cp_mean
  )
})
