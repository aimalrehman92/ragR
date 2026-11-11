# tests/testthat/test_ragas_summary.R

testthat::test_that("summarize_ragas works with empty qa_metrics", {
  empty_metrics <- ragR::qa_metrics_empty()

  summary_tbl <- ragR::summarize_ragas(empty_metrics)

  testthat::expect_true(tibble::is_tibble(summary_tbl))
  testthat::expect_equal(nrow(summary_tbl), 0L)
  testthat::expect_equal(
    names(summary_tbl),
    c("metric", "mean", "sd", "min", "max")
  )
})

testthat::test_that("summarize_ragas returns expected summary for synthetic metrics", {
  qa_metrics <- tibble::tibble(
    qa_id = c(1L, 2L),
    context_precision = c(0.5, 1.0),
    context_recall = c(0.3, 0.6),
    answer_relevance = c(0.4, 0.2),
    faithfulness = c(0.5, 1.0),
    ragas_overall = c(0.425, 0.7)
  )

  summary_tbl <- ragR::summarize_ragas(qa_metrics)

  expected_metrics <- c(
    "context_precision",
    "context_recall",
    "answer_relevance",
    "faithfulness",
    "ragas_overall"
  )

  testthat::expect_true(tibble::is_tibble(summary_tbl))
  testthat::expect_equal(nrow(summary_tbl), length(expected_metrics))
  testthat::expect_equal(
    names(summary_tbl),
    c("metric", "mean", "sd", "min", "max")
  )
  testthat::expect_setequal(summary_tbl$metric, expected_metrics)

  testthat::expect_type(summary_tbl$metric, "character")
  testthat::expect_type(summary_tbl$mean, "double")
  testthat::expect_type(summary_tbl$sd, "double")
  testthat::expect_type(summary_tbl$min, "double")
  testthat::expect_type(summary_tbl$max, "double")

  for (metric in expected_metrics) {
    row <- summary_tbl[summary_tbl$metric == metric, ]

    testthat::expect_equal(nrow(row), 1L)
    testthat::expect_equal(row$mean, mean(qa_metrics[[metric]]))
    testthat::expect_equal(row$sd, stats::sd(qa_metrics[[metric]]))
    testthat::expect_equal(row$min, min(qa_metrics[[metric]]))
    testthat::expect_equal(row$max, max(qa_metrics[[metric]]))
  }
})

testthat::test_that("summarize_ragas handles missing metric values", {
  qa_metrics <- tibble::tibble(
    qa_id = c(1L, 2L, 3L),
    context_precision = c(0.5, NA_real_, 1.0),
    context_recall = c(NA_real_, NA_real_, NA_real_),
    answer_relevance = c(0.4, 0.2, NA_real_),
    faithfulness = c(0.5, 1.0, 0.0),
    ragas_overall = c(0.425, NA_real_, 0.7)
  )

  summary_tbl <- ragR::summarize_ragas(qa_metrics)

  testthat::expect_true(tibble::is_tibble(summary_tbl))
  testthat::expect_equal(nrow(summary_tbl), 5L)

  cp_row <- summary_tbl[summary_tbl$metric == "context_precision", ]
  testthat::expect_equal(cp_row$mean, mean(qa_metrics$context_precision, na.rm = TRUE))
  testthat::expect_equal(cp_row$min, min(qa_metrics$context_precision, na.rm = TRUE))
  testthat::expect_equal(cp_row$max, max(qa_metrics$context_precision, na.rm = TRUE))

  cr_row <- summary_tbl[summary_tbl$metric == "context_recall", ]
  testthat::expect_true(is.nan(cr_row$mean) || is.na(cr_row$mean))
  testthat::expect_true(is.infinite(cr_row$min) || is.na(cr_row$min))
  testthat::expect_true(is.infinite(cr_row$max) || is.na(cr_row$max))
})
