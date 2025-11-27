#' Summarize RAGAS-style metrics
#'
#' Given a tibble of QA metrics (as returned by [compute_ragas_metrics()]),
#' this function computes simple descriptive statistics (mean, standard
#' deviation, minimum, maximum) for each metric column.
#'
#' @param qa_metrics A tibble returned by [compute_ragas_metrics()], with
#'   columns `context_precision`, `context_recall`, `answer_relevance`,
#'   `faithfulness`, and `ragas_overall`.
#'
#' @return A tibble with one row per metric and columns:
#'   `metric`, `mean`, `sd`, `min`, `max`.
#' @export
summarize_ragas <- function(qa_metrics) {
  if (is.null(qa_metrics) || nrow(qa_metrics) == 0L) {
    return(
      tibble::tibble(
        metric = character(),
        mean   = numeric(),
        sd     = numeric(),
        min    = numeric(),
        max    = numeric()
      )
    )
  }

  metric_cols <- c(
    "context_precision",
    "context_recall",
    "answer_relevance",
    "faithfulness",
    "ragas_overall"
  )

  missing <- setdiff(metric_cols, names(qa_metrics))
  if (length(missing) > 0L) {
    stop(
      "qa_metrics is missing required metric columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  rows <- lapply(metric_cols, function(m) {
    v <- qa_metrics[[m]]

    tibble::tibble(
      metric = m,
      mean   = mean(v, na.rm = TRUE),
      sd     = stats::sd(v, na.rm = TRUE),
      min    = min(v, na.rm = TRUE),
      max    = max(v, na.rm = TRUE)
    )
  })

  dplyr::bind_rows(rows)
}

#' Plot distribution of overall RAGAS scores
#'
#' This convenience function draws a simple histogram of the `ragas_overall`
#' scores using base R graphics. It is intended for quick visual inspection
#' of how the overall scores are distributed.
#'
#' @param qa_metrics A tibble returned by [compute_ragas_metrics()].
#' @param breaks Number of breaks for the histogram; passed to [graphics::hist()].
#' @param main Optional main title for the plot.
#'
#' @return Invisibly, the result of [graphics::hist()].
#' @export
plot_ragas_overall <- function(
  qa_metrics,
  breaks = 10,
  main   = "Distribution of RAGAS overall scores"
) {
  if (is.null(qa_metrics) || nrow(qa_metrics) == 0L) {
    warning("qa_metrics is empty; nothing to plot.", call. = FALSE)
    return(invisible(NULL))
  }

  if (!"ragas_overall" %in% names(qa_metrics)) {
    stop("qa_metrics must have a 'ragas_overall' column.", call. = FALSE)
  }

  scores <- qa_metrics$ragas_overall
  scores <- scores[is.finite(scores)]

  if (length(scores) == 0L) {
    warning("No finite ragas_overall scores to plot.", call. = FALSE)
    return(invisible(NULL))
  }

  graphics::hist(
    scores,
    breaks = breaks,
    main   = main,
    xlab   = "RAGAS overall score",
    ylab   = "Frequency"
  )
}
