# R/ragas_summary.R

#' Summarize RAGAS metrics across QA pairs
#'
#' Computes mean, standard deviation, minimum, and maximum for each metric column
#' in a QA-metrics tibble.
#'
#' @param qa_metrics A tibble returned by [compute_ragas_metrics()] or
#'   [compute_ragas_metrics_llm()] with metric columns such as
#'   `context_precision`, `context_recall`, `answer_relevance`, `faithfulness`,
#'   and `ragas_overall`.
#'
#' @return A tibble with columns: `metric`, `mean`, `sd`, `min`, `max`.
#' @export
summarize_ragas <- function(qa_metrics) {
  # Always return the same column structure, even if empty.
  empty_out <- tibble::tibble(
    metric = character(),
    mean   = numeric(),
    sd     = numeric(),
    min    = numeric(),
    max    = numeric()
  )

  if (is.null(qa_metrics) || nrow(qa_metrics) == 0L) {
    return(empty_out)
  }

  if (!is.data.frame(qa_metrics)) {
    stop("qa_metrics must be a data frame / tibble.", call. = FALSE)
  }

  metric_cols <- c(
    "context_precision",
    "context_recall",
    "answer_relevance",
    "faithfulness",
    "ragas_overall"
  )

  metric_cols <- metric_cols[metric_cols %in% names(qa_metrics)]

  if (length(metric_cols) == 0L) {
    stop("qa_metrics contains no known metric columns to summarize.", call. = FALSE)
  }

  rows <- lapply(metric_cols, function(m) {
    x <- suppressWarnings(as.numeric(qa_metrics[[m]]))

    if (length(x) == 0L || all(is.na(x))) {
      tibble::tibble(
        metric = m,
        mean   = NA_real_,
        sd     = NA_real_,
        min    = NA_real_,
        max    = NA_real_
      )
    } else {
      tibble::tibble(
        metric = m,
        mean   = mean(x, na.rm = TRUE),
        sd     = stats::sd(x, na.rm = TRUE),
        min    = min(x, na.rm = TRUE),
        max    = max(x, na.rm = TRUE)
      )
    }
  })

  dplyr::bind_rows(rows)
}

#' Plot mean RAGAS metrics to a PNG
#'
#' Creates a simple bar chart of mean metric values.
#'
#' @param summary_df Output of [summarize_ragas()].
#' @param output_path Where to save the PNG.
#' @param width,height Plot size in pixels.
#'
#' @return Invisibly, `output_path`.
#' @export
plot_ragas_means <- function(
  summary_df,
  output_path,
  width = 1200,
  height = 700
) {
  if (is.null(summary_df) || nrow(summary_df) == 0L) {
    stop("summary_df is empty; nothing to plot.", call. = FALSE)
  }

  if (!all(c("metric", "mean") %in% names(summary_df))) {
    stop("summary_df must have columns: metric, mean.", call. = FALSE)
  }

  means <- suppressWarnings(as.numeric(summary_df$mean))

  if (length(means) == 0L || all(is.na(means))) {
    stop("summary_df$mean contains no finite values to plot.", call. = FALSE)
  }

  names(means) <- as.character(summary_df$metric)

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

  # Base R plotting; avoids adding extra plotting dependencies.
  grDevices::png(filename = output_path, width = width, height = height, res = 150)
  on.exit(grDevices::dev.off(), add = TRUE)

  graphics::par(mar = c(9, 5, 3, 1) + 0.1)

  y_max <- max(1, max(means, na.rm = TRUE)) * 1.1

  bp <- graphics::barplot(
    height = means,
    las    = 2,
    ylim   = c(0, y_max),
    ylab   = "Mean score",
    main   = "RAGAS Metric Means"
  )

  graphics::axis(1, at = bp, labels = names(means), las = 2)
  graphics::grid(nx = NA, ny = NULL)

  invisible(output_path)
}