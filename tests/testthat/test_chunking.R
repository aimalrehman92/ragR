test_that("chunk_text produces non-empty chunks", {
  skip_if_not(exists("chunk_text"), "chunk_text() not defined")

  text <- paste(rep(letters, each = 10), collapse = " ")

  chunks <- chunk_text(
    text,
    chunk_size    = 100,
    chunk_overlap = 20
  )

  # Accept either a character vector, or a data frame / tibble with a 'text' column
  if (is.character(chunks)) {
    expect_true(length(chunks) >= 1)
    expect_true(all(nchar(chunks) > 0))
  } else if (is.data.frame(chunks) || tibble::is_tibble(chunks)) {
    expect_true(nrow(chunks) >= 1)
    expect_true(all(nchar(chunks$text) > 0))
  } else {
    fail("chunk_text() returned an unsupported type")
  }
})
