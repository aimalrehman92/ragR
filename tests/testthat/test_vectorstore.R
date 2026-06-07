# tests/testthat/test_vectorstore.R

testthat::test_that("vectorstore_upsert and vectorstore_query work together", {
  collection <- paste0(
    "test_collection_",
    format(Sys.time(), "%Y%m%d%H%M%OS3"),
    "_",
    sample.int(1e6, 1)
  )

  on.exit(
    ragR::vectorstore_delete_collection(collection),
    add = TRUE
  )

  ragR::vectorstore_delete_collection(collection)

  ids <- c("id1", "id2", "id3")
  docs <- c("apple banana", "banana cherry", "cherry date")

  embeddings <- matrix(
    c(
      1.0, 0.0,
      0.8, 0.2,
      0.0, 1.0
    ),
    nrow = 3,
    byrow = TRUE
  )

  metadatas <- list(
    list(path = "doc.txt", chunk_index = 1L),
    list(path = "doc.txt", chunk_index = 2L),
    list(path = "doc.txt", chunk_index = 3L)
  )

  testthat::expect_silent(
    ragR::vectorstore_upsert(
      collection = collection,
      ids = ids,
      embeddings = embeddings,
      documents = docs,
      metadatas = metadatas
    )
  )

  result <- ragR::vectorstore_query(
    collection = collection,
    query_embedding = c(1, 0),
    top_k = 2L
  )

  expected_cols <- c(
    "collection",
    "id",
    "text",
    "score",
    "metadata"
  )

  testthat::expect_true(tibble::is_tibble(result))
  testthat::expect_true(all(expected_cols %in% names(result)))
  testthat::expect_lte(nrow(result), 2L)
  testthat::expect_gte(nrow(result), 1L)
  testthat::expect_true(all(result$collection == collection))

  testthat::expect_equal(result$id[[1]], "id1")
  testthat::expect_true(all(is.finite(result$score)))
  testthat::expect_true(is.list(result$metadata))
})

testthat::test_that("vectorstore_delete_collection removes collection data", {
  collection <- paste0(
    "delete_me_",
    format(Sys.time(), "%Y%m%d%H%M%OS3"),
    "_",
    sample.int(1e6, 1)
  )

  on.exit(
    ragR::vectorstore_delete_collection(collection),
    add = TRUE
  )

  ragR::vectorstore_delete_collection(collection)

  ragR::vectorstore_upsert(
    collection = collection,
    ids = c("a", "b"),
    embeddings = matrix(
      c(
        1, 0,
        0, 1
      ),
      nrow = 2,
      byrow = TRUE
    ),
    documents = c("foo", "bar"),
    metadatas = list(list(), list())
  )

  result_before <- ragR::vectorstore_query(
    collection = collection,
    query_embedding = c(1, 0),
    top_k = 2L
  )

  testthat::expect_true(tibble::is_tibble(result_before))
  testthat::expect_gt(nrow(result_before), 0L)

  testthat::expect_silent(
    ragR::vectorstore_delete_collection(collection)
  )

  result_after <- ragR::vectorstore_query(
    collection = collection,
    query_embedding = c(1, 0),
    top_k = 2L
  )

  testthat::expect_true(tibble::is_tibble(result_after))
  testthat::expect_equal(nrow(result_after), 0L)
})
