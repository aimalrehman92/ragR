test_that("vectorstore_upsert and vectorstore_query work together", {
  # Clean any existing test collection
  vectorstore_delete_collection("test_collection")

  # Fake embeddings: 3 vectors, 2-dimensional
  ids <- c("id1", "id2", "id3")
  docs <- c("apple banana", "banana cherry", "cherry date")
  embs <- matrix(c(
    1,   0,
    0.8, 0.2,
    0,   1
  ), nrow = 3, byrow = TRUE)

  metas <- list(
    list(path = "doc.txt", chunk_index = 1),
    list(path = "doc.txt", chunk_index = 2),
    list(path = "doc.txt", chunk_index = 3)
  )

  # Upsert into collection
  expect_silent(
    vectorstore_upsert(
      collection = "test_collection",
      ids        = ids,
      embeddings = embs,
      documents  = docs,
      metadatas  = metas
    )
  )

  # Query with a vector similar to first row
  q <- c(1, 0)
  res <- vectorstore_query(
    collection      = "test_collection",
    query_embedding = q,
    top_k           = 2
  )

  # Check basic structure
  expect_s3_class(res, "tbl_df")
  expect_true(nrow(res) <= 2)
  expect_true(all(res$collection == "test_collection"))

  # First result should be id1 (highest similarity)
  expect_equal(res$id[1], "id1")
})

test_that("vectorstore_delete_collection removes data", {
  # Insert small data
  ids <- c("a", "b")
  docs <- c("foo", "bar")
  embs <- matrix(c(1, 0, 0, 1), nrow = 2, byrow = TRUE)

  vectorstore_upsert(
    collection = "delete_me",
    ids        = ids,
    embeddings = embs,
    documents  = docs,
    metadatas  = replicate(2, list(), simplify = FALSE)
  )

  # Confirm something is there
  res_before <- vectorstore_query(
    collection      = "delete_me",
    query_embedding = c(1, 0),
    top_k           = 2
  )
  expect_true(nrow(res_before) > 0)

  # Delete collection
  expect_silent(vectorstore_delete_collection("delete_me"))

  # Now query should give 0 rows
  res_after <- vectorstore_query(
    collection      = "delete_me",
    query_embedding = c(1, 0),
    top_k           = 2
  )
  expect_equal(nrow(res_after), 0)
})
