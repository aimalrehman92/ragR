# R/embeddings_dummy.R

# Internal deterministic embedding generator for offline development/testing.
# Not exported; used for tests or when OpenAI is unavailable.
dummy_embeddings <- function(texts, dims = 32L) {
  texts <- as.character(texts)
  n <- length(texts)

  mat <- matrix(0, nrow = n, ncol = dims)

  for (i in seq_len(n)) {
    # stable-ish hash based on UTF-8 bytes
    bytes <- as.integer(charToRaw(enc2utf8(texts[i])))
    if (length(bytes) == 0L) next

    # repeat / fold into dims
    v <- rep(bytes, length.out = dims)
    v <- (v %% 97) / 97  # scale to [0, 1)
    mat[i, ] <- v
  }

  mat
}
