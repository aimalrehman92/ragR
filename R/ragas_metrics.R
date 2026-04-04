# R/ragas_metrics.R

#' Create an empty QA metrics tibble
#'
#' This helper creates an empty tibble with the columns used to store
#' RAGAS-style evaluation metrics for each QA interaction.
#'
#' All metric values are expected to be in \eqn{[0,1]}.
#'
#' @return A tibble with zero rows and the standard QA metrics columns.
#' @export
qa_metrics_empty <- function() {
  tibble::tibble(
    qa_id             = integer(),
    context_precision = numeric(),
    context_recall    = numeric(),
    answer_relevance  = numeric(),
    faithfulness      = numeric(),
    ragas_overall     = numeric()
  )
}

# ---- APPROX / PROXY METRICS --------------------------------------------------

#' Compute RAGAS-style metrics (deterministic, reference-based)
#'
#' Computes deterministic lexical proxies using token overlap / Jaccard.
#' This mode assumes a reference (ground-truth) answer is available for each
#' interaction via the `answer_reference` column in the QA log.
#'
#' @param qa_log A tibble created and populated by [log_rag_interaction()].
#'
#' @return A tibble with one row per `qa_id` and metric columns.
#' @export
compute_ragas_metrics_approx <- function(qa_log) {
  if (is.null(qa_log) || nrow(qa_log) == 0L) {
    return(qa_metrics_empty())
  }

  required_cols <- c("qa_id", "question", "answer_model", "answer_reference", "retrieved_texts")
  missing_cols <- setdiff(required_cols, names(qa_log))
  if (length(missing_cols) > 0L) {
    stop(
      "qa_log is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  tokenize <- function(x) {
    if (is.null(x) || is.na(x) || !nzchar(x)) return(character(0L))
    toks <- unlist(strsplit(tolower(x), "[^[:alnum:]]+"))
    toks[nzchar(toks)]
  }

  metric_rows <- lapply(seq_len(nrow(qa_log)), function(i) {
    qa_id  <- qa_log$qa_id[i]
    q_text <- qa_log$question[i]
    a_text <- qa_log$answer_model[i]
    g_text <- qa_log$answer_reference[i]

    ctx_vec <- character(0L)
    if (!is.null(qa_log$retrieved_texts) && length(qa_log$retrieved_texts) >= i) {
      ctx_vec <- unlist(qa_log$retrieved_texts[[i]])
    }
    ctx_text <- paste(ctx_vec, collapse = " ")

    q_tokens <- unique(tokenize(q_text))
    a_tokens <- unique(tokenize(a_text))
    g_tokens <- unique(tokenize(g_text))
    c_tokens <- unique(tokenize(ctx_text))

    if (length(c_tokens) == 0L || length(g_tokens) == 0L) {
      context_precision <- 0
    } else {
      context_precision <- length(intersect(c_tokens, g_tokens)) / length(c_tokens)
    }

    if (length(g_tokens) == 0L || length(c_tokens) == 0L) {
      context_recall <- 0
    } else {
      context_recall <- length(intersect(c_tokens, g_tokens)) / length(g_tokens)
    }

    if (length(a_tokens) == 0L || length(c_tokens) == 0L) {
      faithfulness <- 0
    } else {
      faithfulness <- length(intersect(a_tokens, c_tokens)) / length(a_tokens)
    }

    if (length(a_tokens) == 0L && length(g_tokens) == 0L) {
      answer_relevance <- 0
    } else {
      inter_ag <- length(intersect(a_tokens, g_tokens))
      union_ag <- length(union(a_tokens, g_tokens))
      answer_relevance <- if (union_ag == 0L) 0 else inter_ag / union_ag
    }

    ragas_overall <- mean(c(context_precision, context_recall, answer_relevance, faithfulness))

    tibble::tibble(
      qa_id             = as.integer(qa_id),
      context_precision = context_precision,
      context_recall    = context_recall,
      answer_relevance  = answer_relevance,
      faithfulness      = faithfulness,
      ragas_overall     = ragas_overall
    )
  })

  dplyr::bind_rows(metric_rows)
}

# ---- LLM-BASED STRUCTURED METRICS --------------------------------------------

# Internal: compact null-coalescing helper
`%||%` <- function(x, y) if (is.null(x)) y else x

extract_json_block <- function(text) {

  # Step 1: handle ```json blocks correctly
  md_idx <- regexpr("```json", text, fixed = TRUE)
  if (md_idx != -1) {
    text <- substr(text, md_idx + 7, nchar(text))
  }

  # remove trailing ```
  text <- sub("```$", "", text)

  # Step 2: find first { or [
  left_bracket <- regexpr("\\[", text)
  left_brace   <- regexpr("\\{", text)

  candidates <- c(left_bracket, left_brace)
  candidates <- candidates[candidates > 0]

  if (length(candidates) == 0) {
    return(text)
  }

  start_idx <- min(candidates)
  open_char <- substr(text, start_idx, start_idx)
  close_char <- ifelse(open_char == "[", "]", "}")

  # Step 3: bracket matching
  count <- 0

  for (i in seq(start_idx, nchar(text))) {
    char <- substr(text, i, i)

    if (char == open_char) count <- count + 1
    if (char == close_char) count <- count - 1

    if (count == 0) {
      return(substr(text, start_idx, i))
    }
  }

  return(text)
}

# create a log file path (once)
log_file <- "debug_llm_outputs.txt"

ragas_generate_json <- function(prompt, model = "gpt-4o-mini", seed = NULL, temperature = 0, max_retries = 3L) {

  last_raw <- NULL

  for (attempt in seq_len(max_retries)) {

    raw <- generate_openai_chat(
      prompt = prompt,
      model = model,
      temperature = temperature,
      seed = seed
    )

    last_raw <- raw

    # 🔥 Step 1: extract JSON STRING (not parsed)
    json_str <- extract_json_block(raw)

    # 🔥 Step 2: parse JSON safely
    parsed <- tryCatch(
      jsonlite::fromJSON(json_str, simplifyVector = FALSE),
      error = function(e) NULL
    )

    # 🔥 Step 3: only accept valid structured output
    if (!is.null(parsed)) {
      return(parsed)
    }
  }

  stop(
    "Failed to parse JSON. Last response:\n",
    substr(last_raw %||% "", 1, 1000),
    call. = FALSE
  )
}


# Internal: build a PydanticPrompt-style string from instruction/schema/examples/input
ragas_prompt_string <- function(instruction, output_schema, input_data, examples = list()) {
  example_block <- ""
  if (length(examples) > 0L) {
    parts <- lapply(seq_along(examples), function(idx) {
      ex <- examples[[idx]]
      paste0(
        "Example ", idx, "\n",
        "Input: ", jsonlite::toJSON(ex$input, auto_unbox = TRUE, pretty = TRUE, null = "null"), "\n",
        "Output: ", jsonlite::toJSON(ex$output, auto_unbox = TRUE, pretty = TRUE, null = "null")
      )
    })
    example_block <- paste0("\n--------EXAMPLES-----------\n", paste(parts, collapse = "\n\n"))
  }

  paste0(
    instruction, "\n",
    "Please return the output in a JSON format that complies with the following schema as specified in JSON Schema:\n",
    jsonlite::toJSON(output_schema, auto_unbox = TRUE, pretty = FALSE, null = "null"),
    "\nDo not use single quotes in your response but double quotes, properly escaped with a backslash.",
    example_block,
    "\n-----------------------------\n",
    "\nNow perform the same with the following input\n",
    "input: ", jsonlite::toJSON(input_data, auto_unbox = TRUE, pretty = TRUE, null = "null"), "\n",
    "Output: "
  )
}

# Internal: JSON schema helper for repeated prompt definitions
json_schema_object <- function(properties, required = names(properties)) {
  list(
    type = "object",
    properties = properties,
    required = unname(required)
  )
}

json_schema_array <- function(item_schema) {
  list(type = "array", items = item_schema)
}

# Internal: minimal sentence splitter used for context recall fallback/cleanup
split_sentences_basic <- function(text) {
  if (is.null(text) || !nzchar(trimws(text))) return(character(0))
  pieces <- unlist(strsplit(gsub("\\s+", " ", text), "(?<=[.!?])\\s+", perl = TRUE))
  pieces <- trimws(pieces)
  pieces[nzchar(pieces)]
}

# Internal: cosine similarity helpers for answer relevance
ragas_normalize_vec <- function(x) {
  norm <- sqrt(sum(x * x))
  if (!is.finite(norm) || norm == 0) return(rep(0, length(x)))
  x / norm
}

ragas_cosine_similarity <- function(a, b) {
  a <- ragas_normalize_vec(as.numeric(a))
  b <- ragas_normalize_vec(as.numeric(b))
  sum(a * b)
}

# Internal: embedding wrapper. Tries package helpers first and otherwise errors clearly.
ragas_embed_texts <- function(texts, model = "text-embedding-3-small") {
  if (!is.character(texts)) stop("texts must be a character vector", call. = FALSE)
  if (length(texts) == 0L) return(list())

  embed_many <- get0("get_openai_embeddings", mode = "function", inherits = TRUE)
  if (is.function(embed_many)) {
    emb <- embed_many(texts = texts, model = model)
    return(emb)
  }

  embed_one <- get0("get_openai_embedding", mode = "function", inherits = TRUE)
  if (is.function(embed_one)) {
    emb <- lapply(texts, function(txt) embed_one(text = txt, model = model))
    return(emb)
  }

  stop(
    "No embedding helper found. Please expose get_openai_embeddings(texts, model) or generate_openai_embedding(text, model) in your package.",
    call. = FALSE
  )
}

# Internal: generate multiple structured outputs, matching RAGAS generate_multiple behavior
ragas_generate_multiple <- function(prompt, n, model = "gpt-4o-mini", seed = NULL, temperature = 0, max_retries = 3L) {
  out <- vector("list", n)
  for (i in seq_len(n)) {
    out[[i]] <- ragas_generate_json(
      prompt = prompt,
      model = model,
      seed = if (is.null(seed)) NULL else as.integer(seed) + i - 1L,
      temperature = temperature,
      max_retries = max_retries
    )
  }
  out
}

# Internal: majority-vote discrete ensemble mirroring RAGAS discrete ensembling use case
ensemble_binary_verdict <- function(parsed_list, field = "verdict") {
  vals <- vapply(parsed_list, function(x) {
    val <- x[[field]] %||% NA
    suppressWarnings(as.integer(val))
  }, integer(1))
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0L) return(NA_integer_)
  as.integer(mean(vals) >= 0.5)
}

# ---- Faithfulness -------------------------------------------------------------

faithfulness_statement_generator_prompt <- function(question, answer, model = "gpt-4o-mini", seed = NULL) {

  instruction <- paste(
    "Given a question and an answer, analyze the complexity of each sentence in the answer.",
    "Break down each sentence into one or more fully understandable statements.",
    "Ensure that no pronouns are used in any statement.",
    "Do NOT simplify statements.",
    "Do NOT remove nuanced or partial claims.",
    "Do NOT skip any information present in the answer.",
    "Each statement must reflect the original meaning exactly.",
    "Format the outputs in JSON."
  )

  schema <- json_schema_object(list(
    statements = list(
      type = "array",
      items = list(type = "string")
    )
  ))

  examples <- list(list(
    input = list(
      question = "Who was Albert Einstein and what is he best known for?",
      answer = paste(
        "He was a German-born theoretical physicist, widely acknowledged to be one of the greatest and most influential physicists of all time.",
        "He was best known for developing the theory of relativity, he also made important contributions to the development of the theory of quantum mechanics."
      )
    ),
    output = list(
      statements = list(
        "Albert Einstein was a German-born theoretical physicist.",
        "Albert Einstein was widely acknowledged to be one of the greatest and most influential physicists of all time.",
        "Albert Einstein was best known for developing the theory of relativity.",
        "Albert Einstein also made important contributions to the development of the theory of quantum mechanics."
      )
    )
  ))

  prompt <- ragas_prompt_string(
    instruction = instruction,
    output_schema = schema,
    input_data = list(question = question, answer = answer),
    examples = examples
  )

  parsed <- ragas_generate_json(prompt, model = model, seed = seed)

  if (is.null(parsed) || is.null(parsed$statements)) {
    return(character(0))
  }

  statements <- parsed$statements
  statements <- unname(as.character(unlist(statements)))
  statements <- statements[nzchar(statements)]

  return(statements)
}


faithfulness_nli_prompt <- function(contexts, statements, model = "gpt-4o-mini", seed = NULL) {

  instruction <- paste(
    "Your task is to judge the faithfulness of a series of statements based on a given context.",
    "For each statement you must return verdict as 1 if the statement can be directly inferred based on the context or 0 if the statement can not be directly inferred based on the context."
  )

  statement_schema <- json_schema_object(list(
    statement = list(type = "string"),
    reason = list(type = "string"),
    verdict = list(type = "integer")
  ))

  schema <- json_schema_object(list(
    statements = json_schema_array(statement_schema)
  ))

  examples <- list(
    list(
      input = list(
        context = paste(
          "John is a student at XYZ University. He is pursuing a degree in Computer Science.",
          "He is enrolled in Data Structures, Algorithms, and Database Management.",
          "John studies extensively and works late in the library."
        ),
        statements = list(
          "John is majoring in Biology.",
          "John is taking Artificial Intelligence.",
          "John studies extensively.",
          "John has a part-time job."
        )
      ),
      output = list(
        statements = list(
          list(statement = "John is majoring in Biology.", reason = "Not in context.", verdict = 0),
          list(statement = "John is taking Artificial Intelligence.", reason = "Not in context.", verdict = 0),
          list(statement = "John studies extensively.", reason = "Explicitly stated.", verdict = 1),
          list(statement = "John has a part-time job.", reason = "Not in context.", verdict = 0)
        )
      )
    )
  )

  prompt <- ragas_prompt_string(
    instruction = instruction,
    output_schema = schema,
    input_data = list(
      context = paste(contexts, collapse = "\n"),
      statements = as.list(statements)
    ),
    examples = examples
  )

  parsed <- ragas_generate_json(prompt, model = model, seed = seed)

  if (is.null(parsed) || is.null(parsed$statements)) {
    return(list())
  }

  out <- parsed$statements

  lapply(out, function(x) list(
    statement = as.character(x$statement %||% ""),
    reason = as.character(x$reason %||% ""),
    verdict = suppressWarnings(as.integer(x$verdict %||% NA_integer_))
  ))
}

compute_faithfulness_ragas <- function(question, answer, contexts, model = "gpt-4o-mini", seed = NULL) {

  # Step 1: generate statements
  statements <- faithfulness_statement_generator_prompt(
    question = question,
    answer = answer,
    model = model,
    seed = seed
  )

  if (length(statements) == 0L) {
    return(NA_real_)
  }

  # Step 2: get NLI outputs
  verdicts <- faithfulness_nli_prompt(
    contexts = contexts,
    statements = statements,
    model = model,
    seed = if (is.null(seed)) NULL else seed + 1000L
  )

  if (length(verdicts) == 0L) {
    return(NA_real_)
  }

  # 🔥 CRITICAL: mimic Python exactly
  # denominator = length of returned verdict objects

  faithful_count <- sum(vapply(verdicts, function(x) {
    val <- suppressWarnings(as.numeric(x$verdict))
    if (is.na(val)) return(0)
    if (val == 1) return(1)
    0
  }, numeric(1)))

  num_statements <- length(verdicts)

  if (num_statements == 0L) {
    return(NA_real_)
  }

  faithful_count / num_statements
}

# ---- Context Precision --------------------------------------------------------

context_precision_single_verification <- function(question, context, answer, model = "gpt-4o-mini", seed = NULL) {
  instruction <- paste(
    'Given question, answer and context verify if the context was useful in arriving at the given answer.',
    'Give verdict as "1" if useful and "0" if not with json output.'
  )

  schema <- json_schema_object(list(
    reason = list(type = "string", description = "Reason for verification"),
    verdict = list(type = "integer", description = "Binary (0/1) verdict of verification")
  ))

  examples <- list(
    list(
      input = list(
        question = "What can you tell me about Albert Einstein?",
        context = paste(
          "Albert Einstein (14 March 1879 - 18 April 1955) was a German-born theoretical physicist, widely held to be one of the greatest and most influential scientists of all time.",
          "Best known for developing the theory of relativity, he also made important contributions to quantum mechanics, and was thus a central figure in the revolutionary reshaping of the scientific understanding of nature that modern physics accomplished in the first decades of the twentieth century.",
          "He received the 1921 Nobel Prize in Physics 'for his services to theoretical physics, and especially for his discovery of the law of the photoelectric effect', a pivotal step in the development of quantum theory."
        ),
        answer = "Albert Einstein, born on 14 March 1879, was a German-born theoretical physicist, widely held to be one of the greatest and most influential scientists of all time. He received the 1921 Nobel Prize in Physics for his services to theoretical physics."
      ),
      output = list(
        reason = "The provided context was indeed useful in arriving at the given answer. The context includes key information about Albert Einstein's life and contributions, which are reflected in the answer.",
        verdict = 1
      )
    ),
    list(
      input = list(
        question = "who won 2020 icc world cup?",
        context = paste(
          "The 2022 ICC Men's T20 World Cup, held from October 16 to November 13, 2022, in Australia, was the eighth edition of the tournament.",
          "Originally scheduled for 2020, it was postponed due to the COVID-19 pandemic.",
          "England emerged victorious, defeating Pakistan by five wickets in the final to clinch their second ICC Men's T20 World Cup title."
        ),
        answer = "England"
      ),
      output = list(
        reason = "the context was useful in clarifying the situation regarding the 2020 ICC World Cup and indicating that England was the winner of the tournament that was intended to be held in 2020 but actually took place in 2022.",
        verdict = 1
      )
    ),
    list(
      input = list(
        question = "What is the tallest mountain in the world?",
        context = paste(
          "The Andes is the longest continental mountain range in the world, located in South America.",
          "It stretches across seven countries and features many of the highest peaks in the Western Hemisphere.",
          "The range is known for its diverse ecosystems, including the high-altitude Andean Plateau and the Amazon rainforest."
        ),
        answer = "Mount Everest."
      ),
      output = list(
        reason = "the provided context discusses the Andes mountain range, which, while impressive, does not include Mount Everest or directly relate to the question about the world's tallest mountain.",
        verdict = 0
      )
    )
  )

  prompt <- ragas_prompt_string(
    instruction = instruction,
    output_schema = schema,
    input_data = list(question = question, context = context, answer = answer),
    examples = examples
  )

  # Python uses generate_multiple and ensembler.from_discrete over repeated verdicts.
  parsed_list <- ragas_generate_multiple(prompt, n = 3L, model = model, seed = seed)
  verdict <- ensemble_binary_verdict(parsed_list, field = "verdict")
  list(
    reason = as.character(parsed_list[[1]]$reason %||% ""),
    verdict = verdict
  )
}

compute_context_precision_ragas <- function(question, answer_or_reference, contexts, model = "gpt-4o-mini", seed = NULL) {
  if (is.null(contexts) || length(contexts) == 0L) {
    return(NA_real_)
  }

  # Step 1: Get verification results per context
  verifications <- lapply(seq_along(contexts), function(i) {
    context_precision_single_verification(
      question = question,
      context = contexts[[i]],
      answer = answer_or_reference,
      model = model,
      seed = if (is.null(seed)) NULL else seed + i * 10L
    )
  })

  # Step 2: Extract verdicts safely
  verdicts <- vapply(verifications, function(x) {
    if (is.null(x) || is.null(x$verdict) || is.na(x$verdict)) {
      return(0)
    }
    as.numeric(x$verdict)
  }, numeric(1))

  # Step 3: Handle invalid case
  if (length(verdicts) == 0 || all(is.na(verdicts))) {
    return(NA_real_)
  }

  # Step 4: Compute Average Precision (RAGAS style)
  relevant_positions <- which(verdicts == 1)

  if (length(relevant_positions) == 0L) {
    return(0)
  }

  precision_at_k <- sapply(seq_along(verdicts), function(k) {
    sum(verdicts[1:k]) / k
  })

  mean(precision_at_k[relevant_positions])
}


# ---- Context Recall -----------------------------------------------------------

context_recall_classification_prompt <- function(question, context, answer, model = "gpt-4o-mini", seed = NULL) {
  instruction <- paste(
    "Given a context, and an answer, analyze each sentence in the answer and classify if the sentence can be attributed to the given context or not.",
    "Use only 'Yes' (1) or 'No' (0) as a binary classification. Output json with reason."
  )

  item_schema <- json_schema_object(list(
    statement = list(type = "string"),
    reason = list(type = "string"),
    attributed = list(type = "integer")
  ))
  schema <- json_schema_object(list(classifications = json_schema_array(item_schema)))

  examples <- list(
    list(
      input = list(
        question = "What can you tell me about albert Albert Einstein?",
        context = paste(
          "Albert Einstein (14 March 1879 - 18 April 1955) was a German-born theoretical physicist, widely held to be one of the greatest and most influential scientists of all time.",
          "Best known for developing the theory of relativity, he also made important contributions to quantum mechanics, and was thus a central figure in the revolutionary reshaping of the scientific understanding of nature that modern physics accomplished in the first decades of the twentieth century.",
          "He received the 1921 Nobel Prize in Physics 'for his services to theoretical physics, and especially for his discovery of the law of the photoelectric effect', a pivotal step in the development of quantum theory.",
          "His work is also known for its influence on the philosophy of science."
        ),
        answer = paste(
          "Albert Einstein, born on 14 March 1879, was a German-born theoretical physicist, widely held to be one of the greatest and most influential scientists of all time.",
          "He received the 1921 Nobel Prize in Physics for his services to theoretical physics.",
          "He published 4 papers in 1905."
        )
      ),
      output = list(
        classifications = list(
          list(statement = "Albert Einstein, born on 14 March 1879, was a German-born theoretical physicist, widely held to be one of the greatest and most influential scientists of all time.", reason = "This sentence is directly supported by the context.", attributed = 1),
          list(statement = "He received the 1921 Nobel Prize in Physics for his services to theoretical physics.", reason = "This sentence is directly supported by the context.", attributed = 1),
          list(statement = "He published 4 papers in 1905.", reason = "The context does not mention this claim.", attributed = 0)
        )
      )
    )
  )

  prompt <- ragas_prompt_string(
    instruction = instruction,
    output_schema = schema,
    input_data = list(question = question, context = context, answer = answer),
    examples = examples
  )

  parsed <- ragas_generate_json(prompt, model = model, seed = seed)
  out <- parsed$classifications %||% list()
  lapply(out, function(x) list(
    statement = as.character(x$statement %||% ""),
    reason = as.character(x$reason %||% ""),
    attributed = suppressWarnings(as.integer(x$attributed %||% NA_integer_))
  ))
}

compute_context_recall_ragas <- function(
  question,
  answer,
  contexts,
  model = "gpt-4o-mini",
  seed = NULL
) {
  context <- paste(contexts, collapse = "\n")

  classifications <- context_recall_classification_prompt(
    question = question,
    context = context,
    answer = answer,
    model = model,
    seed = seed
  )

  if (is.null(classifications) || length(classifications) == 0L) {
    return(NA_real_)
  }

  vals <- vapply(classifications, function(x) {
    if (is.null(x$attributed)) return(NA_real_)
    suppressWarnings(as.numeric(x$attributed))
  }, numeric(1))

  # 🔥 Keep only valid parsed classifications
  valid_vals <- vals[!is.na(vals)]

  if (length(valid_vals) == 0L) {
    return(NA_real_)
  }
  mean(valid_vals)

}

# ---- Answer Relevance ---------------------------------------------------------

answer_relevance_question_generation_prompt <- function(response, model = "gpt-4o-mini", seed = NULL) {
  instruction <- paste(
    "Generate a question for the given answer and identify if the answer is noncommittal.",
    "A noncommittal answer is one that is evasive, vague, or ambiguous.",
    "For example, 'I don't know' or 'I'm not sure' are noncommittal answers.",
    "",
    "Return output in JSON format:",
    "{",
    "  \"question\": \"...\",",
    "  \"noncommittal\": 0 or 1",
    "}"
  )

  schema <- json_schema_object(list(
    question = list(type = "string"),
    noncommittal = list(type = "integer")
  ))

  examples <- list(
    list(
      input = list(response = "Albert Einstein was born in Germany."),
      output = list(
        question = "Where was Albert Einstein born?",
        noncommittal = 0
      )
    ),
    list(
      input = list(response = "I don't know."),
      output = list(
        question = "What is the answer to the question?",
        noncommittal = 1
      )
    )
  )

  prompt <- ragas_prompt_string(
    instruction = instruction,
    output_schema = schema,
    input_data = list(response = response),
    examples = examples
  )

  parsed_list <- ragas_generate_multiple(
    prompt,
    n = 3L,
    model = model,
    seed = seed
  )

  lapply(parsed_list, function(x) list(
    question = as.character(x$question %||% ""),
    noncommittal = suppressWarnings(as.integer(x$noncommittal %||% 0L))
  ))
}

compute_answer_relevance_ragas <- function(
  question,
  answer,
  model = "gpt-4o-mini",
  seed = NULL,
  embedding_model = "text-embedding-3-small",
  strictness = 3L
) {
  prompt_outputs <- answer_relevance_question_generation_prompt(
    response = answer,
    model = model,
    seed = seed
  )

  if (is.null(prompt_outputs) || length(prompt_outputs) == 0L) {
    return(0)
  }

  if (length(prompt_outputs) > strictness) {
    prompt_outputs <- prompt_outputs[seq_len(strictness)]
  }

  noncommittal_flags <- vapply(prompt_outputs, function(x) {
    isTRUE(as.integer(x$noncommittal %||% 0L) == 1L)
  }, logical(1))

  if (all(noncommittal_flags)) {
    return(0)
  }

  # 🔥 Python behavior: no filtering, no cleaning
  gen_questions <- vapply(prompt_outputs, function(x) {
    as.character(x$question %||% "")
  }, character(1))

  # 🔥 Embed question separately
  q_vec <- get_openai_embeddings(
    texts = question,
    model = embedding_model
  )[1, ]

  # 🔥 Embed generated questions one-by-one
  gen_vecs <- lapply(gen_questions, function(q) {
    get_openai_embeddings(
      texts = q,
      model = embedding_model
    )[1, ]
  })

  cosine_sim <- function(a, b) {
    da <- sqrt(sum(a * a))
    db <- sqrt(sum(b * b))
    if (da == 0 || db == 0) return(0)
    sum(a * b) / (da * db)
  }

  sims <- vapply(gen_vecs, function(vec) {
    cosine_sim(q_vec, vec)
  }, numeric(1))

  mean(sims, na.rm = TRUE)
}

# ---- Main LLM metric driver ---------------------------------------------------

#' Compute RAGAS-style metrics (LLM scored)
#'
#' This implementation mirrors the structured Python RAGAS workflow rather than
#' asking the judge model for one direct scalar score per metric.
#'
#' Notes:
#' - `context_precision` uses `answer_reference` if available; otherwise it uses
#'   `answer_model`, matching the Python with-reference / without-reference split.
#' - `answer_relevance` requires an embedding helper to be available in the
#'   package environment.
#'
#' @param qa_log A tibble created and populated by [log_rag_interaction()].
#' @param judge_model Character scalar; judge model name (default: "gpt-4o-mini").
#' @param seed Optional integer forwarded to judge model calls.
#' @param embedding_model Embedding model name used for answer relevance.
#' @param answer_relevance_strictness Number of generated reverse questions for
#'   answer relevance. Python RAGAS defaults to 3.
#'
#' @return A tibble with one row per `qa_id` and metric columns.
#' @export
compute_ragas_metrics_llm <- function(
  qa_log,
  judge_model = "gpt-4o-mini",
  seed = NULL,
  embedding_model = "text-embedding-3-small",
  answer_relevance_strictness = 3L
) {
  if (is.null(qa_log) || nrow(qa_log) == 0L) {
    return(qa_metrics_empty())
  }

  required_cols <- c("qa_id", "question", "answer_model", "retrieved_texts")
  missing_cols <- setdiff(required_cols, names(qa_log))
  if (length(missing_cols) > 0L) {
    stop(
      "qa_log is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  has_gt <- "answer_reference" %in% names(qa_log)

  rows <- lapply(seq_len(nrow(qa_log)), function(i) {
    qa_id  <- qa_log$qa_id[i]
    q      <- qa_log$question[i]
    a      <- qa_log$answer_model[i]
    ctx    <- unlist(qa_log$retrieved_texts[[i]])
    if (length(ctx) == 0L) ctx <- character(0)

    gt <- NULL
    if (has_gt) {
      gt_val <- qa_log$answer_reference[i]
      if (!is.na(gt_val) && nzchar(gt_val)) gt <- gt_val
    }
    cp_answer <- gt %||% a

    cat("\n==============================\n")
    cat("QUESTION:\n", q, "\n\n")

    cp <- tryCatch(
      compute_context_precision_ragas(
        question = q,
        answer_or_reference = cp_answer,
        contexts = ctx,
        model = judge_model,
        seed = if (is.null(seed)) NULL else seed + i * 1000L
      ),
      error = function(e) NA_real_
    )
    cat("CP:", cp, "\n")

    cr <- tryCatch(
      compute_context_recall_ragas(
        question = q,
        answer = gt %||% a,
        contexts = ctx,
        model = judge_model,
        seed = if (is.null(seed)) NULL else seed + i * 1000L + 100L
      ),
      error = function(e) NA_real_
    )
    cat("CR:", cr, "\n")

    ar <- tryCatch(
      compute_answer_relevance_ragas(
        question = q,
        answer = a,
        model = judge_model,
        seed = if (is.null(seed)) NULL else seed + i * 1000L + 200L,
        embedding_model = embedding_model,
        strictness = as.integer(answer_relevance_strictness)
      ),
      error = function(e) NA_real_
    )
    cat("AR:", ar, "\n")

    fa <- tryCatch(
      compute_faithfulness_ragas(
        question = q,
        answer = a,
        contexts = ctx,
        model = judge_model,
        seed = if (is.null(seed)) NULL else seed + i * 1000L + 300L
      ),
      error = function(e) NA_real_
    )
    cat("FA:", fa, "\n")

    overall <- mean(c(cp, cr, ar, fa), na.rm = TRUE)
    if (is.nan(overall)) overall <- NA_real_

    tibble::tibble(
      qa_id             = as.integer(qa_id),
      context_precision = cp,
      context_recall    = cr,
      answer_relevance  = ar,
      faithfulness      = fa,
      ragas_overall     = overall
    )
  })

  dplyr::bind_rows(rows)
}

# ---- WRAPPER (MODE SWITCH) ---------------------------------------------------

#' Compute RAGAS metrics (mode switch)
#'
#' Convenience wrapper to compute either approximate proxy metrics or
#' LLM-scored metrics.
#'
#' @param qa_log QA log tibble.
#' @param mode One of `"approx"` or `"llm"`. If NULL, uses option
#'   `ragR.ragas_mode` (defaults to `"approx"`).
#' @param judge_model Judge model for `mode="llm"` (default: "gpt-4o-mini").
#' @param seed Optional integer. Forwarded to LLM-scored metrics when `mode="llm"`.
#' @param embedding_model Embedding model for answer relevance when `mode="llm"`.
#' @param answer_relevance_strictness Number of reverse questions generated for
#'   answer relevance when `mode="llm"`.
#'
#' @return A metrics tibble.
#' @export
compute_ragas_metrics <- function(
  qa_log,
  mode = NULL,
  judge_model = "gpt-4o-mini",
  seed = NULL,
  embedding_model = "text-embedding-3-small",
  answer_relevance_strictness = 3L
) {
  if (is.null(mode)) {
    mode <- getOption("ragR.ragas_mode", "approx")
  }
  mode <- match.arg(mode, choices = c("approx", "llm"))

  if (identical(mode, "approx")) {
    compute_ragas_metrics_approx(qa_log)
  } else {
    compute_ragas_metrics_llm(
      qa_log,
      judge_model = judge_model,
      seed = seed,
      embedding_model = embedding_model,
      answer_relevance_strictness = answer_relevance_strictness
    )
  }
}
