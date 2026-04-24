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

# ---- LLM-BASED STRUCTURED METRICS --------------------------------------------

# Internal: compact null-coalescing helper
`%||%` <- function(x, y) if (is.null(x)) y else x

extract_json_block <- function(text) {
  if (is.null(text) || !is.character(text) || length(text) != 1L) {
    return(text)
  }

  md_idx <- regexpr("```json", text, fixed = TRUE)[1]
  if (!is.na(md_idx) && md_idx > 0L) {
    text <- substr(text, md_idx, nchar(text))
  }

  chars <- strsplit(text, "", fixed = TRUE)[[1]]

  left_bracket <- match("[", chars)
  left_brace   <- match("{", chars)

  if (!is.na(left_bracket) && !is.na(left_brace)) {
    start_idx <- min(left_bracket, left_brace)
  } else if (!is.na(left_bracket)) {
    start_idx <- left_bracket
  } else if (!is.na(left_brace)) {
    start_idx <- left_brace
  } else {
    return(text)
  }

  stack <- character(0)
  in_string <- FALSE
  escaped <- FALSE

  for (i in seq.int(start_idx, length(chars))) {
    ch <- chars[[i]]

    if (in_string) {
      if (escaped) {
        escaped <- FALSE
      } else if (identical(ch, "\\")) {
        escaped <- TRUE
      } else if (identical(ch, "\"")) {
        in_string <- FALSE
      }
      next
    }

    if (identical(ch, "\"")) {
      in_string <- TRUE
    } else if (ch %in% c("{", "[")) {
      stack <- c(stack, ch)
    } else if (ch %in% c("}", "]")) {
      if (length(stack) == 0L) {
        return(text)
      }

      last <- stack[[length(stack)]]
      ok <- (identical(last, "{") && identical(ch, "}")) ||
        (identical(last, "[") && identical(ch, "]"))

      if (!ok) {
        return(text)
      }

      stack <- stack[-length(stack)]

      if (length(stack) == 0L) {
        return(paste(chars[start_idx:i], collapse = ""))
      }
    }
  }

  text
}

ragas_generate_json <- function(
  prompt,
  model = "gpt-4o-mini",
  seed = NULL,
  temperature = 0,
  max_retries = 1L
) {
  raw <- generate_openai_chat(
    prompt = prompt,
    model = model,
    temperature = temperature,
    seed = seed
  )

  json_str <- extract_json_block(raw)
  parsed <- tryCatch(
    jsonlite::fromJSON(json_str, simplifyVector = FALSE),
    error = function(e) NULL
  )

  if (!is.null(parsed)) {
    return(parsed)
  }

  if (max_retries <= 0L) {
    stop(
      "Failed to parse JSON. Last response:\n",
      substr(raw %||% "", 1, 1000),
      call. = FALSE
    )
  }

  fix_prompt <- ragas_fix_output_format_prompt_string(
    output_string = raw,
    prompt_value = prompt
  )

  fixed_raw <- generate_openai_chat(
    prompt = fix_prompt,
    model = model,
    temperature = temperature,
    seed = seed
  )

  fixed_outer_json <- extract_json_block(fixed_raw)
  fixed_outer <- tryCatch(
    jsonlite::fromJSON(fixed_outer_json, simplifyVector = FALSE),
    error = function(e) NULL
  )

  fixed_text <- fixed_outer$text %||% NULL

  if (is.null(fixed_text) || !is.character(fixed_text) || length(fixed_text) != 1L) {
    stop(
      "Failed to parse JSON after FixOutputFormat. Last response:\n",
      substr(fixed_raw %||% "", 1, 1000),
      call. = FALSE
    )
  }

  parsed_fixed <- tryCatch(
    jsonlite::fromJSON(fixed_text, simplifyVector = FALSE),
    error = function(e) NULL
  )

  if (!is.null(parsed_fixed)) {
    return(parsed_fixed)
  }

  stop(
    "Failed to parse repaired JSON. Last repaired response:\n",
    substr(fixed_text %||% "", 1, 1000),
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

ragas_fix_output_format_prompt_string <- function(output_string, prompt_value) {
  instruction <- "The output string did not satisfy the constraints given in the prompt. Fix the output string and return it."

  schema <- json_schema_object(list(
    text = list(type = "string")
  ))

  ragas_prompt_string(
    instruction = instruction,
    output_schema = schema,
    input_data = list(
      output_string = output_string,
      prompt_value = prompt_value
    ),
    examples = list()
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
ragas_generate_multiple <- function(
  prompt,
  n,
  model = "gpt-4o-mini",
  seed = NULL,
  temperature = 0,
  max_retries = 3L
) {
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

faithfulness_statement_generator_prompt <- function(
  question,
  answer,
  model = "gpt-4o-mini",
  seed = NULL
) {
  instruction <- paste(
    "Given a question and an answer, analyze the complexity of each sentence in the answer.",
    "Break down each sentence into one or more fully understandable statements.",
    "Ensure that no pronouns are used in any statement.",
    "Format the outputs in JSON."
  )

  schema <- json_schema_object(list(
    statements = list(
      type = "array",
      items = list(type = "string", description = "The generated statements")
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
        "Albert Einstein is recognized as one of the greatest and most influential physicists of all time.",
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

  parsed <- ragas_generate_json(
    prompt,
    model = model,
    seed = seed,
    temperature = 0,
    max_retries = 1L
  )

  if (is.null(parsed) || is.null(parsed$statements)) {
    return(character(0))
  }

  statements <- parsed$statements
  statements <- unname(as.character(unlist(statements, use.names = FALSE)))
  statements <- statements[nzchar(trimws(statements))]

  statements
}

faithfulness_nli_prompt <- function(
  contexts,
  statements,
  model = "gpt-4o-mini",
  seed = NULL
) {
  instruction <- paste(
    "Your task is to judge the faithfulness of a series of statements based on a given context.",
    "For each statement you must return verdict as 1 if the statement can be directly inferred based on the context or 0 if the statement can not be directly inferred based on the context."
  )

  statement_schema <- json_schema_object(list(
    statement = list(type = "string", description = "the original statement, word-by-word"),
    reason = list(type = "string", description = "the reason of the verdict"),
    verdict = list(type = "integer", description = "the verdict(0/1) of the faithfulness.")
  ))

  schema <- json_schema_object(list(
    statements = json_schema_array(statement_schema)
  ))

  examples <- list(
    list(
      input = list(
        context = paste(
          "John is a student at XYZ University. He is pursuing a degree in Computer Science. He is enrolled in several courses this semester, including Data Structures, Algorithms, and Database Management.",
          "John is a diligent student and spends a significant amount of time studying and completing assignments. He often stays late in the library to work on his projects."
        ),
        statements = list(
          "John is majoring in Biology.",
          "John is taking a course on Artificial Intelligence.",
          "John is a dedicated student.",
          "John has a part-time job."
        )
      ),
      output = list(
        statements = list(
          list(
            statement = "John is majoring in Biology.",
            reason = "John's major is explicitly mentioned as Computer Science. There is no information suggesting he is majoring in Biology.",
            verdict = 0
          ),
          list(
            statement = "John is taking a course on Artificial Intelligence.",
            reason = "The context mentions the courses John is currently enrolled in, and Artificial Intelligence is not mentioned. Therefore, it cannot be deduced that John is taking a course on AI.",
            verdict = 0
          ),
          list(
            statement = "John is a dedicated student.",
            reason = "The context states that he spends a significant amount of time studying and completing assignments. Additionally, it mentions that he often stays late in the library to work on his projects, which implies dedication.",
            verdict = 1
          ),
          list(
            statement = "John has a part-time job.",
            reason = "There is no information given in the context about John having a part-time job.",
            verdict = 0
          )
        )
      )
    ),
    list(
      input = list(
        context = "Photosynthesis is a process used by plants, algae, and certain bacteria to convert light energy into chemical energy.",
        statements = list("Albert Einstein was a genius.")
      ),
      output = list(
        statements = list(
          list(
            statement = "Albert Einstein was a genius.",
            reason = "The context and statement are unrelated",
            verdict = 0
          )
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

  parsed <- ragas_generate_json(
    prompt,
    model = model,
    seed = seed,
    temperature = 0,
    max_retries = 1L
  )

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

compute_faithfulness_ragas <- function(
  question,
  answer,
  contexts,
  model = "gpt-4o-mini",
  seed = NULL
) {
  statements <- faithfulness_statement_generator_prompt(
    question = question,
    answer = answer,
    model = model,
    seed = seed
  )

  if (length(statements) == 0L) {
    return(NaN)
  }

  verdicts <- faithfulness_nli_prompt(
    contexts = contexts,
    statements = statements,
    model = model,
    seed = seed
  )

  if (length(verdicts) == 0L) {
    return(NaN)
  }

  faithful_count <- sum(vapply(verdicts, function(x) {
    val <- suppressWarnings(as.integer(x$verdict))
    if (is.na(val)) return(0L)
    if (val == 1L) return(1L)
    0L
  }, integer(1)))

  faithful_count / length(verdicts)
}

# ---- Context Precision --------------------------------------------------------

context_precision_single_verification <- function(
  question,
  context,
  answer,
  model = "gpt-4o-mini",
  seed = NULL
) {
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

  parsed_list <- ragas_generate_multiple(prompt, n = 3L, model = model, seed = seed)
  verdict <- ensemble_binary_verdict(parsed_list, field = "verdict")

  list(
    reason = as.character(parsed_list[[1]]$reason %||% ""),
    verdict = verdict
  )
}

compute_context_precision_ragas <- function(
  question,
  answer_or_reference,
  contexts,
  model = "gpt-4o-mini",
  seed = NULL
) {
  if (is.null(contexts) || length(contexts) == 0L) {
    return(NA_real_)
  }

  verifications <- lapply(seq_along(contexts), function(i) {
    context_precision_single_verification(
      question = question,
      context = contexts[[i]],
      answer = answer_or_reference,
      model = model,
      seed = if (is.null(seed)) NULL else seed + i * 10L
    )
  })

  verdicts <- vapply(verifications, function(x) {
    if (is.null(x) || is.null(x$verdict) || is.na(x$verdict)) {
      return(0)
    }
    as.numeric(x$verdict)
  }, numeric(1))

  if (length(verdicts) == 0 || all(is.na(verdicts))) {
    return(NA_real_)
  }

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

context_recall_classification_prompt <- function(
  question,
  context,
  answer,
  model = "gpt-4o-mini",
  seed = NULL
) {
  instruction <- paste(
    "Given a context, and an answer, analyze each sentence in the answer and classify if the sentence can be attributed to the given context or not.",
    "Use only 'Yes' (1) or 'No' (0) as a binary classification. Output json with reason."
  )

  item_schema <- json_schema_object(list(
    statement = list(type = "string"),
    reason = list(type = "string"),
    attributed = list(type = "integer")
  ))

  schema <- json_schema_object(list(
    classifications = json_schema_array(item_schema)
  ))

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
          list(
            statement = "Albert Einstein, born on 14 March 1879, was a German-born theoretical physicist, widely held to be one of the greatest and most influential scientists of all time.",
            reason = "This sentence is directly supported by the context.",
            attributed = 1
          ),
          list(
            statement = "He received the 1921 Nobel Prize in Physics for his services to theoretical physics.",
            reason = "This sentence is directly supported by the context.",
            attributed = 1
          ),
          list(
            statement = "He published 4 papers in 1905.",
            reason = "The context does not mention this claim.",
            attributed = 0
          )
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

  valid_vals <- vals[!is.na(vals)]

  if (length(valid_vals) == 0L) {
    return(NA_real_)
  }

  mean(valid_vals)
}

# ---- Answer Relevance ---------------------------------------------------------

answer_relevance_question_generation_prompt <- function(
  response,
  model = "gpt-4o-mini",
  seed = NULL
) {
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

  gen_questions <- vapply(prompt_outputs, function(x) {
    as.character(x$question %||% "")
  }, character(1))

  q_vec <- get_openai_embeddings(
    texts = question,
    model = embedding_model
  )[1, ]

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
#' @param judge_model Character scalar; judge model name.
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
    qa_id <- qa_log$qa_id[i]
    q     <- qa_log$question[i]
    a     <- qa_log$answer_model[i]
    ctx   <- unlist(qa_log$retrieved_texts[[i]])

    if (length(ctx) == 0L) {
      ctx <- character(0)
    }

    gt <- NULL
    if (has_gt) {
      gt_val <- qa_log$answer_reference[i]
      if (!is.na(gt_val) && nzchar(gt_val)) {
        gt <- gt_val
      }
    }

    cp_answer <- gt %||% a

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

    overall <- mean(c(cp, cr, ar, fa), na.rm = TRUE)
    if (is.nan(overall)) {
      overall <- NA_real_
    }

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

# ---- WRAPPER -----------------------------------------------------------------

#' Compute RAGAS metrics
#'
#' Convenience wrapper for computing LLM-scored RAGAS-style metrics.
#'
#' @param qa_log QA log tibble.
#' @param judge_model Judge model for LLM-scored metrics.
#' @param seed Optional integer forwarded to LLM-scored metrics.
#' @param embedding_model Embedding model for answer relevance.
#' @param answer_relevance_strictness Number of reverse questions generated for
#'   answer relevance.
#'
#' @return A metrics tibble.
#' @export
compute_ragas_metrics <- function(
  qa_log,
  judge_model = "gpt-4o-mini",
  seed = NULL,
  embedding_model = "text-embedding-3-small",
  answer_relevance_strictness = 3L
) {
  compute_ragas_metrics_llm(
    qa_log,
    judge_model = judge_model,
    seed = seed,
    embedding_model = embedding_model,
    answer_relevance_strictness = answer_relevance_strictness
  )
}