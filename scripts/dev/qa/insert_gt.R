#!/usr/bin/env Rscript

# scripts/dev/qa/insert_gt.R
# Populate qa_log$answer_reference using qa_id from a ground-truth CSV.
#
# Usage:
#   1) Create a template to fill manually:
#      Rscript scripts/dev/qa/insert_gt.R --init
#
#   2) Apply ground truth:
#      Rscript scripts/dev/qa/insert_gt.R --gt db/ground_truth.csv
#
# Optional:
#   --qa_log db/qa_log.rds        (default)
#   --out   db/qa_log.rds         (default: overwrite qa_log)
#   --backup_dir db/backups       (default: db/backups)
#   --strict                       (error if any qa_id missing ground truth)

args <- commandArgs(trailingOnly = TRUE)

get_arg <- function(flag, default = NULL) {
  idx <- match(flag, args)
  if (!is.na(idx) && idx < length(args)) return(args[idx + 1])
  default
}

has_flag <- function(flag) flag %in% args

qa_log_path   <- get_arg("--qa_log", "db/qa_log.rds")
gt_path       <- get_arg("--gt", NA_character_)
out_path      <- get_arg("--out", qa_log_path)
backup_dir    <- get_arg("--backup_dir", "db/backups")
init_mode     <- has_flag("--init")
strict_mode   <- has_flag("--strict")

required_cols <- c(
  "qa_id", "question", "prompt_final", "answer_model",
  "answer_reference", "collection", "retrieved_ids", "retrieved_texts",
  "chat_model", "embedding_model", "timestamp"
)

stopf <- function(...) stop(sprintf(...), call. = FALSE)

if (!file.exists(qa_log_path)) {
  stopf("QA log not found: %s", qa_log_path)
}

qa <- readRDS(qa_log_path)

missing_cols <- setdiff(required_cols, names(qa))
if (length(missing_cols) > 0) {
  stopf("qa_log is missing columns: %s", paste(missing_cols, collapse = ", "))
}

if (anyNA(qa$qa_id)) stopf("qa_id contains NA values; cannot safely join.")
if (any(duplicated(qa$qa_id))) stopf("qa_id contains duplicates; must be unique.")

# INIT MODE: write a template CSV with qa_id + question + empty answer_reference
if (isTRUE(init_mode)) {
  template_path <- if (!is.na(gt_path)) gt_path else "db/ground_truth.csv"

  template <- data.frame(
    qa_id = qa$qa_id,
    question = qa$question,
    answer_reference = "",
    stringsAsFactors = FALSE
  )

  dir.create(dirname(template_path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(template, template_path, row.names = FALSE, na = "")

  cat("Ground truth template created:\n")
  cat("  ", template_path, "\n", sep = "")
  cat("Fill the 'answer_reference' column, then run:\n")
  cat("  Rscript scripts/dev/qa/insert_gt.R --gt ", template_path, "\n", sep = "")
  quit(status = 0)
}

# APPLY MODE: require --gt
if (is.na(gt_path) || !nzchar(gt_path)) {
  stopf("Missing --gt. Provide a CSV file, or run with --init to generate a template.")
}
if (!file.exists(gt_path)) {
  stopf("Ground truth CSV not found: %s", gt_path)
}

gt <- utils::read.csv(gt_path, stringsAsFactors = FALSE, check.names = FALSE)

# Accept either:
#   (A) columns: qa_id, answer_reference
#   (B) columns: qa_id, answer (alias for answer_reference)
if (!("qa_id" %in% names(gt))) stopf("Ground truth CSV must have a 'qa_id' column.")

if (!("answer_reference" %in% names(gt))) {
  if ("answer" %in% names(gt)) {
    gt$answer_reference <- gt$answer
  } else {
    stopf("Ground truth CSV must have 'answer_reference' (or 'answer') column.")
  }
}

gt <- gt[, c("qa_id", "answer_reference")]
gt$qa_id <- suppressWarnings(as.integer(gt$qa_id))

if (anyNA(gt$qa_id)) stopf("Ground truth CSV has non-integer/NA qa_id values.")
if (any(duplicated(gt$qa_id))) stopf("Ground truth CSV has duplicate qa_id values.")

# Normalize text
gt$answer_reference <- as.character(gt$answer_reference)
gt$answer_reference[is.na(gt$answer_reference)] <- ""

# Join + update
idx <- match(qa$qa_id, gt$qa_id)  # position in gt for each qa row
found <- !is.na(idx)

updated <- qa
updated$answer_reference <- qa$answer_reference

# Update only where provided and non-empty
new_vals <- rep("", nrow(qa))
new_vals[found] <- gt$answer_reference[idx[found]]

to_update <- found & nzchar(trimws(new_vals))
updated$answer_reference[to_update] <- new_vals[to_update]

# Reporting
missing_in_gt <- qa$qa_id[!found]
empty_in_gt <- qa$qa_id[found & !nzchar(trimws(new_vals))]

cat("QA log:", qa_log_path, "\n")
cat("GT CSV:", gt_path, "\n")
cat("Rows in QA log:", nrow(qa), "\n")
cat("GT provided for qa_id:", sum(found), "\n")
cat("Updated answer_reference:", sum(to_update), "\n")

if (length(missing_in_gt) > 0) {
  cat("qa_id missing in GT CSV:", length(missing_in_gt), "\n")
  cat("  Example missing qa_id:", paste(utils::head(missing_in_gt, 10), collapse = ", "), "\n")
}
if (length(empty_in_gt) > 0) {
  cat("qa_id present but empty answer_reference:", length(empty_in_gt), "\n")
  cat("  Example empty qa_id:", paste(utils::head(empty_in_gt, 10), collapse = ", "), "\n")
}

if (isTRUE(strict_mode) && (length(missing_in_gt) > 0 || length(empty_in_gt) > 0)) {
  stopf("--strict enabled: refusing to write because some qa_id are missing/empty ground truth.")
}

# Backup original QA log before writing
dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)
stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
backup_path <- file.path(backup_dir, paste0("qa_log_backup_", stamp, ".rds"))
saveRDS(qa, backup_path)
cat("Backup written:", backup_path, "\n")

# Write updated QA log
saveRDS(updated, out_path)
cat("Updated QA log written:", out_path, "\n")
