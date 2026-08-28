#!/usr/bin/env Rscript
# ==============================================================================
# run_validation_batch.R
#
# Author:  Noam Tal-Perry, University of Cambridge
# Project: CaSDaR - "Evidencing the impact of data stewardship roles using
#          administrative and scientometrics data" (UKRI-funded, Network+
#          grant UKRI739)
# License: MIT (see LICENSE)
# Note:    drafted with the assistance of Claude Sonnet 5 (Anthropic),
#          Jul-Aug 2026; reviewed and verified by the author.
#
# Data scope (what this repository does and does not include):
#   This repository contains CODE ONLY. It does not include any job-ad
#   PDFs (Cambridge or external/validation), extracted text, or output
#   files. To run this script yourself, place your own external validation
#   PDFs under a "Validation_PDFs" folder (see input_dir below) alongside
#   your own configured extract_job_criteria.R.
#
# Runs extract_job_criteria.R against the external (non-Cambridge) job ads
# used to validate the similarity-score threshold - fully ISOLATED from the
# main recruitment dataset pipeline: separate input folder, separate output
# folders, separate status table, no category filter, no HR enrichment.
# Nothing here touches ../PDFs, ../TXTs, ../TXTs_merged, or
# ../Output/vacancy_status.csv.
#
# You do NOT need to rename these PDFs to fit the vacancy_[number] pattern.
# A filename that doesn't match just becomes its own singleton "group" -
# which is exactly what you want here anyway, since these are distinct
# postings, not reposts of each other. Each one gets its own MERGED.txt that
# is just its own (single-posting) extraction.
#
# Two ways to select the files - pick ONE, see the two options below.
# ==============================================================================

# Sourcing extract_job_criteria.R here is safe and does NOT trigger a run
# against the main Cambridge PDFs folder - that script only auto-runs
# main(CONFIG) when executed directly (`if (sys.nframe() == 0)`), which is
# false when the file is source()'d like this. This just defines CONFIG and
# all the functions (main, run_merge_pass, etc.) in this session.
source("extract_job_criteria.R")

# ------------------------------------------------------------------------------
# Isolated config: starts from the real CONFIG (so model/prompt/trim
# settings etc. stay consistent with your production runs) and overrides
# only what needs to differ for this validation batch.
#
# IMPORTANT: every toggle that controls WHICH files get selected/skipped is
# explicitly pinned below, not left to inherit from CONFIG's current value.
# CONFIG is the same object you actively edit for day-to-day production runs
# (retry_mode, target_files_only, call_llm, skip_existing_output have all
# been flipped on/off during testing) - modifyList() only overrides what's
# listed here, so anything NOT listed silently carries over whatever CONFIG
# happens to be set to at the moment this script is sourced. That's exactly
# what caused validation runs to silently do nothing earlier: retry_mode was
# left TRUE from unrelated testing, so this run went looking for retries
# against a validation_status.csv that didn't exist yet, rather than doing a
# normal scan. Pinning all four below (retry_mode, target_files_only,
# call_llm, skip_existing_output) makes this script's behaviour independent
# of whatever state you last left CONFIG in.
# ------------------------------------------------------------------------------
VALIDATION_CONFIG <- modifyList(CONFIG, list(

  # --- OPTION A (recommended): dedicated folder, no filenames to type ---
  # Drop all external PDFs into this folder and leave target_files_only
  # FALSE (pinned below) - a normal folder scan, just scoped to a small,
  # separate folder instead of the main PDFs/ folder.
  input_dir = file.path("..", "Validation_PDFs"),
  target_files_only = FALSE,

  # --- OPTION B: explicit file list instead ---
  # If the PDFs are scattered across different folders and consolidating
  # them isn't convenient, switch target_files_only above to TRUE and
  # uncomment target_files below (input_dir above is only used then to
  # resolve any BARE filenames you list, so it's harmless to leave as-is):
  # target_files_only = TRUE,
  # target_files = c(
  #   "/full/path/to/first_external_ad.pdf",
  #   "/full/path/to/second_external_ad.pdf"
  #   # ... etc
  # ),

  # Pinned explicitly - see the big comment above for why. This is a
  # from-scratch validation run every time, not a retry of a previous one.
  retry_mode = FALSE,

  # Pinned explicitly - if this were silently inherited as FALSE from a
  # prior debugging session, the run would "succeed" with zero API calls
  # and zero output, with no warning message at all (unlike the retry_mode
  # case above, which at least logged why). Worth being paranoid about.
  call_llm = TRUE,

  # Pinned explicitly TRUE (the standard/safe default) so re-running this
  # script twice doesn't silently re-spend tokens on files already done in
  # Validation_TXTs/. Flip to FALSE yourself if you deliberately want to
  # force-overwrite while iterating.
  skip_existing_output = TRUE,

  # Fully separate output tree - never overlaps with the main run's files.
  output_dir           = file.path("..", "Validation_TXTs"),
  extracted_text_dir   = file.path("..", "Validation_TXTs_extracted"),
  merged_dir           = file.path("..", "Validation_TXTs_merged"),
  status_table_path    = file.path("..", "Validation_Output", "validation_status.csv"),
  log_dir              = file.path("..", "Validation_Output"),
  category_filter_log_path = file.path("..", "Validation_Output", "category_filter_decisions.csv"),

  # These external postings have no Cambridge HR record at all - turn both
  # off rather than let every row come back with blank HR columns and a
  # meaningless category-filter pass (filter_by_category is skipped
  # automatically anyway whenever target_files_only is TRUE, but turning it
  # off here too keeps behaviour identical whichever of Option A/B you use).
  filter_by_category     = FALSE,
  add_hr_status_columns   = FALSE,

  # No need to cap a small batch.
  enable_pdf_limit = FALSE,

  # Keep this on - see the note above about singleton groups being exactly
  # the right behaviour for distinct, unrelated postings.
  group_by_vacancy = TRUE
))

# ------------------------------------------------------------------------------
# Sanity check, printed BEFORE anything runs - so a mistake like the one
# above is obvious immediately, not discovered after the fact from a log
# file. Glance at this every time before letting it proceed.
# ------------------------------------------------------------------------------
cat("=== Validation run settings ===\n")
cat("input_dir:            ", VALIDATION_CONFIG$input_dir, "\n")
cat("target_files_only:    ", VALIDATION_CONFIG$target_files_only, "\n")
if (isTRUE(VALIDATION_CONFIG$target_files_only)) {
  cat("target_files:        ", paste(VALIDATION_CONFIG$target_files, collapse = ", "), "\n")
}
cat("retry_mode:           ", VALIDATION_CONFIG$retry_mode, "\n")
cat("call_llm:             ", VALIDATION_CONFIG$call_llm, "\n")
cat("skip_existing_output: ", VALIDATION_CONFIG$skip_existing_output, "\n")
cat("output_dir:           ", VALIDATION_CONFIG$output_dir, "\n")
cat("status_table_path:    ", VALIDATION_CONFIG$status_table_path, "\n")
cat("================================\n")

main(VALIDATION_CONFIG)
