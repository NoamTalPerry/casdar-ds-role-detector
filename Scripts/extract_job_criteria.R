#!/usr/bin/env Rscript
# ==============================================================================
# extract_job_criteria.R  ("Script 1")
#
# Author:  Noam Tal-Perry, University of Cambridge
# Project: CaSDaR - "Evidencing the impact of data stewardship roles using
#          administrative and scientometrics data" (UKRI-funded, Network+
#          grant UKRI739)
# License: MIT (see LICENSE)
# Note:    drafted with the assistance of Claude Sonnet 5 (Anthropic),
#          Jul-Aug 2026; reviewed and verified by the author.
#
# Purpose:
#   Scan a folder of job advertisement PDFs, convert each to text, send it to
#   the Claude API and ask it to extract (verbatim, as they appear in the ad):
#     1) Responsibilities / duties
#     2) Skills / person-specification criteria (tagged Essential/Desirable
#        where the ad distinguishes them)
#   Each result is written to its own .txt file (named after the source PDF)
#   in a dedicated output folder, as two bullet-point lists.
#
# Data scope (what this repository does and does not include):
#   This repository contains CODE ONLY. The job-advertisement PDFs and the
#   two HR reference files this script reads (CONFIG$server_file_mapping_path
#   and CONFIG$recruitment_data_path below) are NOT included here, since they
#   contain personal/HR data. To run this script yourself, point CONFIG at
#   your own equivalent inputs - the folders referenced below (../PDFs,
#   ../Data) will not exist until you supply them.
#
# Usage:
#   Rscript extract_job_criteria.R --input path/to/pdfs --output path/to/out
#
#   Or just edit the CONFIG block below and run interactively / with no args.
#
# Requirements:
#   install.packages(c("pdftools", "httr2", "jsonlite", "stringr"))
#
# API key:
#   Set the environment variable ANTHROPIC_API_KEY before running, e.g.:
#     Sys.setenv(ANTHROPIC_API_KEY = "sk-ant-...")          # in an R session
#     export ANTHROPIC_API_KEY="sk-ant-..."                 # in a shell
#   The script never hard-codes or prints the key.
# ==============================================================================

suppressPackageStartupMessages({
  library(pdftools)
  library(httr2)
  library(jsonlite)
  library(stringr)
})

# ------------------------------------------------------------------------------
# CONFIG - edit these defaults if you prefer not to use command-line flags
# ------------------------------------------------------------------------------
CONFIG <- list(
  input_dir  = file.path("..", "PDFs"),       # folder containing the PDFs
  output_dir = file.path("..", "TXTs"),       # dedicated output folder for final .txt criteria files
  extracted_text_dir = file.path("..", "TXTs_extracted"), # debug folder for saved extracted/trimmed text

  # --- Model toggle -----------------------------------------------------------
  # Switch between models by changing this one value. Must be a key present in
  # CONFIG$model_request_overrides below - call_claude() looks up model-specific
  # request quirks from there rather than guessing from the model name, so an
  # unrecognized model fails loudly instead of silently sending the wrong (or
  # rejected) parameters.
  #
  # IMPORTANT for A/B comparison runs: point output_dir/merged_dir/
  # status_table_path at MODEL-SPECIFIC folders when switching models (e.g.
  # "../TXTs_haiku" vs "../TXTs"). skip_existing_output does not know which
  # model produced an existing file, so reusing the same output folder across
  # models will silently mix results and invalidate the comparison.
  model      = "claude-sonnet-5",             # "claude-sonnet-5" or "claude-haiku-4-5-20251001"

  # Per-model request-body overrides, verified against Anthropic's docs
  # (platform.claude.com) as of writing:
  #   - Claude Sonnet 5: adaptive thinking is ON by default and draws from the
  #     same max_tokens budget as the answer, so we explicitly disable it for
  #     this pure verbatim-extraction task. Manual extended thinking
  #     (thinking.type="enabled") and any non-default temperature/top_p/top_k
  #     both return an HTTP 400 on this model - never send either.
  #   - Claude Haiku 4.5: the inverse on both counts. Extended thinking here is
  #     OFF by default and opt-in only (no adaptive thinking on this model at
  #     all) - so there is nothing to disable; we simply omit `thinking`
  #     entirely rather than sending a Sonnet-style {type:"disabled"} value
  #     that doesn't apply to this model's thinking implementation. Sampling
  #     parameters ARE supported on Haiku 4.5 (it predates the "4.7 and later"
  #     models where they were removed) - so temperature is available and set
  #     to 0 here for run-to-run consistency on this deterministic extraction
  #     task (docs note this reduces but does not fully eliminate variance).
  #     Only temperature is set, never top_p alongside it - Claude 4.5 models
  #     reject requests specifying both at once.
  model_request_overrides = list(
    "claude-sonnet-5" = list(
      thinking = list(type = "disabled")
    ),
    "claude-haiku-4-5-20251001" = list(
      temperature = 0
    )
  ),

  max_tokens = 4096,                          # ceiling for long ads; raised further if truncation occurs
  api_url    = "https://api.anthropic.com/v1/messages",
  api_version = "2023-06-01",
  sleep_between_calls = 1,                    # seconds; be polite to the API / avoid rate limits

  # --- Page-trimming (cost reduction) ---------------------------------------
  # Most of these ads are 12-16 pages, but responsibilities/skills content is
  # only ever within the first few pages, followed by boilerplate (pensions,
  # "living in Cambridge", equality statements, etc.). Sending the whole PDF
  # as input tokens is the main cost driver, so we keep only pages that look
  # relevant, plus a small buffer, instead of sending everything.
  #
  # NOTE: keywords must be fairly distinctive PHRASES, not generic single
  # words. Words like "essential", "desirable", "qualifications", "duties",
  # "requirements" also show up incidentally in unrelated boilerplate (e.g.
  # the standard "Terms of Appointment" paragraph mentions "qualifications
  # that are essential and/or desirable", the disability section mentions
  # "adjustments that you may require", etc.) - so single-word keywords like
  # that will match nearly every page and defeat the point. "the role" is
  # similarly too broad - it's a running page header ("The role (continued)")
  # on pages that have nothing to do with responsibilities/skills. Keep this
  # list to multi-word section-header phrases that are unlikely to appear
  # incidentally in prose.
  #
  # NOTE on matching strategy: we do NOT do a simple grepl() of the keyword
  # anywhere in the page text. Two separate failure modes were found and
  # fixed here:
  #   1. Words like "responsibilit(y/ies)" also occur incidentally in
  #      unrelated boilerplate common to every ad (e.g. "We have a legal
  #      responsibility to ensure...", "...employees with caring
  #      responsibilities..."). Fix: only match short, heading-like LINES,
  #      not any occurrence in the page.
  #   2. pdftools sometimes merges text from two side-by-side PDF columns
  #      onto a single rendered line, producing short hybrid lines that
  #      contain a keyword purely by coincidence (e.g. "We have a legal
  #      responsibility Information for disabled", "The person
  #      specification for time."). These pass the short-line-length check
  #      but are NOT genuine headings. Fix: a genuine heading has only a
  #      short/allowed amount of text immediately before and after the
  #      keyword match on its line (e.g. "Key ", "(continued)", "& skills");
  #      merge artifacts have unrelated sentence fragments before/after.
  trim_pages       = TRUE,                    # set FALSE to send full PDF text (old behaviour)
  trim_keywords     = c(
    "responsibilit(?:y|ies)", "person specification", "person profile",
    "role profile", "skills\\s+and\\s+(?:experience|qualifications)", "specialist knowledge",
    "purpose\\s+of\\s+the\\s+role|role\\s+purpose", "criteria\\s+essential\\s+desirable",
    "key duties", "key attributes", "additional requirements"
  ),
  trim_header_max_chars   = 55,                # max length (whitespace-collapsed) for a line to count as a heading
  trim_header_prefix_max  = 6,                 # max chars allowed before the keyword match on the line (e.g. "Key ", "Main ", "1. ")
  trim_header_suffix_patterns = c(             # allowed short continuations after the keyword match (regex, matched in full)
    "^$", "^\\(?continued\\)?$", "^(&|and)\\s*(skills|experience)$", "^essential\\s+desirable$",
    "^:.{0,30}$",                               # "Heading: short subheading" (e.g. "Person profile: Key Skills...")
    "^(include|includes|required|needed|desired)s?:?$"  # "Heading include:" style (e.g. "Key responsibilities include:")
  ),
  trim_buffer_pages    = 1,                    # include N pages before/after each matched page
  trim_always_include_pages = 1,               # ALWAYS include the first N pages regardless of keyword matches.
  # Safety net: ad templates vary a lot across departments, and some
  # (e.g. shorter, plain-prose ads) put real responsibilities/skills
  # content on page 1 with heading phrasing we haven't anticipated.
  # Missing genuine content is a much worse failure than sending one
  # extra cheap page, so this is a deliberately low-cost safety net.
  trim_fallback_pages  = 5,                    # if NO page matches any keyword, fall back to first N pages
  trim_hard_cap_pages  = 10,                   # safety ceiling on total pages sent, regardless of matches

  # --- Diagnostics ------------------------------------------------------------
  save_extracted_text = TRUE,                # if TRUE, save the exact text sent to the LLM (after any
  # page-trimming) to a debug .txt file per PDF, so you can
  # inspect what would be sent without re-opening the PDF
  call_llm             = TRUE,                # if FALSE, skip the Claude API call entirely - just does
  # text extraction/page-trimming (and saves debug text if
  # save_extracted_text is also TRUE). Useful for iterating
  # on the keyword list without spending any tokens.

  # --- Re-run behaviour ---------------------------------------------------
  skip_existing_output = TRUE,                # if TRUE, skip a PDF entirely (no page extraction, no API
  # call) when its output .txt already exists in output_dir -
  # avoids re-spending tokens on files you've already done.
  # If FALSE, always reprocess and overwrite the existing file.

  # --- Targeted single/multi-file mode ---------------------------------------
  # When target_files_only is TRUE, config$input_dir is ignored entirely and
  # ONLY the files listed in target_files are processed - a quick way to
  # test/debug one or a few specific PDFs without physically restructuring
  # the PDFs folder. Entries can be bare filenames (resolved against
  # input_dir) or full paths; missing files are logged and skipped, not an
  # error. Ordinary skip_existing_output behaviour still applies here (set
  # it to FALSE yourself if you want to force-overwrite while iterating).
  #
  # Mutually exclusive with retry_mode below - see resolve_extraction_pdf_files().
  # Does NOT affect run_merge_pass(): the merge/status-table step always
  # scans the complete input_dir (see that function's own comment) so a
  # targeted single-file run still produces a fully correct, complete
  # vacancy_status.csv - not one truncated to a single-PDF "group".
  target_files_only = FALSE,
  target_files       = character(0),          # e.g. c("vacancy_EXAMPLE1_abc123.pdf") or full paths

  # --- Retry mode --------------------------------------------------------
  # When retry_mode is TRUE, config$input_dir is ignored as a source of
  # "what to process" and replaced with a targeted set derived from
  # vacancy_status.csv: only PDFs belonging to a vacancy GROUP whose status
  # is in retry_target_statuses below (default: failed or partial), AND
  # which don't yet have a genuine (non-empty) extraction on disk. PDFs
  # belonging to "complete" groups are skipped even if their group's row is
  # otherwise touched, and any PDF not yet part of any known group at all
  # (never seen by a previous merge pass) is ignored - retry_mode is for
  # finishing/fixing known incomplete work, not discovering new files.
  #
  # "Genuine (non-empty) extraction" specifically means: a PDF is only
  # skipped as already-done if its .txt file exists AND contains at least
  # one responsibility or skill - a "processed" run that legitimately
  # succeeded (valid JSON, no error) but came back with BOTH sections empty
  # ("(none extracted)") is treated the same as a real failure and will be
  # retried. See is_empty_extraction() and its use in run_merge_pass(),
  # which now also classifies group_status this way - a singleton group
  # whose one posting extracted nothing now correctly shows "failed"
  # instead of "complete", which is what makes retry_mode actually able to
  # find these via the status column as requested, rather than them hiding
  # inside groups that look "complete".
  #
  # Retry candidates ALWAYS force reprocessing regardless of
  # skip_existing_output - the whole point is overwriting a known-bad
  # existing .txt, per instruction. Every candidate PDF is re-checked
  # against CURRENT disk state (not just trusted from vacancy_status.csv,
  # which could be slightly stale), so this is safe to run repeatedly.
  #
  # Mutually exclusive with target_files_only above.
  retry_mode             = TRUE,
  retry_target_statuses  = c("failed", "partial"),   # edit to e.g. c("failed") to retry only one

  # --- Per-run PDF cap (cost control) -----------------------------------
  # A simple throttle, independent of and composable with everything above
  # (retry_mode, target_files_only, filter_by_category, skip_existing_output
  # all continue to work exactly as normal) - lets you run a large backlog
  # in deliberately small batches (e.g. 100-200 PDFs at a time) to check
  # actual cost before committing to the rest.
  #
  # Counts only files that actually reach the API call this run - a file
  # skip_existing_output skips for free does NOT count against the cap, so
  # re-running the same capped config repeatedly naturally advances through
  # the backlog (each run picks up where the last one's actual work left
  # off) rather than getting stuck re-scanning the same already-done window
  # every time. Once the cap is hit, the run stops immediately rather than
  # skipping ahead to look for more free (already-done) files further down
  # the list.
  #
  # The merge/status-table pass at the end is scoped to exactly the files
  # THIS run actually looked at (via run_merge_pass()'s scope_pdf_files) -
  # so a group with members beyond the cap is left completely untouched
  # this run (not marked partial/failed just for being unseen; it'll be
  # picked up correctly whenever a later run actually reaches it).
  enable_pdf_limit  = TRUE,
  max_pdfs_per_run  = 500,

  # --- Logging -------------------------------------------------------------
  log_to_file = TRUE,                         # if TRUE, mirror all console output to a timestamped log
  # file as well (see log_dir)
  log_dir     = file.path("..", "Output"),    # folder for run logs, sibling to PDFs/TXTs/TXTs_extracted

  # --- Repost grouping & merging (vacancy-level dedup) ----------------------
  # Files sharing the same "vacancy_NNN" number (different UUID/template
  # suffix) can be reposts of the same underlying job - see dedup_union() and
  # normalize_bullet() below for exactly how postings sharing a vacancy
  # number are merged.
  # Each posting is still extracted independently above via its own LLM call
  # (deliberately never combined into one prompt, to avoid re-introducing
  # context-confusion bugs) - grouping/merging happens afterwards, purely
  # programmatically, from already-extracted results. No extra API calls.
  #
  # Decided design: the merged record per vacancy is the UNION of every
  # distinct item ever stated across its postings, deduped on a normalized
  # EXACT match only (not fuzzy) - under-merging (a little harmless
  # redundancy) is a much safer failure than over-merging (silently losing a
  # genuinely distinct item). PDF metadata is captured for information only
  # and is NOT used to pick a "most recent" posting, since it was found to
  # reflect template-authoring dates rather than posting dates for at least
  # one confirmed case in our own corpus.
  group_by_vacancy   = TRUE,                  # set FALSE to disable grouping/merging/status-table entirely
  vacancy_regex      = "(?i)vacancy[_-]?[0-9]+",  # pattern used to pull the vacancy number out of a filename
  merged_dir         = file.path("..", "TXTs_merged"),           # one UNION-merged .txt per vacancy group
  status_table_path  = file.path("..", "Output", "vacancy_status.csv"),  # one row per vacancy group, persisted/upserted across runs
  overlap_flag_threshold = 0.3,                # groups with >1 posting whose mean pairwise overlap (Jaccard
  # over normalized items) falls below this are marked
  # flagged_for_review = TRUE in the status table. This NEVER blocks or
  # changes the merge itself - it is only a signal for a human to spot-check
  # whether two postings under the same vacancy number are really the same
  # role (we deliberately do not try to auto-judge that automatically).

  # --- Category-based pre-filtering (HR data) -------------------------------
  # Cost-reduction lever #2, separate from page-trimming above: some job ads
  # are never going to be data-stewardship roles based on their HR "Position
  # Category", so we can skip processing (and paying for) them entirely
  # rather than just trimming pages within them.
  #
  # IMPORTANT - this is a PRECISION lever, not a recall one, and it sits in
  # real tension with this project's usual recall-over-precision bias (see
  # the page-trimming comments above, which always err toward sending one
  # extra cheap page rather than risking missed content). Excluding an
  # entire PDF from processing is a bigger and less reversible loss than
  # sending one extra page - if this filter gets it wrong, the file is never
  # even attempted. To manage that risk:
  #   1. EVERY decision is logged per-PDF to category_filter_log_path (not
  #      just excluded files) every run the filter is enabled, so exclusions
  #      are fully auditable, never silent.
  #   2. Of the cases where we lack clear category evidence, two are
  #      user-controlled below rather than hardcoded, since in practice
  #      they've turned out to be large buckets, not rare edge cases:
  #        - category_filter_include_no_server_mapping: no row at all for
  #          the PDF in ras_fp_files.csv (that mapping currently only runs
  #          to mid-2025, so PDFs from after that have no row by
  #          construction - this was ~900 files in one observed dataset).
  #        - category_filter_include_no_recruitment_match: a VACANCY_NUMBER
  #          exists, but no row for it in the recruitment export (which only
  #          covers Jan 2022-Dec 2024).
  #      Both default TRUE (include - the original recall-biased default).
  #      Set either to FALSE to exclude that bucket instead once you've
  #      confirmed (via category_filter_log_path) that doing so is an
  #      acceptable precision/cost tradeoff for your data - flipping them
  #      off trades recall for a smaller/cheaper run, which only makes
  #      sense once the HR-data gap driving these buckets is well
  #      understood, not as a blind default.
  #      A third such case - a blank VACANCY_NUMBER on an existing
  #      ras_fp_files.csv row (a known single-row case observed during
  #      development) - is NOT separately configurable; it always defaults
  #      to include, since it has so far been a single-row edge case rather
  #      than a meaningful bucket. Ask if you want that one exposed too.
  #      Only a CONFIRMED category match outside the target list causes
  #      exclusion unconditionally (exclude_other_category).
  #   3. All lookups against the recruitment export's column headers (e.g.
  #      "Position Category", "Job Reference") go through get_hr_column(),
  #      a case-insensitive/whitespace-normalized matcher, rather than a
  #      hardcoded exact string - a real header-casing mismatch ("Position
  #      category" vs "Position Category") already broke an earlier version
  #      of this filter, so exact casing can no longer silently matter.
  #   4. Still recommend spot-checking category_filter_log_path against
  #      real data before trusting a full/production run - exclusion cases
  #      specifically haven't been reviewed yet as of this note.
  #
  # Join path - note the two HR files use a DIFFERENT vacancy-numbering
  # system from the "vacancy_NNN" the repost-grouping regex above extracts
  # from PDF filenames; the two must never be conflated (see the note in
  # build_hr_lookup() below):
  #   PDF filename (SERVER_FILENAME) --[ras_fp_files.csv]--> VACANCY_NUMBER
  #     --[join]--> Job Reference --[recruitment export]--> Position Category
  filter_by_category = TRUE,
  category_filter_include_no_server_mapping     = TRUE,  # see point 2 above
  category_filter_include_no_recruitment_match  = TRUE,  # see point 2 above
  hr_data_dir               = file.path("..", "Data"),
  server_file_mapping_path  = file.path("..", "Data", "ras_fp_files.csv"),
  # NOTE: point this at your own HR recruitment-system export - the filename
  # below is a generic placeholder, not the name of a real file in this repo.
  recruitment_data_path     = file.path("..", "Data", "recruitment_export.csv"),
  # Categories judged (from skimming job/position titles) most likely to
  # contain data stewardship roles. "Researcher" is deliberately excluded
  # for now - too large/heterogeneous a category, to be targeted separately
  # later - this is a project decision, not a limitation of the filter.
  target_position_categories = c(
    "Technician",
    "Computing",
    "Library",
    "Research Support",
    "Science, Engineering & Health Professional",
    "Other Administrative Professional"
  ),
  category_filter_log_path = file.path("..", "Output", "category_filter_decisions.csv"),
  # Full per-PDF decision log (every file, not just excluded ones) written
  # every run the filter is enabled - see point 1 above.

  # --- HR enrichment columns on vacancy_status.csv ---------------------------
  # Independent of filter_by_category above (can be used with or without
  # it) - this toggle loads the SAME two HR files purely to ADD informational
  # columns to vacancy_status.csv: job_ref, position_ref, position_category,
  # position_title, job_title, date_opened, date_closed. Unlike
  # filter_by_category, this NEVER removes or skips a PDF from processing -
  # it's purely additive, so it carries none of the recall-vs-precision risk
  # noted above. Defaults to FALSE only because it requires the two HR files
  # to exist (read_hr_csv() errors loudly if they don't), so turning it on
  # unexpectedly could break a run for someone without the Data/ folder.
  #
  # When multiple postings share a vacancy group (or a single posting's Job
  # Reference matches more than one recruitment-export row), each field is
  # deduplicated first (exact match after trimming) and only genuinely
  # distinct values are kept, joined with "; " - see dedup_concat_field().
  # A real case in our own corpus already showed fields that legitimately
  # differ across postings within the same vacancy_NNN group (different
  # specialty tracks), so divergence here is expected, real signal - not
  # something to collapse away.
  add_hr_status_columns = TRUE
)

# ------------------------------------------------------------------------------
# Allow simple --input / --output command-line overrides
# ------------------------------------------------------------------------------
parse_args <- function(config) {
  args <- commandArgs(trailingOnly = TRUE)
  if ("--input" %in% args) {
    config$input_dir <- args[which(args == "--input") + 1]
  }
  if ("--output" %in% args) {
    config$output_dir <- args[which(args == "--output") + 1]
  }
  config
}
CONFIG <- parse_args(CONFIG)

# ------------------------------------------------------------------------------
# Logging: mirrors console output to a timestamped log file (if enabled).
# .log_con is set up once in main() and used by log_msg()/log_warn()
# throughout the rest of the script, so every message/warning shown in the
# console also ends up in the log file for later reference.
# ------------------------------------------------------------------------------
.log_con <- NULL

log_msg <- function(text) {
  message(text)
  if (!is.null(.log_con)) {
    cat(text, "\n", file = .log_con, sep = "")
  }
}

# Used in place of warning() throughout: logs the same "WARNING: ..." text to
# both console and log file via log_msg(), for a single consistent log
# format, rather than relying on R's separate (and differently-timed)
# warning-condition printing.
log_warn <- function(text) {
  log_msg(paste("WARNING:", text))
}

setup_log_file <- function(config) {
  if (!isTRUE(config$log_to_file)) return(NULL)
  if (!dir.exists(config$log_dir)) {
    dir.create(config$log_dir, recursive = TRUE)
  }
  log_path <- file.path(config$log_dir, sprintf("run_log_%s.txt", format(Sys.time(), "%Y%m%d_%H%M%S")))
  con <- file(log_path, open = "at")
  message(sprintf("Logging this run to: %s", log_path))
  con
}

# ------------------------------------------------------------------------------
# Helper: extract plain text from a PDF (all pages, concatenated)
# ------------------------------------------------------------------------------
extract_pdf_text <- function(pdf_path) {
  pages <- tryCatch(
    pdftools::pdf_text(pdf_path),
    error = function(e) {
      log_warn(sprintf("Failed to read %s: %s", pdf_path, conditionMessage(e)))
      NULL
    }
  )
  if (is.null(pages)) return(NA_character_)
  paste(pages, collapse = "\n\n")
}

# ------------------------------------------------------------------------------
# Helper: does this page contain a keyword as a genuine short heading line
# (rather than buried inside boilerplate prose, or a two-column PDF
# rendering artifact that happens to contain the keyword)? See the long
# comment above CONFIG$trim_keywords for why both checks are needed.
# ------------------------------------------------------------------------------
line_is_heading_match <- function(line, keywords, max_header_chars, prefix_max, suffix_patterns) {
  collapsed <- str_trim(gsub("\\s+", " ", line))
  if (nchar(collapsed) == 0 || nchar(collapsed) > max_header_chars) return(FALSE)

  pattern <- paste(keywords, collapse = "|")
  loc <- regexpr(pattern, collapsed, ignore.case = TRUE, perl = TRUE)
  if (loc == -1) return(FALSE)

  match_start <- as.integer(loc)
  match_len   <- attr(loc, "match.length")
  match_end   <- match_start + match_len

  prefix <- str_trim(substr(collapsed, 1, match_start - 1))
  suffix <- str_trim(substr(collapsed, match_end, nchar(collapsed)))

  if (nchar(prefix) > prefix_max) return(FALSE)

  # suffix_patterns are pre-anchored (^...$) regexes for allowed short
  # continuations after the keyword (e.g. "(continued)", "& skills").
  # Anything else after the keyword on the line disqualifies the match.
  any(vapply(suffix_patterns, function(p) grepl(p, suffix, ignore.case = TRUE, perl = TRUE), logical(1)))
}

page_has_heading_match <- function(page_text, config) {
  lines <- strsplit(page_text, "\n")[[1]]
  any(vapply(
    lines,
    line_is_heading_match,
    logical(1),
    keywords = config$trim_keywords,
    max_header_chars = config$trim_header_max_chars,
    prefix_max = config$trim_header_prefix_max,
    suffix_patterns = config$trim_header_suffix_patterns
  ))
}

# ------------------------------------------------------------------------------
# Helper: extract only the pages likely to contain responsibilities/skills
# content, to cut down input tokens (and therefore cost) per PDF.
#
# Strategy: keep any page with a heading-like line matching one of
# `keywords` (see page_has_heading_match()), plus a small buffer of pages
# immediately before/after each match (continuation pages - e.g. "Key
# responsibilities (continued)" - are usually themselves a keyword match
# too, but the buffer catches cases where they aren't). If NO page matches
# at all (unusual ad layout), fall back to the first N pages rather than
# sending nothing. A hard cap bounds the worst case.
#
# Returns a list: $text (character), $pages_used (integer vector),
# $total_pages (integer), $matched_by_keyword (logical) - the last flag lets
# the caller warn when a file fell back to the blind first-N-pages heuristic,
# since that's worth spot-checking.
# ------------------------------------------------------------------------------
extract_relevant_pdf_text <- function(pdf_path, config) {
  pages <- tryCatch(
    pdftools::pdf_text(pdf_path),
    error = function(e) {
      log_warn(sprintf("Failed to read %s: %s", pdf_path, conditionMessage(e)))
      NULL
    }
  )
  if (is.null(pages)) {
    return(list(text = NA_character_, pages_used = integer(0),
                total_pages = 0L, matched_by_keyword = FALSE))
  }

  n <- length(pages)
  is_match <- vapply(pages, page_has_heading_match, logical(1), config = config)
  always_include <- seq_len(min(config$trim_always_include_pages, n))

  if (any(is_match)) {
    match_idx <- which(is_match)
    buffered <- unlist(lapply(match_idx, function(i) {
      seq(max(1, i - config$trim_buffer_pages), min(n, i + config$trim_buffer_pages))
    }))
    selected <- sort(unique(c(buffered, always_include)))
    if (length(selected) > config$trim_hard_cap_pages) {
      selected <- selected[seq_len(config$trim_hard_cap_pages)]
    }
    matched_by_keyword <- TRUE
  } else {
    selected <- sort(unique(c(seq_len(min(config$trim_fallback_pages, n)), always_include)))
    matched_by_keyword <- FALSE
  }

  list(
    text = paste(pages[selected], collapse = "\n\n"),
    pages_used = selected,
    total_pages = n,
    matched_by_keyword = matched_by_keyword
  )
}

# ------------------------------------------------------------------------------
# Helper: fallback job-reference scraper, used only when the HR mapping
# (ras_fp_files.csv) has no usable job reference for a PDF - either no row
# at all (the ras_fp export has a coverage gap - e.g. it currently only runs
# to mid-2025 while the PDF folder has postings into late 2025), or a row
# exists but VACANCY_NUMBER on it is blank. Operates on the ALREADY-
# EXTRACTED text used for the LLM prompt (job_text) - never re-opens or
# re-reads the PDF - per instruction, since that text is already in memory
# for this run at essentially no extra cost.
#
# Finds the FIRST occurrence of "Job Reference:" (case-insensitive,
# whitespace-tolerant on the colon) and returns whatever follows on that
# line, cut short at the first run of 2+ spaces/tab - a common column-
# boundary merge artifact in pdftools output, the same class of issue
# documented in CONFIG$trim_keywords above - so trailing unrelated text
# rendered on the same line isn't captured. Returns NA if no match is
# found (including if trim_pages happened to exclude the page the label
# was on) - the caller leaves the field blank in that case, per instruction.
# ------------------------------------------------------------------------------
extract_job_reference_from_text <- function(text) {
  if (is.na(text) || !nzchar(text)) return(NA_character_)
  m <- regexpr("(?i)Job\\s*Reference\\s*:\\s*[^\\r\\n]*", text, perl = TRUE)
  if (m == -1) return(NA_character_)
  full_match <- regmatches(text, m)
  value_part <- sub("(?i)^Job\\s*Reference\\s*:\\s*", "", full_match, perl = TRUE)
  value_part <- sub("\\s{2,}.*$", "", value_part)   # cut at column-merge artifact / trailing label
  value_part <- trimws(value_part)
  if (!nzchar(value_part)) return(NA_character_)
  value_part
}

# ------------------------------------------------------------------------------
# Helper: build the extraction prompt sent to Claude
# ------------------------------------------------------------------------------
build_prompt <- function(job_ad_text) {
  paste0(
    "You will be given the raw text of a job advertisement (extracted from a PDF, ",
    "so spacing/line breaks may be imperfect).\n\n",
    "Extract two things, using the EXACT wording from the advertisement ",
    "(verbatim quotes only - do not paraphrase, summarise, reorder words, or invent text):\n\n",
    "1. \"responsibilities\": every bullet point, sentence, or item describing job duties, ",
    "responsibilities, or what the postholder will do. Job ads sometimes contain MORE THAN ONE ",
    "responsibilities-type list or section (e.g. a short informal list near the top of the ad, ",
    "and a separate, more formally labelled list further down, such as under a 'Main ",
    "Responsibilities' or numbered/templated heading). These separate sections may be divided ",
    "by unrelated prose (e.g. background information about the department or organisation) ",
    "rather than appearing consecutively. Do NOT assume there is exactly one responsibilities ",
    "section: scan the ENTIRE text and collect responsibility/duty items from EVERY such list or ",
    "section you find, even if an unrelated paragraph sits between them, and even if one list ",
    "reads informally (e.g. embedded in introductory ad copy) rather than under a clearly-labelled ",
    "heading.\n",
    "2. \"skills\": every skill, qualification, or person-specification criterion.\n\n",
    "IMPORTANT rules for extracting skills:\n",
    "- Job ads often describe requirements in TWO places: a formal Person Specification / ",
    "Criteria table (usually with Essential/Desirable columns or checkmarks), and a ",
    "narrative paragraph elsewhere in the ad (e.g. under 'The role' or 'Purpose of the role') ",
    "that describes similar requirements in prose.\n",
    "- Extract skills ONLY from the formal Person Specification / Criteria table (or equivalent ",
    "structured list of essential/desirable criteria). Do NOT extract criteria from narrative ",
    "prose elsewhere in the ad, even if it restates or overlaps with something in the table - ",
    "this causes duplication. If there is no formal table at all, and skills/criteria are only ",
    "described in prose, then extract them from the prose instead.\n",
    "- Every row in the table is a criterion with its own tick/label - extract every one of them.\n",
    "- If the advertisement labels items as Essential or Desirable, prefix each extracted item with ",
    "\"[Essential] \" or \"[Desirable] \" accordingly (still verbatim otherwise). ",
    "If no such labelling exists, extract the items with no prefix.\n",
    "- Some tables have SECTION or CATEGORY HEADERS (e.g. 'Education & Qualifications', ",
    "'Specialist Knowledge & Skills') that may themselves carry a tick/checkmark in the ",
    "Essential or Desirable column, separate from the individual criteria rows beneath them. ",
    "NEVER extract a section/category header as if it were a skill or criterion in its own ",
    "right - only extract the actual criterion sentences/rows beneath each header. If a header ",
    "has a tick but no criterion text of its own, skip it entirely.\n",
    "- Some table cells span multiple sentences describing a single criterion (e.g. one cell may ",
    "contain two or three sentences under one tick mark). Extract the ENTIRE cell content as one ",
    "skill item, not just the first sentence - do not truncate multi-sentence cells.\n\n",
    "Return ONLY a JSON object with this exact shape, and nothing else ",
    "(no markdown fences, no commentary):\n",
    "{\n",
    "  \"responsibilities\": [\"...\", \"...\"],\n",
    "  \"skills\": [\"...\", \"...\"]\n",
    "}\n\n",
    "If a section genuinely isn't present in the text, return an empty array for it.\n\n",
    "--- JOB ADVERTISEMENT TEXT START ---\n",
    job_ad_text,
    "\n--- JOB ADVERTISEMENT TEXT END ---"
  )
}

# ------------------------------------------------------------------------------
# Helper: call the Claude API and return the parsed JSON (list with
# $responsibilities and $skills character vectors)
# ------------------------------------------------------------------------------
call_claude <- function(prompt, config, api_key, max_tries = 3) {
  if (!config$model %in% names(config$model_request_overrides)) {
    stop(sprintf(
      paste(
        "CONFIG$model = '%s' has no entry in CONFIG$model_request_overrides.",
        "Add one before using this model - sending default/unverified sampling",
        "and thinking parameters to an unrecognized model risks either an HTTP",
        "400 (e.g. Sonnet 5 rejecting temperature) or silently-wrong behaviour",
        "(e.g. omitting a needed thinking override). See the CONFIG$model",
        "comment for the models currently verified against Anthropic's docs."
      ),
      config$model
    ))
  }

  body <- c(
    list(
      model      = config$model,
      max_tokens = config$max_tokens,
      messages   = list(
        list(role = "user", content = prompt)
      )
    ),
    config$model_request_overrides[[config$model]]  # e.g. thinking=disabled on
    # Sonnet 5, or temperature=0 on Haiku 4.5 - see the CONFIG$model comment
    # for the verified reasoning behind each model's specific overrides
  )

  req <- request(config$api_url) |>
    req_headers(
      "x-api-key"         = api_key,
      "anthropic-version" = config$api_version,
      "content-type"      = "application/json"
    ) |>
    req_body_json(body)
  # Manual retry loop (avoids depending on req_retry(), which requires a
  # newer httr2 than some installs have). Retries on request errors and on
  # HTTP 429 / 5xx responses, with simple exponential backoff.
  resp <- NULL
  for (attempt in seq_len(max_tries)) {
    resp <- tryCatch(req_perform(req), error = function(e) e)
    if (!inherits(resp, "error")) {
      status <- resp_status(resp)
      if (status < 500 && status != 429) break
    }
    if (attempt < max_tries) Sys.sleep(2 ^ attempt)
  }

  if (inherits(resp, "error")) {
    log_warn(paste("API call failed:", conditionMessage(resp)))
    return(NULL)
  }

  body <- resp_body_json(resp, simplifyVector = FALSE)

  if (!is.null(body$error)) {
    log_warn(paste("API returned an error:", body$error$message %||% "unknown error"))
    return(NULL)
  }

  if (!is.null(body$stop_reason) && body$stop_reason == "max_tokens") {
    log_warn(paste(
      "Response was truncated because max_tokens was reached.",
      "Consider raising CONFIG$max_tokens for this batch (this ad",
      "likely has many bullet points). The raw partial response will",
      "still be saved for inspection."
    ))
  }

  # Concatenate all text blocks in the response
  text_blocks <- vapply(body$content, function(block) {
    if (!is.null(block$type) && block$type == "text") block$text else ""
  }, character(1))
  raw_text <- paste(text_blocks, collapse = "\n")

  # Strip any accidental ```json fences before parsing
  clean_text <- str_replace_all(raw_text, "```json|```", "") |> str_trim()

  parsed <- tryCatch(
    jsonlite::fromJSON(clean_text, simplifyVector = TRUE),
    error = function(e) {
      log_warn(paste("Could not parse JSON from model response:", conditionMessage(e)))
      NULL
    }
  )

  # Return both the parsed result (may be NULL) and the raw text, so the
  # caller can save the raw text for debugging even on failure.
  list(parsed = parsed, raw_text = raw_text)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# ------------------------------------------------------------------------------
# Helper: write the two bullet-point lists to a .txt file. Also persists
# scraped_job_ref and pages_used/total_pages into the header (when known) -
# not just for human reference, but so a LATER, independent merge pass
# (run_merge_pass(), possibly in a completely separate session) can fully
# reconstruct everything needed for vacancy_status.csv from disk alone,
# with no dependency on the in-memory state of whichever run originally
# processed this PDF. See run_merge_pass() for why that matters.
# ------------------------------------------------------------------------------
write_criteria_file <- function(out_path, responsibilities, skills, source_pdf, model = NULL,
                                scraped_job_ref = NA_character_, pages_used = NA_integer_,
                                total_pages = NA_integer_) {
  lines <- c(
    paste0("Source: ", basename(source_pdf)),
    if (!is.null(model)) paste0("Model: ", model),  # traceability for model A/B comparisons -
    # see the CONFIG$model comment for why the SAME output_dir should not be
    # reused across models, but this line still helps confirm which model
    # actually produced a given file if that guidance wasn't followed
    if (!is.na(scraped_job_ref) && nzchar(trimws(scraped_job_ref))) paste0("Scraped Job Reference: ", scraped_job_ref),
    if (!is.na(pages_used) && !is.na(total_pages)) paste0("Pages Used: ", pages_used, " of ", total_pages),
    "",
    "RESPONSIBILITIES",
    "----------------",
    if (length(responsibilities) == 0) "(none extracted)"
    else paste0("- ", responsibilities),
    "",
    "SKILLS (Essential / Desirable as labelled in the original ad)",
    "---------------------------------------------------------------",
    if (length(skills) == 0) "(none extracted)"
    else paste0("- ", skills)
  )
  writeLines(lines, out_path, useBytes = TRUE)
}

# ==============================================================================
# Repost grouping & merging helpers
# Design discussion for everything below: files sharing a "vacancy_NNN"
# number are treated as reposts of the same underlying job and merged via a
# normalized-exact-match union (see normalize_bullet()/dedup_union() below),
# deliberately never a fuzzy/semantic match, so a little harmless redundancy
# across postings is preferred over silently losing a genuinely distinct
# item.
# ==============================================================================

# ------------------------------------------------------------------------------
# Helper: pull the vacancy number out of a filename for repost grouping.
# Files where no vacancy number can be found are NOT dropped - each becomes
# its own singleton group, keyed by its own base filename, so it still flows
# through the merge/status-table step untouched (as a "group of one").
# ------------------------------------------------------------------------------
get_vacancy_group_key <- function(pdf_path, config) {
  base <- tools::file_path_sans_ext(basename(pdf_path))
  loc <- regexpr(config$vacancy_regex, base, perl = TRUE)
  if (loc == -1) {
    return(list(key = paste0("UNGROUPED::", base), vacancy_number = NA_character_, matched = FALSE))
  }
  whole_match <- regmatches(base, loc)
  num <- gsub("(?i)vacancy[_-]?", "", whole_match, perl = TRUE)
  list(key = num, vacancy_number = num, matched = TRUE)
}

# ------------------------------------------------------------------------------
# Helper: normalize an extracted item for use as a dedup/comparison KEY only -
# the original wording is always what gets kept/written out. This is
# intentionally narrow: it folds together purely TYPOGRAPHIC variants (which
# cannot conflate two genuinely different criteria) while remaining an EXACT
# match on content - it is NOT fuzzy/semantic matching, by design.
#
# Each normalization step below is evidenced by a real case found during
# manual QA on live vacancy data, not speculative (vacancy identifiers below
# are illustrative placeholders, not real reference numbers):
#   - curly vs straight quotes: vacancy_EXAMPLE1 - "practical 'can do'"
#     (posting 1) vs "practical 'can do'" (postings 2 & 3), same wording,
#     Word autocorrect difference between repost drafts.
#   - trailing period vs semicolon: vacancy_EXAMPLE2 - "...research staff."
#     (posting 1) vs "...research staff;" (posting 2), identical otherwise.
# Deliberately NOT included: stripping [Essential]/[Desirable] tags (a real
# vacancy_EXAMPLE3 case showed the SAME wording tagged Desirable in one
# posting and Essential in another - genuinely different signal, not noise -
# so tag stripping stays a separate, undecided question), and no
# prefix-stripping for category labels like "IT Infrastructure: ..."
# (vacancy_EXAMPLE2) - the line between a real label and real content is not
# safe to guess at automatically.
# ------------------------------------------------------------------------------
normalize_bullet <- function(x) {
  x <- gsub("[‘’ʼ]", "'", x)   # curly/modifier single quotes -> straight
  x <- gsub("[“”]", '"', x)          # curly double quotes -> straight
  x <- gsub("[–—]", "-", x)          # en/em dash -> hyphen
  x <- gsub("[   ]", " ", x)    # non-breaking/narrow spaces -> regular space
  x <- gsub("\\s+", " ", x)
  x <- trimws(tolower(x))
  x <- sub("[.;]+$", "", x)                    # drop a single trailing period/semicolon
  x
}

# ------------------------------------------------------------------------------
# Helper: UNION-merge bulleted items across multiple postings of the same
# vacancy, deduplicating on a normalized EXACT match only. `item_lists` is a
# list of character vectors (one per posting); first occurrence wins for
# which exact original wording is kept in the output.
# ------------------------------------------------------------------------------
dedup_union <- function(item_lists) {
  seen_keys <- character(0)
  out <- character(0)
  for (items in item_lists) {
    if (length(items) == 0) next
    keys <- normalize_bullet(items)
    keep <- !duplicated(keys) & !(keys %in% seen_keys)  # first-seen within this posting AND not seen in an earlier posting
    out <- c(out, items[keep])
    seen_keys <- c(seen_keys, keys[keep])
  }
  out
}

# ------------------------------------------------------------------------------
# Helper: Jaccard similarity between two normalized-key sets. Used only as an
# automated "worth a second look?" signal for the status table - never to
# auto-decide whether two postings are/aren't the same role. We deliberately
# flag low-overlap groups for manual review rather than trying to
# auto-judge "are these the same role".
# ------------------------------------------------------------------------------
jaccard <- function(a, b) {
  a <- unique(a); b <- unique(b)
  if (length(a) == 0 && length(b) == 0) return(NA_real_)
  length(intersect(a, b)) / length(union(a, b))
}

# Mean pairwise Jaccard (on normalized items) across all postings in a
# vacancy group. Returns NA for single-posting groups (nothing to compare).
compute_group_overlap <- function(item_lists) {
  n <- length(item_lists)
  if (n < 2) return(NA_real_)
  key_sets <- lapply(item_lists, normalize_bullet)
  pairs <- utils::combn(n, 2, simplify = FALSE)
  scores <- vapply(pairs, function(p) jaccard(key_sets[[p[1]]], key_sets[[p[2]]]), numeric(1))
  scores <- scores[!is.na(scores)]
  if (length(scores) == 0) return(NA_real_)
  mean(scores)
}

# ------------------------------------------------------------------------------
# Helper: read a previously-written per-PDF criteria .txt file (in the exact
# format written by write_criteria_file()) back into responsibilities/skills
# character vectors, plus scraped_job_ref and pages_used/total_pages from the
# header if present. Needed so files skipped this run (via
# skip_existing_output), or produced in an entirely separate earlier session,
# can still be pulled into a vacancy-group merge without re-calling the API -
# this is what lets the status table detect and fold in newly-added postings
# for an already-processed vacancy across runs, and what makes run_merge_pass()
# fully self-sufficient from disk alone (see that function's comment).
# ------------------------------------------------------------------------------
parse_criteria_txt <- function(path) {
  empty <- list(responsibilities = character(0), skills = character(0),
                scraped_job_ref = NA_character_, pages_used = NA_integer_, total_pages = NA_integer_)
  if (!file.exists(path)) return(empty)
  lines <- readLines(path, warn = FALSE)
  resp_start  <- which(lines == "RESPONSIBILITIES")
  skill_start <- which(grepl("^SKILLS", lines))
  if (length(resp_start) == 0 || length(skill_start) == 0) return(empty)
  resp_lines  <- lines[(resp_start[1] + 2):(skill_start[1] - 1)]
  skill_lines <- lines[(skill_start[1] + 2):length(lines)]
  parse_block <- function(block) {
    if (length(block) == 0) return(character(0))
    block <- block[nzchar(trimws(block))]
    block <- block[block != "(none extracted)"]
    sub("^-\\s", "", block)
  }

  header_lines <- lines[seq_len(resp_start[1] - 1)]

  ref_line <- grep("^Scraped Job Reference: ", header_lines, value = TRUE)
  scraped_job_ref <- if (length(ref_line) > 0) sub("^Scraped Job Reference: ", "", ref_line[1]) else NA_character_

  pages_line <- grep("^Pages Used: ", header_lines, value = TRUE)
  pages_used  <- NA_integer_
  total_pages <- NA_integer_
  if (length(pages_line) > 0) {
    m <- regmatches(pages_line[1], regexec("^Pages Used: (\\d+) of (\\d+)$", pages_line[1]))[[1]]
    if (length(m) == 3) {
      pages_used  <- as.integer(m[2])
      total_pages <- as.integer(m[3])
    }
  }

  list(responsibilities = parse_block(resp_lines), skills = parse_block(skill_lines),
       scraped_job_ref = scraped_job_ref, pages_used = pages_used, total_pages = total_pages)
}

# ------------------------------------------------------------------------------
# Helper: is a parsed extraction (from parse_criteria_txt(), or equivalent
# freshly-parsed API result) genuinely empty - both responsibilities AND
# skills came back empty? This can happen even on a fully "successful" run
# (valid JSON, no API/parse error) if the model legitimately found nothing
# to extract - often a sign that page-trimming excluded the real content
# page, the source PDF text extraction was garbled, or (rarely) the ad
# genuinely has neither section. Used by run_merge_pass() to classify such
# postings as NOT successful for group_status purposes (previously they
# were indistinguishable from a real, content-bearing success), and by
# get_retry_pdf_files() to decide whether a candidate still needs retrying.
# ------------------------------------------------------------------------------
is_empty_extraction <- function(parsed) {
  length(parsed$responsibilities) == 0 && length(parsed$skills) == 0
}

# ------------------------------------------------------------------------------
# Helper: grab PDF metadata (creation date, title) as a free, purely
# informational column for the status table. Explicitly NOT used for any
# recency/ordering decision - ad templates are often reused internal HR
# documents whose authoring date has nothing to do with when a specific job
# was actually advertised (confirmed case in our own corpus). Failures are
# swallowed to NA - metadata absence should never stop a run.
# ------------------------------------------------------------------------------
get_pdf_metadata_note <- function(pdf_path) {
  info <- tryCatch(pdftools::pdf_info(pdf_path), error = function(e) NULL)
  if (is.null(info)) return(paste0(basename(pdf_path), " (metadata unavailable)"))
  created <- if (!is.null(info$created) && length(info$created) == 1 && !is.na(info$created)) {
    format(info$created, "%Y-%m-%d")
  } else "NA"
  title <- info$keys$Title %||% "NA"
  sprintf("%s (created %s, title \"%s\")", basename(pdf_path), created, title)
}

# ------------------------------------------------------------------------------
# Helper: write the UNION-merged record for one vacancy group. Same
# bullet-list format as write_criteria_file(), plus a short header
# identifying the group and its overlap score.
# ------------------------------------------------------------------------------
write_merged_file <- function(out_path, vacancy_label, source_files, responsibilities, skills, overlap_score, n_postings, model = NULL) {
  lines <- c(
    paste0("Vacancy group: ", vacancy_label),
    if (!is.null(model)) paste0("Model: ", model),
    paste0("Source postings (", n_postings, "): ", paste(basename(source_files), collapse = "; ")),
    paste0("Overlap score (mean pairwise Jaccard across postings): ",
           if (is.na(overlap_score)) "NA (single posting)" else sprintf("%.2f", overlap_score)),
    "NOTE: UNION merge across all postings above - every distinct item ever",
    "stated across the group is retained (deduped on normalized exact match",
    "only).",
    "",
    "RESPONSIBILITIES",
    "----------------",
    if (length(responsibilities) == 0) "(none extracted)"
    else paste0("- ", responsibilities),
    "",
    "SKILLS (Essential / Desirable as labelled in the original ad)",
    "---------------------------------------------------------------",
    if (length(skills) == 0) "(none extracted)"
    else paste0("- ", skills)
  )
  writeLines(lines, out_path, useBytes = TRUE)
}

# ------------------------------------------------------------------------------
# Helper: persist the vacancy-group status table as CSV, upserting rows keyed
# by vacancy_number so re-running on a folder with a newly-added repost
# updates that group's row rather than duplicating it. Rows for vacancy
# groups not touched by this run are left as-is, since the table is meant to
# accumulate across runs/folders over time and support the eventual
# HR-database join.
# ------------------------------------------------------------------------------
upsert_status_table <- function(status_path, new_rows) {
  if (nrow(new_rows) == 0) return(invisible(NULL))
  dir.create(dirname(status_path), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(status_path)) {
    # na.strings = character(0): by default read.csv() treats any cell whose
    # entire value is the text "NA" as a true missing value, REGARDLESS of
    # colClasses = "character" - which silently turned quoted "NA" cells
    # (e.g. overlap_score for singleton vacancies) into unquoted NA on every
    # subsequent upsert of a row that wasn't even touched that run. Confirmed
    # via a round-trip test: a row's overlap_score field went from `"NA"` to
    # `NA` after one extra upsert cycle it had no part in. Disabling
    # na.strings here makes every cell read back exactly as written -
    # verbatim strings in, verbatim strings out - so nothing degrades across
    # repeated runs.
    existing <- utils::read.csv(status_path, stringsAsFactors = FALSE, colClasses = "character", na.strings = character(0))
    existing <- existing[!(existing$vacancy_number %in% new_rows$vacancy_number), , drop = FALSE]
    # Schema evolution, BOTH directions: a pre-existing status CSV from an
    # older version of this script (e.g. one written before the "model"
    # column, or the HR-enrichment columns, existed) won't have every column
    # new_rows now has - add those as blank. AND, symmetrically, if THIS
    # run's new_rows is missing a column the existing CSV already has (e.g.
    # add_hr_status_columns was on for an earlier run and is off for this
    # one), add blank columns to new_rows too - otherwise subsetting
    # existing[names(new_rows)] below would silently drop that column from
    # EVERY row, including historical ones this run never touched.
    missing_in_existing <- setdiff(names(new_rows), names(existing))
    for (col in missing_in_existing) existing[[col]] <- ""
    missing_in_new <- setdiff(names(existing), names(new_rows))
    for (col in missing_in_new) new_rows[[col]] <- ""
    all_cols <- union(names(existing), names(new_rows))
    combined <- rbind(existing[all_cols], new_rows[all_cols])
  } else {
    combined <- new_rows
  }
  combined <- combined[order(combined$vacancy_number), , drop = FALSE]
  utils::write.csv(combined, status_path, row.names = FALSE)
}

# ==============================================================================
# HR data helpers (category pre-filtering AND vacancy_status.csv enrichment)
# See "Category-based pre-filtering (HR data)" and "HR enrichment columns on
# vacancy_status.csv" in the CONFIG block above for the full design
# rationale, in particular why every uncertain case defaults to INCLUDE
# rather than EXCLUDE for the filter.
# ==============================================================================

# ------------------------------------------------------------------------------
# Helper: read one of the HR CSVs defensively. Column names are trimmed (the
# recruitment export's headers are known to contain literal spaces, e.g.
# "Job Reference"). Everything is read as character - we do our own
# key-normalization rather than trusting read.csv's type-guessing on
# reference numbers, which could otherwise silently drop leading zeros.
# na.strings is disabled for the same reason it was disabled in
# upsert_status_table() - these are raw external exports, not something we
# want read.csv silently converting literal "NA"-text cells into true NA on.
# ------------------------------------------------------------------------------
read_hr_csv <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf(
      paste(
        "HR data is required ('%s' does not exist) because",
        "CONFIG$filter_by_category and/or CONFIG$add_hr_status_columns is",
        "TRUE. Either add the file, or set both of those to FALSE."
      ),
      path
    ))
  }
  df <- utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character",
                        check.names = FALSE, na.strings = character(0))
  names(df) <- trimws(names(df))
  df
}

# ------------------------------------------------------------------------------
# Helper: fetch a column from an HR data.frame by name, case-insensitively
# and whitespace-normalized, rather than requiring an exact-case hardcoded
# match. A real header-casing mismatch ("Position category" vs "Position
# Category") already broke an earlier version of the category filter -
# every HR column lookup goes through this now so that class of bug can't
# recur for any of the columns used here. Errors loudly (listing the
# columns actually present) on no match or an ambiguous match, rather than
# silently returning NULL and failing somewhere downstream and confusingly.
# ------------------------------------------------------------------------------
get_hr_column <- function(df, target_name, source_label) {
  normalized <- tolower(trimws(names(df)))
  idx <- which(normalized == tolower(trimws(target_name)))
  if (length(idx) == 0) {
    stop(sprintf(
      "Expected column '%s' not found in %s (case-insensitive match). Columns present: %s",
      target_name, source_label, paste(names(df), collapse = ", ")
    ))
  }
  if (length(idx) > 1) {
    stop(sprintf(
      "Column '%s' matched more than one column in %s (case-insensitive): %s",
      target_name, source_label, paste(names(df)[idx], collapse = ", ")
    ))
  }
  df[[idx]]
}

# ------------------------------------------------------------------------------
# Helper: deduplicate a set of short field values (e.g. job/position
# references, categories, titles, dates) and join the distinct ones into a
# single display string. Used for the HR-enrichment columns on
# vacancy_status.csv, where multiple postings in one vacancy group - or
# multiple recruitment-export rows sharing one Job Reference - can carry
# differing values. The same posting should carry the same job_ref and
# position_category, and often the same position_title/job_title - i.e.
# divergence is expected and can be real signal, not noise (a real case in
# our own corpus already showed genuinely different content across postings
# sharing one vacancy_NNN group).
#
# Dedup here is a plain trimmed-EXACT match, deliberately not the
# typographic normalize_bullet() used for responsibilities/skills bullets -
# these are short reference codes/titles/dates, not free-text sentences, so
# no typographic-noise handling is needed. Blank/NA values are dropped
# entirely (never shown as an empty "; " segment). First-seen original
# wording is kept for each distinct value; order of first appearance is
# preserved.
# ------------------------------------------------------------------------------
dedup_concat_field <- function(values, sep = "; ") {
  values <- values[!is.na(values)]
  values <- values[nzchar(trimws(values))]
  if (length(values) == 0) return("")
  keys <- trimws(values)
  keep <- !duplicated(keys)
  paste(values[keep], collapse = sep)
}

# ------------------------------------------------------------------------------
# Helper: load and join the two HR files once, returning a list keyed by
# SERVER_FILENAME, each element holding both (a) the category-filter
# decision and (b) the raw HR-enrichment field VECTORS used for
# vacancy_status.csv (job_ref, position_ref, position_category,
# position_title, job_title, date_opened, date_closed). Enrichment fields
# are kept as VECTORS here (not yet joined into "; "-separated strings),
# because dedup/concatenation happens ONCE, across every posting in a
# vacancy GROUP together, via dedup_concat_field() at the point of use -
# pre-joining per file and re-splitting later would risk breaking on any
# field value that itself happens to contain "; ".
#
# NOTE on the two numbering systems: ras_fp_files.csv's VACANCY_NUMBER is
# the recruitment-system reference, joined here to the recruitment export's
# "Job Reference" column. This is a DIFFERENT number from the "vacancy_NNN"
# embedded in the PDF filename that get_vacancy_group_key() uses for repost
# grouping elsewhere in this script - the two must never be conflated. This
# function never touches the filename-embedded number at all; the link to a
# specific PDF is made purely via the full SERVER_FILENAME <-> PDF-basename
# match done in lookup_hr_record() below.
# ------------------------------------------------------------------------------
build_hr_lookup <- function(config) {
  mapping <- read_hr_csv(config$server_file_mapping_path)
  recruit <- read_hr_csv(config$recruitment_data_path)

  server_filename_col <- get_hr_column(mapping, "SERVER_FILENAME", config$server_file_mapping_path)
  vacancy_number_col  <- get_hr_column(mapping, "VACANCY_NUMBER", config$server_file_mapping_path)

  job_ref_col            <- get_hr_column(recruit, "Job Reference", config$recruitment_data_path)
  position_ref_col       <- get_hr_column(recruit, "Position Reference", config$recruitment_data_path)
  position_category_col  <- get_hr_column(recruit, "Position Category", config$recruitment_data_path)
  position_title_col     <- get_hr_column(recruit, "Position title", config$recruitment_data_path)
  job_title_col          <- get_hr_column(recruit, "Job Title", config$recruitment_data_path)
  date_opened_col        <- get_hr_column(recruit, "Short Date Advert Published", config$recruitment_data_path)
  date_closed_col        <- get_hr_column(recruit, "Short Date Advert Closed", config$recruitment_data_path)

  vacancy_number_key <- trimws(vacancy_number_col)
  job_reference_key  <- trimws(job_ref_col)
  position_category_key <- trimws(position_category_col)

  target_categories <- trimws(config$target_position_categories)

  # A Job Reference can appear on more than one recruitment-export row (e.g.
  # reposted within the recruitment system itself, independent of PDF-side
  # reposting). Recall-biased for the FILTER decision: a Job Reference
  # counts as "in scope" if ANY of its rows has a target category, not only
  # if all of them agree - excluding based on one disagreeing row risks
  # losing genuine content.
  valid_idx       <- nzchar(job_reference_key)
  known_job_refs  <- unique(job_reference_key[valid_idx])
  target_job_refs <- unique(job_reference_key[valid_idx & position_category_key %in% target_categories])

  decide_one <- function(vn) {
    if (!nzchar(vn)) return("include_no_vacancy_number")  # not user-configurable - see CONFIG comment
    if (!(vn %in% known_job_refs)) {
      return(if (isTRUE(config$category_filter_include_no_recruitment_match)) {
        "include_no_recruitment_match"
      } else {
        "exclude_no_recruitment_match"
      })
    }
    if (vn %in% target_job_refs) return("include_target_category")
    "exclude_other_category"
  }

  n <- nrow(mapping)
  per_file <- vector("list", n)
  for (i in seq_len(n)) {
    vn <- vacancy_number_key[i]
    match_idx <- if (nzchar(vn)) which(job_reference_key == vn) else integer(0)
    per_file[[i]] <- list(
      decision           = decide_one(vn),
      job_ref            = vn,
      position_ref       = position_ref_col[match_idx],
      position_category  = position_category_col[match_idx],
      position_title     = position_title_col[match_idx],
      job_title          = job_title_col[match_idx],
      date_opened        = date_opened_col[match_idx],
      date_closed        = date_closed_col[match_idx]
    )
  }
  names(per_file) <- server_filename_col
  per_file
}

# ------------------------------------------------------------------------------
# Helper: look up the HR record for one PDF on disk, matching against the
# per-SERVER_FILENAME list built by build_hr_lookup(). Tries an exact
# basename match first, then falls back to matching without file extension
# (in case SERVER_FILENAME in the source data omits ".pdf"). Returns NULL if
# no row at all exists for this PDF in ras_fp_files.csv - callers decide how
# to treat that (the filter treats it as RECALL-BIASED include; enrichment
# treats it as "nothing to add").
# ------------------------------------------------------------------------------
lookup_hr_record <- function(pdf_path, hr_lookup) {
  fname <- basename(pdf_path)
  if (!is.null(hr_lookup[[fname]])) return(hr_lookup[[fname]])

  fname_noext <- tools::file_path_sans_ext(fname)
  noext_names <- tools::file_path_sans_ext(names(hr_lookup))
  idx <- which(noext_names == fname_noext)
  if (length(idx) >= 1) return(hr_lookup[[idx[1]]])

  NULL
}

# ------------------------------------------------------------------------------
# Helper: the category-filter include/exclude decision for one PDF. A PDF
# with no HR record at all (lookup_hr_record() returns NULL) is a case the
# filter has no direct category evidence about - governed by
# config$category_filter_include_no_server_mapping (default TRUE, i.e.
# recall-biased include; see CONFIG comment for why this and
# category_filter_include_no_recruitment_match are user-controlled rather
# than hardcoded).
#
# Every decision string produced anywhere in this filter (here and in
# build_hr_lookup()'s decide_one()) is prefixed "include_" or "exclude_" by
# construction, so `include` is simply derived from that prefix rather than
# enumerated case by case - this also means any future new decision type
# only needs to follow the same naming convention to work correctly here
# with no changes needed in this function.
# ------------------------------------------------------------------------------
decision_for_pdf <- function(pdf_path, hr_lookup, config) {
  rec <- lookup_hr_record(pdf_path, hr_lookup)
  if (is.null(rec)) {
    decision <- if (isTRUE(config$category_filter_include_no_server_mapping)) {
      "include_no_server_mapping"
    } else {
      "exclude_no_server_mapping"
    }
    return(list(include = startsWith(decision, "include"), decision = decision))
  }
  list(include = startsWith(rec$decision, "include"), decision = rec$decision)
}

# ------------------------------------------------------------------------------
# Helper: gather the HR-enrichment columns for a vacancy GROUP (one or more
# member PDFs), for vacancy_status.csv. Collects every candidate value for
# each field across every member's HR record (including any PDF with no HR
# record at all - it simply contributes nothing from the mapping side),
# then deduplicates and concatenates ONCE across the whole group via
# dedup_concat_field() - see that function's comment for why divergence
# within a group is preserved rather than collapsed.
#
# job_ref fallback: for any member whose HR-mapping-derived job_ref ends up
# blank - either no HR record was found at all, or a record was found but
# its VACANCY_NUMBER was blank - the job reference scraped from that PDF's
# extracted text (file_results[[path]]$scraped_job_ref, computed once per
# PDF in the main loop via extract_job_reference_from_text()) is used
# instead, and folded into the SAME job_ref dedup/concat pool as any
# mapping-derived values. Only job_ref can be recovered this way - the
# other enrichment fields (position_ref, position_category, etc.)
# genuinely cannot be filled in without a server-mapping row to join
# through the recruitment export, so they stay blank for these postings.
# ------------------------------------------------------------------------------
gather_hr_enrichment <- function(member_paths, hr_lookup, file_results) {
  recs <- lapply(member_paths, lookup_hr_record, hr_lookup = hr_lookup)

  fallback_job_refs <- mapply(function(path, rec) {
    known_ref <- if (!is.null(rec)) rec$job_ref else NA_character_
    if (!is.null(known_ref) && !is.na(known_ref) && nzchar(trimws(known_ref))) {
      return(NA_character_)  # already have a usable job_ref from the HR mapping
    }
    scraped <- file_results[[path]]$scraped_job_ref
    if (is.null(scraped)) NA_character_ else scraped
  }, member_paths, recs, SIMPLIFY = TRUE)

  recs_present <- recs[!vapply(recs, is.null, logical(1))]

  fields <- c("job_ref", "position_ref", "position_category", "position_title",
              "job_title", "date_opened", "date_closed")
  out <- setNames(vector("list", length(fields)), fields)
  for (f in fields) {
    values <- unlist(lapply(recs_present, function(r) r[[f]]))
    if (f == "job_ref") values <- c(values, fallback_job_refs)
    out[[f]] <- dedup_concat_field(values)
  }
  out
}

# ==============================================================================
# Standalone merge / status-table pass
# ==============================================================================

# ------------------------------------------------------------------------------
# Rebuilds vacancy-group merges and vacancy_status.csv ENTIRELY from what is
# currently on disk: group membership comes from the PDFs currently in
# config$input_dir (same as always), and each member's content comes from
# parsing its .txt criteria file in config$output_dir if one exists (via
# parse_criteria_txt()) - NOT from any in-memory state left over from a
# particular run. It makes no API calls and needs no ANTHROPIC_API_KEY.
#
# Why this exists (batch/crash-safety): previously, merging only happened
# inline at the very end of main(), after the full per-PDF extraction loop
# finished. If that loop crashed partway through, every PDF successfully
# processed before the crash still had its .txt file safely written to disk
# - but the run never reached the merge step, so vacancy_status.csv and the
# merged .txt files simply wouldn't reflect that work until some LATER run
# happened to complete all the way through. Nothing was ever silently lost
# (the source .txt files are the durable record), but it WAS invisible/stale
# until then, and if you never re-ran the full script, it would stay that
# way indefinitely.
#
# run_merge_pass() fixes this by being independently callable at any time -
# e.g. `run_merge_pass(CONFIG)` in an R session, with no need to run main()
# or touch the API at all. Call it right after a crash, between batches, or
# just periodically during a long run, to get an up-to-date
# vacancy_status.csv reflecting everything completed so far, cheaply. It is
# also called automatically at the end of a normal main() run (see below),
# so a single successful run behaves exactly as before.
#
# Why this is SAFE to call repeatedly / from separate batches, without the
# "does it overwrite or duplicate, and does a run with fewer postings give a
# group less weight" concern: because group membership and content are
# ALWAYS re-derived FRESH and COMPLETE from disk on every call (never a
# partial view of "just this run's/batch's files"), the row/file
# upsert_status_table() and write_merged_file() write for a given vacancy
# number is always a full, correct, up-to-date recomputation - never a
# partial subset that could lose information a previous call's row had. That
# is precisely why upsert_status_table()'s per-vacancy REPLACE (rather than
# an append or a manual field-by-field merge) is the right behaviour, not a
# bug: the new row already IS the complete merge of everything currently on
# disk for that vacancy, so replacing the old row with it is strictly a
# refresh, never a loss. The union/dedup logic in dedup_union() also already
# prevents a duplicate PDF (or a repost containing largely the same content)
# from inflating item counts or skewing the merge - each distinct item is
# counted once regardless of how many postings, or how many separate
# runs/batches, contributed it.
#
# Known limitation: a PDF whose .txt output doesn't yet exist is reported
# simply as "not yet processed" for group_status purposes - this pass has no
# way to distinguish "genuinely failed extraction" from "not attempted yet"
# from disk state alone, since only successful extractions persist a .txt
# file. This is not a new gap - group_status already treated "processed" and
# "skipped_existing" identically before this refactor. Check the run log
# from the actual extraction run if you need the specific per-file failure
# reason for a "partial"/"failed" group.
#
# ASSUMPTION: every PDF that has ever been part of a vacancy group remains
# present in config$input_dir. This was already an implicit assumption
# before this refactor too (pdf_files has always been a fresh, full folder
# listing every run, not a persistent record of "files ever seen"). If your
# batching plan involves moving or deleting PDFs out of the folder once
# they're processed, flag that - this design would need adapting (e.g.
# tracking membership via output_dir filenames too, not input_dir alone).
#
# If config$filter_by_category is TRUE, that filter is reapplied here too
# (identically to how it's applied during extraction), BEFORE group
# membership is computed - a PDF excluded from processing is also excluded
# from its group's membership count, not counted as "not yet processed".
# Without this, an excluded PDF would permanently pin its group's status to
# "partial" forever, since it will never get a .txt output by design.
#
# scope_pdf_files (optional): when NULL (the default), every vacancy group
# in input_dir is recomputed and rewritten - a full refresh, as described
# above. When given a vector of PDF paths, only groups containing at least
# one of those PDFs are actually recomputed/rewritten; every other group's
# existing merged file and vacancy_status.csv row is left completely
# untouched (not deleted, not recomputed - upsert_status_table() only ever
# touches rows for vacancy numbers present in this call's output). This
# does NOT reintroduce the group-truncation risk: group MEMBERSHIP is still
# always derived from the complete input_dir listing first, exactly as
# without scoping - scope_pdf_files only decides which of the resulting
# (correctly, fully-membered) groups are worth spending time recomputing
# this call. This exists because the dominant per-group cost,
# get_pdf_metadata_note() (opens every member PDF via pdftools::pdf_info()
# for the pdf_metadata column), was being paid for every group in the
# folder on every call - including a call that only extracted 2 PDFs out
# of 5,000. main() passes its own resolved extraction set here
# automatically, so a normal full run is unaffected (scope = everything, as
# before) but a target_files_only/retry_mode run only touches the handful
# of groups it actually has new information about.
# ------------------------------------------------------------------------------
run_merge_pass <- function(config, scope_pdf_files = NULL) {
  if (!isTRUE(config$group_by_vacancy)) {
    log_msg("group_by_vacancy is FALSE - nothing to merge.")
    return(invisible(NULL))
  }
  if (!dir.exists(config$input_dir)) {
    stop(sprintf("Input directory '%s' does not exist.", config$input_dir))
  }

  pdf_files <- list.files(config$input_dir, pattern = "\\.pdf$",
                          full.names = TRUE, ignore.case = TRUE)
  if (length(pdf_files) == 0) {
    log_msg(paste("No PDF files found in", config$input_dir, "- nothing to merge."))
    return(invisible(NULL))
  }

  # HR data is loaded once here (before filtering) if needed for EITHER
  # purpose below: reapplying the category filter to group membership, or
  # enrichment columns further down.
  hr_lookup <- NULL
  if (isTRUE(config$filter_by_category) || isTRUE(config$add_hr_status_columns)) {
    log_msg("Loading HR data...")
    hr_lookup <- build_hr_lookup(config)
  }

  # Reapply the SAME category filter used during extraction (see
  # CONFIG$filter_by_category), so a PDF that was deliberately excluded from
  # processing is also excluded from vacancy-group membership here. Without
  # this, an excluded PDF would sit in input_dir forever with no .txt output
  # (by design - it was never meant to be processed), get counted as
  # "not_yet_processed", and permanently pin its whole group's status to
  # "partial" even though every INCLUDED posting in that group completed
  # successfully. This mirrors what the old inline merge (before
  # run_merge_pass() existed) did implicitly, since pdf_files there was
  # already the filtered list by the time merging happened.
  if (isTRUE(config$filter_by_category)) {
    include_flags <- vapply(pdf_files, function(p) decision_for_pdf(p, hr_lookup, config)$include, logical(1))
    n_before <- length(pdf_files)
    pdf_files <- pdf_files[include_flags]
    log_msg(sprintf(
      "Category filter reapplied for merge: %d of %d PDF(s) in scope (%d excluded, consistent with extraction).",
      length(pdf_files), n_before, n_before - length(pdf_files)
    ))
    if (length(pdf_files) == 0) {
      log_msg("No PDFs remain after category filtering - nothing to merge.")
      return(invisible(NULL))
    }
  }

  group_info <- lapply(pdf_files, get_vacancy_group_key, config = config)
  names(group_info) <- pdf_files
  group_keys <- vapply(group_info, function(g) g$key, character(1))

  # Determine which groups this call actually needs to recompute. See
  # scope_pdf_files in the function comment above - membership (group_keys,
  # just computed) always covers the complete folder regardless; this only
  # narrows which groups get the expensive per-group work below.
  if (!is.null(scope_pdf_files)) {
    target_group_keys <- unique(group_keys[pdf_files %in% scope_pdf_files])
    log_msg(sprintf(
      "run_merge_pass: scoped to %d PDF(s), touching %d of %d known vacancy group(s).",
      length(scope_pdf_files), length(target_group_keys), length(unique(group_keys))
    ))
    if (length(target_group_keys) == 0) {
      log_msg("run_merge_pass: none of the scoped PDFs matched a known vacancy group - nothing to update.")
      return(invisible(NULL))
    }
  } else {
    target_group_keys <- unique(group_keys)
  }

  # Rebuild file_results from disk, but ONLY for members of target groups -
  # this is what actually saves the work for a scoped call (skips
  # parse_criteria_txt() reads, and later get_pdf_metadata_note()'s
  # pdftools::pdf_info() calls, for every PDF outside the groups this call
  # cares about). A PDF counts as "processed" (from this pass's point of
  # view) iff its .txt criteria output currently exists AND is a genuine
  # (non-empty) extraction - see "Known limitation" note above and
  # is_empty_extraction()'s comment. A "processed_empty" status (file
  # exists, both sections empty) does NOT count toward n_successful below,
  # same as "not_yet_processed" - this is what lets a singleton group whose
  # one posting extracted nothing correctly show as "failed" rather than
  # "complete".
  scoped_pdf_files <- pdf_files[group_keys %in% target_group_keys]
  file_results <- setNames(vector("list", length(scoped_pdf_files)), scoped_pdf_files)
  for (pdf_path in scoped_pdf_files) {
    base_name <- tools::file_path_sans_ext(basename(pdf_path))
    out_path  <- file.path(config$output_dir, paste0(base_name, ".txt"))
    if (file.exists(out_path)) {
      parsed <- parse_criteria_txt(out_path)
      file_results[[pdf_path]] <- list(
        status = if (is_empty_extraction(parsed)) "processed_empty" else "processed",
        responsibilities = parsed$responsibilities,
        skills = parsed$skills, pages_used = parsed$pages_used, total_pages = parsed$total_pages,
        scraped_job_ref = parsed$scraped_job_ref
      )
    } else {
      file_results[[pdf_path]] <- list(
        status = "not_yet_processed", responsibilities = character(0), skills = character(0),
        pages_used = NA_integer_, total_pages = NA_integer_, scraped_job_ref = NA_character_
      )
    }
  }

  log_msg("Merging postings within each vacancy group...")
  if (!dir.exists(config$merged_dir)) dir.create(config$merged_dir, recursive = TRUE)

  status_rows <- list()
  for (key in target_group_keys) {
    member_paths <- pdf_files[group_keys == key]
    members <- file_results[member_paths]

    vacancy_number <- group_info[[member_paths[1]]]$vacancy_number
    vacancy_label  <- if (is.na(vacancy_number)) sub("^UNGROUPED::", "", key) else vacancy_number

    resp_lists  <- lapply(members, function(m) m$responsibilities)
    skill_lists <- lapply(members, function(m) m$skills)
    combo_lists <- Map(c, resp_lists, skill_lists)

    merged_resp   <- dedup_union(resp_lists)
    merged_skill  <- dedup_union(skill_lists)
    overlap_score <- compute_group_overlap(combo_lists)

    n_postings   <- length(member_paths)
    statuses     <- vapply(members, function(m) m$status, character(1))
    n_successful <- sum(statuses == "processed")
    group_status <- if (n_successful == 0) "failed" else if (n_successful < n_postings) "partial" else "complete"
    flagged      <- isTRUE(!is.na(overlap_score) && overlap_score < config$overlap_flag_threshold)

    pages_summary <- paste(vapply(members, function(m) {
      if (is.na(m$pages_used) || is.na(m$total_pages)) "NA" else sprintf("%d/%d", m$pages_used, m$total_pages)
    }, character(1)), collapse = "; ")

    metadata_note <- paste(vapply(member_paths, get_pdf_metadata_note, character(1)), collapse = " | ")

    # HR-enrichment columns (job_ref, position_ref, position_category,
    # position_title, job_title, date_opened, date_closed) - always
    # computed (blank when add_hr_status_columns is off or no HR record
    # was found) so the status table's column set never changes shape
    # between runs based on this toggle. See gather_hr_enrichment() for
    # the dedup-then-concatenate logic across a group's postings.
    hr_fields <- if (isTRUE(config$add_hr_status_columns)) {
      gather_hr_enrichment(member_paths, hr_lookup, file_results)
    } else {
      list(job_ref = "", position_ref = "", position_category = "",
           position_title = "", job_title = "", date_opened = "", date_closed = "")
    }

    merged_out_path <- file.path(config$merged_dir, sprintf("vacancy_%s_MERGED.txt", vacancy_label))
    write_merged_file(merged_out_path, vacancy_label, member_paths, merged_resp, merged_skill, overlap_score, n_postings, model = config$model)

    if (n_postings > 1) {
      log_msg(sprintf(
        "  -> vacancy %s: %d postings, overlap=%s, status=%s%s -> %s",
        vacancy_label, n_postings,
        if (is.na(overlap_score)) "NA" else sprintf("%.2f", overlap_score),
        group_status, if (flagged) " [FLAGGED FOR REVIEW]" else "",
        merged_out_path
      ))
    }

    # Output columns (vacancy_status.csv):
    #   vacancy_number      - grouping key: a real Cambridge vacancy reference
    #                          number, or a synthesized singleton ID if none
    #                          was found/matched in the PDF
    #   model                - which Claude model produced this row's extraction
    #   source_files          - semicolon-separated basenames of every PDF
    #                            merged into this vacancy group
    #   n_postings             - how many source PDFs were merged into this row
    #   status                  - "complete" / "partial" / "failed" for this group
    #   pages_used_summary       - which PDF pages were sent to the model, one
    #                              entry per source file
    #   overlap_score              - Jaccard overlap between reposts in a
    #                                multi-posting group; NA for singletons
    #   flagged_for_review           - TRUE if repost grouping looks uncertain
    #                                  and needs a human check (not a comment
    #                                  on extraction quality)
    #   n_responsibilities /            - bullet counts in the merged
    #   n_skills                          RESPONSIBILITIES/SKILLS sections
    #   pdf_metadata                        - free-text diagnostic note per
    #                                        source PDF
    #   job_ref / position_ref /              - HR-enrichment columns, always
    #   position_category / position_title /    present (blank if
    #   job_title / date_opened /                 add_hr_status_columns is off
    #   date_closed                                 or no HR match found), so
    #                                                the column set never
    #                                                changes shape between runs
    #   last_run_timestamp                            - when this row was last
    #                                                  (re)written
    status_rows[[length(status_rows) + 1]] <- data.frame(
      vacancy_number     = vacancy_label,
      model              = config$model,
      source_files       = paste(basename(member_paths), collapse = "; "),
      n_postings         = as.character(n_postings),
      status             = group_status,
      pages_used_summary = pages_summary,
      overlap_score      = if (is.na(overlap_score)) "NA" else sprintf("%.3f", overlap_score),
      flagged_for_review = as.character(flagged),
      n_responsibilities = as.character(length(merged_resp)),
      n_skills           = as.character(length(merged_skill)),
      pdf_metadata       = metadata_note,
      job_ref            = hr_fields$job_ref,
      position_ref       = hr_fields$position_ref,
      position_category  = hr_fields$position_category,
      position_title     = hr_fields$position_title,
      job_title          = hr_fields$job_title,
      date_opened        = hr_fields$date_opened,
      date_closed        = hr_fields$date_closed,
      last_run_timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      stringsAsFactors = FALSE
    )
  }

  if (length(status_rows) > 0) {
    status_df <- do.call(rbind, status_rows)
    upsert_status_table(config$status_table_path, status_df)
    log_msg(sprintf(
      "Updated vacancy status table: %s (%d group(s) touched this call%s)",
      config$status_table_path, nrow(status_df),
      if (is.null(scope_pdf_files)) "" else ", scoped run - all other groups left untouched"
    ))
    n_flagged <- sum(status_df$flagged_for_review == "TRUE")
    if (n_flagged > 0) {
      log_warn(sprintf(
        "%d vacancy group(s) flagged for manual review (low overlap between postings) - see %s",
        n_flagged, config$status_table_path
      ))
    }
    n_incomplete <- sum(status_df$status %in% c("failed", "partial"))
    if (n_incomplete > 0) {
      log_msg(sprintf(
        "%d vacancy group(s) have at least one posting not yet successfully processed (status 'partial' or 'failed') - re-run extraction to pick those up.",
        n_incomplete
      ))
    }
  }

  invisible(status_rows)
}

# ==============================================================================
# Extraction-scope resolution: retry_mode and target_files_only
# ==============================================================================

# ------------------------------------------------------------------------------
# Helper: for retry_mode - determine which PDFs need (re)processing, based on
# vacancy_status.csv's GROUP-level status narrowed down to the specific
# member PDFs that don't currently have a genuine (non-empty) extraction on
# disk. See CONFIG$retry_mode comment for the full design rationale.
#
# Deliberately does NOT just return every PDF listed under a target-status
# group: for a "partial" group, some members already succeeded with real
# content - retrying those too would waste API calls for no benefit (the
# PDF hasn't changed, the result would presumably be identical). Only
# members that are missing or empty get returned.
#
# Every candidate is re-checked against CURRENT disk state at call time
# (never just trusted from vacancy_status.csv, which reflects whenever
# run_merge_pass() last ran and so could be slightly stale) - so this is
# safe to call repeatedly, including immediately after manually fixing a
# file some other way.
# ------------------------------------------------------------------------------
get_retry_pdf_files <- function(config) {
  if (!file.exists(config$status_table_path)) {
    log_warn(sprintf(
      "retry_mode is TRUE but '%s' does not exist yet - nothing to retry. Run a normal extraction pass first.",
      config$status_table_path
    ))
    return(character(0))
  }

  status_df <- utils::read.csv(config$status_table_path, stringsAsFactors = FALSE,
                               colClasses = "character", na.strings = character(0))
  required_cols <- c("status", "source_files")
  missing_cols <- setdiff(required_cols, names(status_df))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "'%s' is missing expected column(s): %s - is this a valid vacancy_status.csv?",
      config$status_table_path, paste(missing_cols, collapse = ", ")
    ))
  }

  target_rows <- status_df[status_df$status %in% config$retry_target_statuses, , drop = FALSE]
  if (nrow(target_rows) == 0) {
    log_msg(sprintf(
      "retry_mode: no vacancy groups currently have status in {%s} - nothing to retry.",
      paste(config$retry_target_statuses, collapse = ", ")
    ))
    return(character(0))
  }

  candidate_basenames <- unique(trimws(unlist(strsplit(target_rows$source_files, ";\\s*"))))
  candidate_basenames <- candidate_basenames[nzchar(candidate_basenames)]

  all_pdfs   <- list.files(config$input_dir, pattern = "\\.pdf$", full.names = TRUE, ignore.case = TRUE)
  candidates <- all_pdfs[basename(all_pdfs) %in% candidate_basenames]

  missing_from_disk <- setdiff(candidate_basenames, basename(all_pdfs))
  if (length(missing_from_disk) > 0) {
    log_warn(sprintf(
      "retry_mode: %d file(s) recorded under a target status in %s are no longer present in %s and will be skipped: %s",
      length(missing_from_disk), config$status_table_path, config$input_dir,
      paste(utils::head(missing_from_disk, 10), collapse = ", ")
    ))
  }

  needs_retry <- vapply(candidates, function(pdf_path) {
    base_name <- tools::file_path_sans_ext(basename(pdf_path))
    out_path  <- file.path(config$output_dir, paste0(base_name, ".txt"))
    if (!file.exists(out_path)) return(TRUE)
    is_empty_extraction(parse_criteria_txt(out_path))
  }, logical(1))

  retry_files <- candidates[needs_retry]
  log_msg(sprintf(
    paste(
      "retry_mode: %d vacancy group(s) matched status {%s}, covering %d candidate PDF(s);",
      "%d actually need (re)processing (missing or empty output); %d already have real",
      "content and will be left untouched."
    ),
    nrow(target_rows), paste(config$retry_target_statuses, collapse = ", "), length(candidates),
    length(retry_files), length(candidates) - length(retry_files)
  ))

  retry_files
}

# ------------------------------------------------------------------------------
# Helper: resolve the set of PDFs main()'s extraction loop should process
# this run - the normal full-folder listing, unless target_files_only or
# retry_mode narrows it. Mutually exclusive by design (errors if both are
# TRUE): target_files_only names exact files directly, which doesn't
# compose with retry_mode's status-driven selection, and trying to support
# both at once would make it unclear which one "wins".
#
# category filter and add_hr_status_columns note: filter_by_category is
# intentionally NOT (re)applied when either mode narrows the file set - see
# CONFIG$target_files_only / CONFIG$retry_mode comments: a file you named
# explicitly, or one already known from vacancy_status.csv, shouldn't be
# silently re-excluded by a category rule that may have changed since it
# was first processed.
#
# IMPORTANT: this ONLY affects what main()'s extraction loop processes.
# run_merge_pass() deliberately does NOT call this - it always scans the
# complete input_dir (see that function's comment), so a narrowed
# extraction run still produces a fully correct, complete
# vacancy_status.csv rather than one that looks like every unaffected
# group suddenly has fewer postings than it really does.
# ------------------------------------------------------------------------------
resolve_extraction_pdf_files <- function(config) {
  if (isTRUE(config$target_files_only) && isTRUE(config$retry_mode)) {
    stop(paste(
      "CONFIG$target_files_only and CONFIG$retry_mode cannot both be TRUE at",
      "once - target_files_only names exact files directly, which doesn't",
      "compose with retry_mode's status-driven selection. Use one or the other."
    ))
  }

  if (isTRUE(config$target_files_only)) {
    if (length(config$target_files) == 0) {
      log_warn("target_files_only is TRUE but CONFIG$target_files is empty - nothing to process.")
      return(character(0))
    }
    resolved <- vapply(config$target_files, function(f) {
      if (file.exists(f)) f else file.path(config$input_dir, f)
    }, character(1), USE.NAMES = FALSE)
    found   <- resolved[file.exists(resolved)]
    missing <- resolved[!file.exists(resolved)]
    if (length(missing) > 0) {
      log_warn(sprintf("target_files_only: %d file(s) not found, will be skipped: %s",
                       length(missing), paste(missing, collapse = ", ")))
    }
    return(found)
  }

  if (isTRUE(config$retry_mode)) {
    return(get_retry_pdf_files(config))
  }

  list.files(config$input_dir, pattern = "\\.pdf$", full.names = TRUE, ignore.case = TRUE)
}

# ------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------
main <- function(config) {
  api_key <- Sys.getenv("ANTHROPIC_API_KEY")
  if (identical(api_key, "")) {
    stop("ANTHROPIC_API_KEY environment variable is not set. ",
         "Set it before running this script, e.g. Sys.setenv(ANTHROPIC_API_KEY = '...').")
  }

  if (!dir.exists(config$input_dir)) {
    stop(sprintf("Input directory '%s' does not exist.", config$input_dir))
  }
  if (!dir.exists(config$output_dir)) {
    dir.create(config$output_dir, recursive = TRUE)
    message(sprintf("Created output directory: %s", config$output_dir))
  }
  if (isTRUE(config$save_extracted_text) && !dir.exists(config$extracted_text_dir)) {
    dir.create(config$extracted_text_dir, recursive = TRUE)
    message(sprintf("Created extracted-text debug directory: %s", config$extracted_text_dir))
  }

  # Set up the log file (if enabled) via the module-level .log_con, used by
  # log_msg()/log_warn() everywhere below. Guaranteed to close even on error.
  .log_con <<- setup_log_file(config)
  if (!is.null(.log_con)) {
    on.exit({
      close(.log_con)
      .log_con <<- NULL
    }, add = TRUE)
  }

  pdf_files <- resolve_extraction_pdf_files(config)

  if (length(pdf_files) == 0) {
    if (isTRUE(config$retry_mode)) {
      log_msg("retry_mode: nothing to retry - done.")
    } else if (isTRUE(config$target_files_only)) {
      log_msg("target_files_only: no valid files to process - done.")
    } else {
      log_msg(paste("No PDF files found in", config$input_dir))
    }
    return(invisible(NULL))
  }

  log_msg(sprintf("Found %d PDF file(s). Starting extraction...", length(pdf_files)))

  # --- HR data loading (shared by category filter and status enrichment) ----
  # Both features join the same two files, so load/join once here if either
  # is enabled, and reuse the result below (filtering) and later (merge-loop
  # enrichment columns).
  hr_lookup <- NULL
  if (isTRUE(config$filter_by_category) || isTRUE(config$add_hr_status_columns)) {
    log_msg("Loading HR data (ras_fp_files.csv + recruitment export)...")
    hr_lookup <- build_hr_lookup(config)
  }

  # --- Category-based pre-filtering (HR data) --------------------------------
  # See CONFIG$filter_by_category comment for full rationale. This can only
  # ever REMOVE files from pdf_files below (never add), and every decision -
  # not just exclusions - is logged to category_filter_log_path for audit,
  # since (unlike page-trimming) getting this wrong means a file is never
  # attempted at all rather than merely being trimmed.
  #
  # Skipped when target_files_only or retry_mode already narrowed pdf_files
  # (see resolve_extraction_pdf_files() comment) - a file you named
  # explicitly, or one already known incomplete from vacancy_status.csv,
  # shouldn't be silently re-excluded by a category rule.
  if (isTRUE(config$filter_by_category) && !isTRUE(config$target_files_only) && !isTRUE(config$retry_mode)) {
    per_file_decisions <- lapply(pdf_files, function(p) {
      d <- decision_for_pdf(p, hr_lookup, config)
      data.frame(pdf_file = basename(p), decision = d$decision, include = d$include,
                 stringsAsFactors = FALSE)
    })
    # Output columns (category_filter_decisions.csv):
    #   pdf_file   - basename of the PDF this decision applies to
    #   decision   - the category-filter rule outcome for this file
    #   include    - TRUE/FALSE, whether the file proceeds to extraction
    decisions_df <- do.call(rbind, per_file_decisions)

    dir.create(dirname(config$category_filter_log_path), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(decisions_df, config$category_filter_log_path, row.names = FALSE)
    log_msg(sprintf("Full per-PDF category filter decision log written to: %s", config$category_filter_log_path))

    summary_counts <- table(decisions_df$decision)
    log_msg("Category filter decision counts:")
    for (nm in names(summary_counts)) {
      log_msg(sprintf("  %s: %d", nm, summary_counts[[nm]]))
    }

    n_before   <- length(pdf_files)
    pdf_files  <- pdf_files[decisions_df$include]
    n_excluded <- n_before - length(pdf_files)
    log_msg(sprintf(
      paste(
        "Category filter: keeping %d of %d PDF(s) for processing (%d excluded",
        "on confirmed category grounds). Recommend spot-checking %s before a",
        "full production run."
      ),
      length(pdf_files), n_before, n_excluded, config$category_filter_log_path
    ))

    if (length(pdf_files) == 0) {
      log_msg("No PDFs remain after category filtering - nothing to process.")
      return(invisible(NULL))
    }
  }

  # Pre-scan: group files by vacancy number (see "Repost grouping & merging
  # helpers" above). This only determines grouping for the merge step later -
  # it does NOT change how each PDF is individually extracted below.
  group_info <- lapply(pdf_files, get_vacancy_group_key, config = config)
  names(group_info) <- pdf_files
  group_keys <- vapply(group_info, function(g) g$key, character(1))
  if (isTRUE(config$group_by_vacancy)) {
    n_groups <- length(unique(group_keys))
    n_multi  <- sum(table(group_keys) > 1)
    log_msg(sprintf(
      "Grouped %d PDF(s) into %d vacancy group(s) (%d group(s) have more than one posting).",
      length(pdf_files), n_groups, n_multi
    ))
  }

  # Tracks the outcome + extracted items for every PDF this run, whether
  # freshly processed, backfilled from disk (skip_existing_output), or
  # failed - used by the merge/status-table step after the loop.
  file_results <- setNames(vector("list", length(pdf_files)), pdf_files)

  # n_attempted_this_run: counts only files that reach the API call this
  # run (see CONFIG$enable_pdf_limit - a free skip_existing_output skip
  # never increments this). files_touched_this_run: every PDF the loop
  # actually made a decision about this run (skipped-as-done OR attempted)
  # - becomes run_merge_pass()'s scope_pdf_files below, so a PDF the cap
  # stopped us from reaching is correctly left out of both.
  n_attempted_this_run   <- 0
  files_touched_this_run <- character(0)

  for (pdf_index in seq_along(pdf_files)) {
    pdf_path  <- pdf_files[pdf_index]
    base_name <- tools::file_path_sans_ext(basename(pdf_path))
    out_path  <- file.path(config$output_dir, paste0(base_name, ".txt"))

    will_skip <- file.exists(out_path) && isTRUE(config$skip_existing_output) && !isTRUE(config$retry_mode)

    if (!will_skip && isTRUE(config$enable_pdf_limit) && n_attempted_this_run >= config$max_pdfs_per_run) {
      log_msg(sprintf(
        "PDF limit reached (%d processed this run) - stopping here. %d file(s) remain for a future run.",
        config$max_pdfs_per_run, length(pdf_files) - pdf_index + 1
      ))
      break
    }

    files_touched_this_run <- c(files_touched_this_run, pdf_path)
    log_msg(sprintf("Processing file %d of %d: %s", pdf_index, length(pdf_files), basename(pdf_path)))

    if (will_skip) {
      log_msg(sprintf(
        "  -> output already exists at %s - skipping (skip_existing_output = TRUE)",
        out_path
      ))
      existing <- parse_criteria_txt(out_path)
      # pages_used/total_pages/scraped_job_ref are now recovered directly
      # from the header written by write_criteria_file() (previously these
      # were hardcoded NA here, a documented known limitation - now closed,
      # since that header data persists across runs/sessions regardless of
      # skip_existing_output). Will still be NA for .txt files written
      # before this header change existed.
      file_results[[pdf_path]] <- list(
        status = "skipped_existing", responsibilities = existing$responsibilities,
        skills = existing$skills, pages_used = existing$pages_used, total_pages = existing$total_pages,
        scraped_job_ref = existing$scraped_job_ref
      )
      next
    } else if (file.exists(out_path)) {
      log_msg(sprintf(
        "  -> output already exists at %s - will overwrite (%s)",
        out_path,
        if (isTRUE(config$retry_mode)) "retry_mode = TRUE, forcing reprocessing regardless of skip_existing_output"
        else "skip_existing_output = FALSE"
      ))
    }

    n_attempted_this_run <- n_attempted_this_run + 1

    if (isTRUE(config$trim_pages)) {
      extraction <- extract_relevant_pdf_text(pdf_path, config)
      job_text <- extraction$text
      if (!is.na(job_text)) {
        log_msg(sprintf(
          "  using %d of %d page(s)%s",
          length(extraction$pages_used), extraction$total_pages,
          if (extraction$matched_by_keyword) "" else " (no keyword match - used first-N fallback, worth spot-checking)"
        ))
      }
    } else {
      job_text <- extract_pdf_text(pdf_path)
    }

    if (is.na(job_text) || nchar(str_trim(job_text)) == 0) {
      log_warn(sprintf("No extractable text in %s - skipping (may be a scanned PDF).",
                       basename(pdf_path)))
      file_results[[pdf_path]] <- list(
        status = "failed_no_text", responsibilities = character(0), skills = character(0),
        pages_used = NA_integer_, total_pages = NA_integer_, scraped_job_ref = NA_character_
      )
      next
    }

    # Fallback job-reference scrape (see extract_job_reference_from_text()) -
    # computed once here from the already-extracted text and reused in
    # every file_results branch below, since job_text is available for all
    # of them from this point on. Always computed and persisted to the .txt
    # header (via write_criteria_file()) regardless of add_hr_status_columns,
    # since the cost is negligible (a regex over text already in memory) and
    # doing so means turning add_hr_status_columns on LATER doesn't require
    # reprocessing files that were already extracted with it off.
    scraped_job_ref <- extract_job_reference_from_text(job_text)

    if (isTRUE(config$save_extracted_text)) {
      debug_path <- file.path(config$extracted_text_dir, paste0(base_name, "_EXTRACTED_TEXT.txt"))
      debug_header <- if (isTRUE(config$trim_pages)) {
        sprintf(
          "[Pages used: %s of %d total | matched_by_keyword: %s]\n\n",
          paste(extraction$pages_used, collapse = ", "),
          extraction$total_pages,
          extraction$matched_by_keyword
        )
      } else {
        "[Full PDF text - trim_pages was FALSE]\n\n"
      }
      writeLines(paste0(debug_header, job_text), debug_path, useBytes = TRUE)
      log_msg(sprintf("  -> saved extracted text: %s", debug_path))
    }

    if (!isTRUE(config$call_llm)) {
      log_msg("  call_llm is FALSE - skipping API call (text extraction only).")
      file_results[[pdf_path]] <- list(
        status = "skipped_call_llm_disabled", responsibilities = character(0), skills = character(0),
        pages_used = if (isTRUE(config$trim_pages)) length(extraction$pages_used) else NA_integer_,
        total_pages = if (isTRUE(config$trim_pages)) extraction$total_pages else NA_integer_,
        scraped_job_ref = scraped_job_ref
      )
      next
    }

    prompt <- build_prompt(job_text)
    call_result <- call_claude(prompt, config, api_key)

    if (is.null(call_result)) {
      log_warn(sprintf("Skipping %s: API call failed (see warning above).",
                       basename(pdf_path)))
      file_results[[pdf_path]] <- list(
        status = "failed_api", responsibilities = character(0), skills = character(0),
        pages_used = if (isTRUE(config$trim_pages)) length(extraction$pages_used) else NA_integer_,
        total_pages = if (isTRUE(config$trim_pages)) extraction$total_pages else NA_integer_,
        scraped_job_ref = scraped_job_ref
      )
      next
    }

    if (is.null(call_result$parsed)) {
      # JSON parsing failed (often truncation). Save the raw model output so
      # nothing is lost, then move on to the next file.
      raw_out_path <- file.path(config$output_dir, paste0(base_name, "_RAW_RESPONSE.txt"))
      writeLines(call_result$raw_text, raw_out_path, useBytes = TRUE)
      log_warn(sprintf(
        "Skipping %s: could not parse model response as JSON. Raw response saved to %s for inspection. Try increasing CONFIG$max_tokens and re-running just this file.",
        basename(pdf_path), raw_out_path
      ))
      file_results[[pdf_path]] <- list(
        status = "failed_parse_json", responsibilities = character(0), skills = character(0),
        pages_used = if (isTRUE(config$trim_pages)) length(extraction$pages_used) else NA_integer_,
        total_pages = if (isTRUE(config$trim_pages)) extraction$total_pages else NA_integer_,
        scraped_job_ref = scraped_job_ref
      )
      next
    }

    result <- call_result$parsed
    responsibilities <- result$responsibilities %||% character(0)
    skills           <- result$skills %||% character(0)

    pages_used_this  <- if (isTRUE(config$trim_pages)) length(extraction$pages_used) else NA_integer_
    total_pages_this <- if (isTRUE(config$trim_pages)) extraction$total_pages else NA_integer_

    write_criteria_file(out_path, responsibilities, skills, pdf_path, model = config$model,
                        scraped_job_ref = scraped_job_ref, pages_used = pages_used_this,
                        total_pages = total_pages_this)
    log_msg(sprintf("  -> saved: %s", out_path))

    file_results[[pdf_path]] <- list(
      status = "processed", responsibilities = responsibilities, skills = skills,
      pages_used = pages_used_this, total_pages = total_pages_this,
      scraped_job_ref = scraped_job_ref
    )

    Sys.sleep(config$sleep_between_calls)
  }

  log_msg(sprintf(
    "Finished this run's extraction pass: %d file(s) attempted (sent to the API), %d file(s) touched in total (including free skip_existing_output skips) out of %d in scope.",
    n_attempted_this_run, length(files_touched_this_run), length(pdf_files)
  ))

  # ----------------------------------------------------------------------------
  # Merge postings within each vacancy group and update the status table.
  # Delegated to run_merge_pass() - a standalone function that re-derives
  # group membership and content ENTIRELY from what is currently on disk,
  # rather than solely from this run's in-memory file_results. This is what
  # makes it safe to re-run after a crash (nothing from a completed .txt
  # file is ever missed just because the run that produced it didn't reach
  # this point), and safe/idempotent across separate batch invocations - see
  # run_merge_pass()'s own comment for the full reasoning.
  #
  # scope_pdf_files = files_touched_this_run (not the full pdf_files): for a
  # normal, uncapped full run this still covers every group (every file was
  # touched, same result as an unscoped call), but for a target_files_only,
  # retry_mode, or enable_pdf_limit-capped run, it means only the groups
  # actually touched this run get recomputed/rewritten. In particular, a
  # group with members beyond a PDF-count cap is left completely alone this
  # run - not marked partial for simply not having been reached yet.
  # ----------------------------------------------------------------------------
  if (isTRUE(config$call_llm)) {
    run_merge_pass(config, scope_pdf_files = files_touched_this_run)
  } else {
    log_msg("call_llm is FALSE - skipping the merge/status-table pass (nothing meaningful was extracted this run).")
  }

  log_msg("Done.")
}

if (sys.nframe() == 0) {
  main(CONFIG)
}
