#!/usr/bin/env Rscript
# ==============================================================================
# diagnose_failed_vacancies.R
#
# Author:  Noam Tal-Perry, University of Cambridge
# Project: CaSDaR - "Evidencing the impact of data stewardship roles using
#          administrative and scientometrics data" (UKRI-funded, Network+
#          grant UKRI739)
# License: MIT (see LICENSE)
# Note:    drafted with the assistance of Claude Sonnet 5.5 (Anthropic),
#          Oct-Nov 2026; reviewed and verified by the author.
#
# Data scope (what this repository does and does not include):
#   This repository contains CODE ONLY. It does not include job-ad PDFs, run
#   logs or any extraction output. To run this script yourself, first run
#   extract_job_criteria.R against your own corpus.
#
# Purpose:
#   Work out WHY vacancy groups in vacancy_status.csv are 'failed' (or
#   'partial'), one cause per posting, by combining four independent sources of
#   evidence:
#     1. every run log (run_log_*.txt) - what the extraction script said happened
#     2. what is on disk now (TXTs/, TXTs_extracted/) - the durable record
#     3. the PDFs themselves (optional probe) - covers vacancies with no log entry
#     4. a garbled-text detector - catches PDFs whose text layer is encoded with
#        a broken font mapping (text comes out as e.g. "7KHUROHZLOO" = "The role
#        will"), which neither the logs nor status.csv flag on their own
#
# Why this exists:
#   A 'failed' status alone does not say why. A group can be 'failed' because
#   the category filter deliberately skipped its postings, because a PDF is a
#   scan with no text layer, because its fonts have no Unicode mapping, or
#   because an API call or JSON parse failed. Indirect evidence such as job_ref
#   and page counts does not prove a PDF was readable - job_ref comes from the
#   HR mapping, and run_merge_pass() leaves pages_used as NA for any posting
#   with no .txt file at all. This script separates the real causes before any
#   decision about OCR, a higher max_tokens, or re-running.
#
# Relationship to other scripts:
#   Pure CONSUMER of extract_job_criteria.R output. Never calls the API, never
#   writes to TXTs/ or TXTs_merged/, and touches vacancy_status.csv only when
#   CONFIG_DIAG$write_back_status = TRUE (additive columns, backup first). Otherwise only writes new files to
#   the diagnostics folder. Standalone (sources nothing), so it follows the
#   project convention of a distinct entry-point name (main_diagnose_failed_
#   vacancies) and a dfv_ prefix on every helper, so it can safely be sourced
#   alongside other scripts without name collisions. Config object is
#   CONFIG_DIAG, not CONFIG, for the same reason.
#
# Design principle: recall over precision. This script FILTERS NOTHING and
# EXCLUDES NOTHING. Every failed/partial posting gets a row and a cause, with
# "unknown" kept as an honest cause rather than guessed.
#
# Usage (same convention as the other scripts - run with the scripts folder as
# the working directory, so "../Output" etc. resolve):
#   source("diagnose_failed_vacancies.R"); main_diagnose_failed_vacancies()
# or: Rscript diagnose_failed_vacancies.R
#
# Requirements: base R only. Optional: pdftools (for the PDF probe; the script
# skips the probe with a message if it is not installed).
# ==============================================================================

CONFIG_DIAG <- list(
  status_table_path  = file.path("..", "Output", "vacancy_status.csv"),
  
  # Logs are found by pattern, recursively under log_dir. extract_job_criteria.R
  # writes its run logs to Output/logs/ by default; searching from Output/
  # also picks up logs from older runs that were written directly there.
  log_dir            = file.path("..", "Output"),
  log_pattern        = "^run_log_.*\\.txt$",
  log_recursive      = TRUE,
  
  pdf_dir            = file.path("..", "PDFs"),
  output_dir         = file.path("..", "TXTs"),
  extracted_text_dir = file.path("..", "TXTs_extracted"),
  diagnostics_dir    = file.path("..", "Output", "diagnostics"),
  
  # Category-filter decision log written by extract_job_criteria.R. Used to tell
  # 'skipped by the filter' apart from 'genuinely failed'. Optional: if the file
  # is absent, nothing is relabelled.
  category_filter_log_path = file.path("..", "Output", "category_filter_decisions.csv"),
  
  # Write the diagnosis back into vacancy_status.csv as three ADDITIVE columns
  # (failure_cause, failure_cause_detail, failure_diagnosed_at). The existing
  # 'status' column is never changed. Off by default; a timestamped backup of
  # the status table is saved to diagnostics_dir first (status_backup = FALSE
  # to skip that).
  write_back_status  = FALSE,
  status_backup      = TRUE,
  
  # Which vacancy-group statuses to diagnose. Matches retry_target_statuses in
  # extract_job_criteria.R.
  target_statuses    = c("failed", "partial"),
  
  # PDF probe (opens PDFs with pdftools; needed for postings with no log entry):
  #   "unlogged_or_notext" - postings with no substantive log outcome, or whose
  #                          log says no-text / read error (cross-check). Default.
  #   "all"                - every posting lacking a genuine extraction (slowest)
  #   "none"               - skip the probe entirely
  probe_scope        = "unlogged_or_notext",
  
  # The probe is the slow step, so its results are checkpointed to a cache file
  # (diagnostics_dir/pdf_probe_cache.csv) every probe_checkpoint_every PDFs and
  # reused on the next run - aborting (Esc / Stop) never loses finished probes,
  # and a rerun only probes what is not cached yet. Delete the cache file to
  # force a fresh probe. Progress lines name the PDF being opened, so a hang can
  # be pinned on one file; add its filename to probe_skip_pdfs to bypass it.
  probe_checkpoint_every = 10,
  probe_skip_pdfs        = character(0),
  
  # Also scan EVERY extracted-text file and every successful output .txt for
  # garbled text, not just failed ones. This matters because a garbled PDF can
  # come out 'successful' with garbage bullets (the model copies them verbatim),
  # which would then score as noise. Writes garbled_text_scan_*.csv.
  scan_all_outputs   = TRUE,
  
  # Garbled-text detector thresholds (see dfv_garble_verdict()). Rates are
  # occurrences of common English trigrams per 1,000 letters.
  garble_min_letters   = 300,
  garble_readable_rate = 15,
  garble_ratio         = 3,
  garble_sample_chars  = 6000,
  garble_shifts        = setdiff(-60:60, 0),
  
  # Scanned-PDF-with-text-stub detector: a posting whose saved extracted text has
  # fewer than this many NON-WHITESPACE characters (header/footer/page-number
  # text over a scanned body) is classed 'scanned_text_stub' (needs OCR) rather
  # than 'empty_extraction'. Keep in line with min_text_chars_per_pdf in
  # extract_job_criteria.R. The extracted_chars column in the member CSV shows
  # the raw counts, so you can see where genuine scans end and short real ads begin.
  stub_max_chars       = 300
)

# ------------------------------------------------------------------------------
# Small helpers
# ------------------------------------------------------------------------------
dfv_log <- function(text) message(text)

dfv_read_lines_safe <- function(path) {
  x <- readLines(path, warn = FALSE)
  x <- iconv(x, from = "UTF-8", to = "UTF-8", sub = "?")
  x[is.na(x)] <- ""
  sub("\r$", "", x)
}

dfv_base_name <- function(pdf_name) sub("\\.pdf$", "", pdf_name, ignore.case = TRUE)

# ------------------------------------------------------------------------------
# Garbled-text detector
#
# Some PDFs embed fonts with no proper Unicode mapping, so pdftools returns text
# that is a fixed character-code shift away from real text. Example seen in
# practice: every letter shifted by 29 in ASCII, so "7KH" = "The".
# The detector does not hard-code that shift: it measures how English-like the
# text is (rate of common trigrams), and if it is not, tries every candidate
# shift and checks whether one of them makes it English.
#
# Verdicts:
#   readable            - looks like English as it stands
#   garbled_shift       - not English as it stands, but English after a constant
#                         ASCII shift (the shift is reported)
#   low_english_signal  - not English, and no shift fixes it. Could be another
#                         encoding, a non-English ad, or mostly tables/numbers.
#                         Lower confidence - worth a human look, not a verdict.
#   too_short           - not enough letters to judge
#
# Limitation: a document that is only PARTLY garbled (e.g. some pages) can still
# read as 'readable' overall. This is a screen, not a proof.
# ------------------------------------------------------------------------------
dfv_letters_only <- function(x) tolower(gsub("[^A-Za-z]", "", x))

dfv_count_fixed <- function(s, pat) {
  m <- gregexpr(pat, s, fixed = TRUE)[[1]]
  if (m[1] == -1L) 0L else length(m)
}

dfv_english_rate <- function(letters_lower) {
  n <- nchar(letters_lower)
  if (n == 0) return(0)
  pats <- c("the", "and", "ing", "ion", "ent", "for", "tio")
  hits <- sum(vapply(pats, function(p) dfv_count_fixed(letters_lower, p), integer(1)))
  1000 * hits / n
}

dfv_shift_ascii <- function(x, shift) {
  cp <- utf8ToInt(x)
  if (length(cp) == 0 || anyNA(cp)) return(x)
  in_range <- cp >= 33L & cp <= 126L
  moved <- cp + as.integer(shift)
  ok <- in_range & moved >= 33L & moved <= 126L
  cp[ok] <- moved[ok]
  intToUtf8(cp)
}

dfv_garble_verdict <- function(text, cfg) {
  text <- substr(paste(text, collapse = "\n"), 1, cfg$garble_sample_chars)
  n_letters <- nchar(dfv_letters_only(text))
  blank <- list(verdict = "too_short", letters = n_letters, raw_rate = NA_real_,
                best_shift = NA_integer_, best_rate = NA_real_)
  if (n_letters < cfg$garble_min_letters) return(blank)
  
  raw_rate <- dfv_english_rate(dfv_letters_only(text))
  if (raw_rate >= cfg$garble_readable_rate) {
    return(list(verdict = "readable", letters = n_letters, raw_rate = raw_rate,
                best_shift = NA_integer_, best_rate = NA_real_))
  }
  
  # Letters are compared case-insensitively, so shifts that differ by 32 give the
  # same letter sequence (the known real case, +29, is reported as -3, its
  # equivalent). A candidate shift must also KEEP most of the letters: a shift
  # that turns most letters into punctuation can leave a few intact words and
  # look "English" by accident (seen on a mixed garbled/readable raw response).
  best_shift <- NA_integer_
  best_rate  <- -1
  for (s in cfg$garble_shifts) {
    shifted_letters <- dfv_letters_only(dfv_shift_ascii(text, s))
    if (nchar(shifted_letters) < 0.6 * n_letters) next
    r <- dfv_english_rate(shifted_letters)
    if (r > best_rate) { best_rate <- r; best_shift <- as.integer(s) }
  }
  fixed_by_shift <- best_rate >= cfg$garble_readable_rate &&
    best_rate >= cfg$garble_ratio * max(raw_rate, 1)
  list(verdict = if (fixed_by_shift) "garbled_shift" else "low_english_signal",
       letters = n_letters, raw_rate = raw_rate,
       best_shift = if (fixed_by_shift) best_shift else NA_integer_,
       best_rate = best_rate)
}

# Verdict for one saved text file, ignoring the header lines the extraction
# script writes ("[Pages used ...", "Source:", "Model:", ...) so they cannot
# dilute or inflate the English rate.
dfv_garble_verdict_file <- function(path, cfg) {
  if (!file.exists(path)) {
    return(list(verdict = NA_character_, letters = NA_integer_, raw_rate = NA_real_,
                best_shift = NA_integer_, best_rate = NA_real_))
  }
  lines <- dfv_read_lines_safe(path)
  lines <- lines[!grepl("^\\[Pages used|^\\[Full PDF text|^Source: |^Model: |^Pages Used: |^Scraped Job Reference: ", lines)]
  dfv_garble_verdict(lines, cfg)
}

# ------------------------------------------------------------------------------
# Run-log parser
#
# Each PDF attempt in a log starts at "Processing file N of M: <pdf>" and runs to
# the next such line. Outcome strings below are the exact log messages produced
# by extract_job_criteria.R (checked against the script, not guessed):
#   "  -> saved: <txt>"                                   success
#   "WARNING: Failed to read <path>: <msg>"               pdftools could not open it
#   "WARNING: No extractable text in <pdf> - skipping ..." empty text layer
#   "WARNING: Skipping <pdf>: API call failed ..."        API error after retries
#   "WARNING: Skipping <pdf>: could not parse model response as JSON ..."
#   "  -> output already exists ... - skipping"           not re-run
#   "  call_llm is FALSE - skipping API call ..."         text-only run
# plus a separate "Response was truncated because max_tokens was reached" flag.
#
# An attempt with none of these means the log ended (or the run was interrupted)
# before an outcome was written - recorded as "no_outcome_logged".
# ------------------------------------------------------------------------------
dfv_block_outcome <- function(blk) {
  has <- function(p) any(grepl(p, blk))
  parse_line <- grep("Could not parse JSON from model response:", blk, value = TRUE)
  parse_kind <- if (length(parse_line) == 0) NA_character_
  else if (grepl("premature EOF", parse_line[1])) "premature_eof"
  else if (grepl("lexical error", parse_line[1])) "lexical_error"
  else "other"
  api_line <- grep("^WARNING: API (call failed|returned an error)", blk, value = TRUE)
  
  outcome <- if (has("^\\s*-> saved: ")) "success"
  else if (has("^WARNING: Failed to read ")) "read_error"
  else if (has("^WARNING: No extractable text in ")) "no_text"
  else if (has("^WARNING: Skipping .*API call failed")) "api_failed"
  else if (has("^WARNING: Skipping .*could not parse model response as JSON")) "parse_failed"
  else if (has("output already exists at .* - skipping")) "skipped_existing"
  else if (has("call_llm is FALSE")) "text_only"
  else "no_outcome_logged"
  
  list(outcome = outcome,
       truncated = has("Response was truncated because max_tokens"),
       parse_kind = parse_kind,
       api_detail = if (length(api_line) > 0) substr(api_line[1], 1, 200) else NA_character_)
}

dfv_parse_logs <- function(cfg) {
  files <- list.files(cfg$log_dir, pattern = cfg$log_pattern,
                      recursive = isTRUE(cfg$log_recursive), full.names = TRUE)
  files <- files[order(basename(files))]   # run_log_YYYYMMDD_HHMMSS sorts chronologically
  if (length(files) == 0) {
    dfv_log(sprintf("No run logs matching '%s' found under %s.", cfg$log_pattern, cfg$log_dir))
    return(data.frame(log_file = character(0), pdf = character(0), outcome = character(0),
                      truncated = logical(0), parse_kind = character(0),
                      api_detail = character(0), seq = integer(0), stringsAsFactors = FALSE))
  }
  rows <- list()
  for (f in files) {
    lines  <- dfv_read_lines_safe(f)
    starts <- grep("^Processing file [0-9]+ of [0-9]+: ", lines)
    if (length(starts) == 0) next
    ends <- c(starts[-1] - 1L, length(lines))
    for (k in seq_along(starts)) {
      blk <- lines[starts[k]:ends[k]]
      pdf <- trimws(sub("^Processing file [0-9]+ of [0-9]+: ", "", blk[1]))
      o <- dfv_block_outcome(blk)
      rows[[length(rows) + 1]] <- data.frame(
        log_file = basename(f), pdf = pdf, outcome = o$outcome, truncated = o$truncated,
        parse_kind = o$parse_kind, api_detail = o$api_detail, stringsAsFactors = FALSE)
    }
  }
  if (length(rows) == 0) {
    dfv_log("Log files found, but none contained a 'Processing file N of M:' line.")
    return(data.frame(log_file = character(0), pdf = character(0), outcome = character(0),
                      truncated = logical(0), parse_kind = character(0),
                      api_detail = character(0), seq = integer(0), stringsAsFactors = FALSE))
  }
  attempts <- do.call(rbind, rows)
  attempts$seq <- seq_len(nrow(attempts))
  dfv_log(sprintf("Parsed %d log file(s): %d PDF attempt(s) across %d distinct PDF(s).",
                  length(files), nrow(attempts), length(unique(attempts$pdf))))
  attempts
}

# Latest SUBSTANTIVE attempt per PDF. A free skip (output already existed) or a
# text-only run does not override an earlier real failure.
dfv_latest_attempts <- function(attempts) {
  if (nrow(attempts) == 0) {
    return(data.frame(pdf = character(0), n_attempts = integer(0), history = character(0),
                      latest_outcome = character(0), latest_log = character(0),
                      truncated = logical(0), parse_kind = character(0),
                      api_detail = character(0), stringsAsFactors = FALSE))
  }
  by_pdf <- split(attempts, attempts$pdf)
  do.call(rbind, lapply(names(by_pdf), function(p) {
    a <- by_pdf[[p]]
    a <- a[order(a$seq), ]
    subst <- a[!(a$outcome %in% c("skipped_existing", "text_only")), , drop = FALSE]
    last <- if (nrow(subst) > 0) subst[nrow(subst), ] else NULL
    data.frame(
      pdf = p, n_attempts = nrow(a), history = paste(a$outcome, collapse = " > "),
      latest_outcome = if (is.null(last)) NA_character_ else last$outcome,
      latest_log     = if (is.null(last)) NA_character_ else last$log_file,
      truncated      = if (is.null(last)) NA else last$truncated,
      parse_kind     = if (is.null(last)) NA_character_ else last$parse_kind,
      api_detail     = if (is.null(last)) NA_character_ else last$api_detail,
      stringsAsFactors = FALSE)
  }))
}

# ------------------------------------------------------------------------------
# PDF probe
# ------------------------------------------------------------------------------
dfv_probe_pdf <- function(path, cfg) {
  empty <- list(probe = NA_character_, detail = NA_character_,
                n_pages = NA_integer_, pages_with_text = NA_integer_)
  if (!file.exists(path)) { empty$probe <- "pdf_missing_from_folder"; return(empty) }
  info <- tryCatch(pdftools::pdf_info(path), error = function(e) e)
  if (inherits(info, "error")) {
    empty$probe <- "unreadable_pdf"; empty$detail <- substr(conditionMessage(info), 1, 200)
    return(empty)
  }
  txt <- tryCatch(pdftools::pdf_text(path), error = function(e) e)
  if (inherits(txt, "error")) {
    empty$probe <- "unreadable_pdf"; empty$detail <- substr(conditionMessage(txt), 1, 200)
    empty$n_pages <- as.integer(info$pages)
    return(empty)
  }
  nch <- nchar(gsub("\\s+", "", txt))
  out <- list(probe = NA_character_, detail = NA_character_,
              n_pages = as.integer(info$pages), pages_with_text = as.integer(sum(nch > 0)))
  if (out$pages_with_text == 0) {
    out$probe <- "no_text_layer"
  } else {
    g <- dfv_garble_verdict(paste(txt[seq_len(min(5, length(txt)))], collapse = "\n"), cfg)
    out$probe <- if (identical(g$verdict, "garbled_shift")) "text_garbled" else "text_ok"
    if (!is.na(g$best_shift)) out$detail <- sprintf("shift %+d", g$best_shift)
  }
  out
}

# Probe cache: one row per probed PDF. Written with na = "" and read back with
# na.strings = "" so a missing detail/page count round-trips as NA.
dfv_probe_cache_read <- function(path) {
  empty <- data.frame(pdf = character(0), probe = character(0), detail = character(0),
                      n_pages = integer(0), pages_with_text = integer(0), stringsAsFactors = FALSE)
  if (!file.exists(path)) return(empty)
  x <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character", na.strings = ""),
                error = function(e) NULL)
  if (is.null(x) || !all(c("pdf", "probe", "detail", "n_pages", "pages_with_text") %in% names(x))) return(empty)
  x$n_pages <- as.integer(x$n_pages); x$pages_with_text <- as.integer(x$pages_with_text)
  x[!duplicated(x$pdf), c("pdf", "probe", "detail", "n_pages", "pages_with_text")]
}

dfv_probe_cache_write <- function(cache, path) {
  utils::write.csv(cache, path, row.names = FALSE, na = "")
}

# ------------------------------------------------------------------------------
# Cause classification for ONE posting that has no genuine extraction
# Priority (root cause first): unreadable file > no text layer > garbled text >
# truncated > other JSON failure > API error > empty extraction > unknown.
# ------------------------------------------------------------------------------
dfv_classify_member <- function(m) {
  lo <- m$latest_outcome
  failed_log <- !is.na(lo)
  garbled_in <- identical(m$extracted_verdict, "garbled_shift") ||
    identical(m$probe, "text_garbled")
  
  if (failed_log && lo == "read_error")  return(c("unreadable_pdf", "log"))
  if (failed_log && lo == "no_text")     return(c("no_text_layer", "log"))
  if (identical(m$probe, "unreadable_pdf")) return(c("unreadable_pdf", "pdf_probe"))
  if (garbled_in && (identical(lo, "parse_failed") || isTRUE(m$txt_exists) ||
                     identical(m$probe, "text_garbled"))) {
    return(c("garbled_text_layer", if (failed_log || isTRUE(m$txt_exists)) "log+disk" else "pdf_probe"))
  }
  if (isTRUE(m$text_stub))               return(c("scanned_text_stub", "extracted_text"))
  if (failed_log && lo == "parse_failed") {
    return(c(if (isTRUE(m$truncated)) "truncated_max_tokens" else "json_parse_other", "log"))
  }
  if (failed_log && lo == "api_failed")  return(c("api_error", "log"))
  if (isTRUE(m$txt_exists))              return(c("empty_extraction", "disk"))
  if (failed_log && lo == "no_outcome_logged") return(c("interrupted_mid_file", "log"))
  if (identical(m$probe, "no_text_layer")) return(c("no_text_layer", "pdf_probe"))
  if (identical(m$probe, "text_ok"))     return(c("no_log_text_ok_unknown", "pdf_probe"))
  if (identical(m$probe, "pdf_missing_from_folder")) return(c("pdf_missing_from_folder", "pdf_probe"))
  c("unknown_no_evidence", "none")
}

dfv_recommended_action <- function(cause) {
  switch(cause,
         truncated_max_tokens   = "Raise CONFIG$max_tokens in extract_job_criteria.R, then re-run with retry_mode = TRUE.",
         json_parse_other       = "Open the _RAW_RESPONSE.txt; usually garbled input - see garbled_text_layer. Otherwise re-run.",
         api_error              = "Re-run as-is with retry_mode = TRUE (check api_detail for the error).",
         no_text_layer          = "Needs OCR (scanned/image-only PDF). Not fixable by re-running.",
         scanned_text_stub      = "Scanned PDF whose only text is a header/footer stub (see extracted_chars). Needs OCR; the empty output is not a real extraction.",
         garbled_text_layer     = "Needs OCR or a font-aware text extractor. Re-running alone will not fix it.",
         unreadable_pdf         = "File will not open. Check integrity / obtain a replacement copy.",
         empty_extraction       = "Open the _EXTRACTED_TEXT.txt: wrong pages trimmed, or the ad has no recognisable sections.",
         interrupted_mid_file   = "Run stopped mid-file. Re-run with retry_mode = TRUE.",
         no_log_text_ok_unknown = "Text layer is fine but no failure logged and no filter decision found: never attempted, or its log was lost. Re-run with target_files_only.",
         excluded_by_category_filter = "Not a failure: skipped by the category filter. Only processed if the category_filter_include_no_* flags are turned on.",
         pdf_missing_from_folder = "PDF is listed in vacancy_status.csv but not in the PDFs folder.",
         "No evidence found: inspect manually.")
}

# ------------------------------------------------------------------------------
# Cause ranking used to pick ONE primary cause per vacancy group (root cause
# first). 'excluded_by_category_filter' is deliberately low: a group that also
# has a genuine attempted failure should show THAT failure, not the exclusion.
# ------------------------------------------------------------------------------
dfv_cause_priority <- c("unreadable_pdf", "no_text_layer", "scanned_text_stub", "garbled_text_layer",
                        "truncated_max_tokens", "json_parse_other", "api_error",
                        "empty_extraction", "interrupted_mid_file",
                        "no_log_text_ok_unknown", "excluded_by_category_filter",
                        "pdf_missing_from_folder", "unknown_no_evidence")

# ------------------------------------------------------------------------------
# Relabel postings that were never attempted because the category filter
# excluded them. Only 'no_log_text_ok_unknown' (text layer fine, no evidence of
# an attempt) is relabelled - anything with real failure evidence keeps its
# cause. 'decisions' = Output/category_filter_decisions.csv (pdf_file, decision).
# Adds/overwrites members$filter_decision.
# ------------------------------------------------------------------------------
dfv_relabel_filtered <- function(members, decisions) {
  members$filter_decision <- if (is.null(decisions)) NA_character_ else
    decisions$decision[match(members$pdf, decisions$pdf_file)]
  hit <- members$cause %in% "no_log_text_ok_unknown" &
    !is.na(members$filter_decision) & startsWith(members$filter_decision, "exclude")
  members$cause[hit] <- "excluded_by_category_filter"
  if ("cause_source" %in% names(members)) members$cause_source[hit] <- "category_filter_log"
  if ("recommended_action" %in% names(members)) {
    members$recommended_action[hit] <- paste("Not a failure: skipped by the category filter",
                                             "(see filter_decision). Only processed if category_filter_include_no_* flags are turned on.")
  }
  members
}

# ------------------------------------------------------------------------------
# One row per vacancy group: primary cause (by dfv_cause_priority) + detail.
# ------------------------------------------------------------------------------
dfv_vacancy_rollup <- function(members) {
  out <- do.call(rbind, lapply(split(members, members$vacancy_number), function(g) {
    bad <- g[g$cause != "has_extraction_now", , drop = FALSE]
    causes <- unique(bad$cause)
    primary <- if (length(causes) == 0) NA_character_ else
      causes[order(match(causes, dfv_cause_priority))][1]
    data.frame(vacancy_number = g$vacancy_number[1], group_status = g$group_status[1],
               n_postings = nrow(g), n_postings_needing_attention = nrow(bad),
               primary_cause = primary, all_causes = paste(sort(causes), collapse = "; "),
               position_category = g$position_category[1], job_ref = g$job_ref[1],
               stringsAsFactors = FALSE)
  }))
  rownames(out) <- NULL
  out
}

# ------------------------------------------------------------------------------
# Write the diagnosis back into vacancy_status.csv as three ADDITIVE columns:
#   failure_cause, failure_cause_detail, failure_diagnosed_at
# The existing 'status' column and every other column are left exactly as they
# were. Only groups in 'vac' (the diagnosed failed/partial groups) get values;
# rows whose status is NOT failed/partial have the three columns blanked (any
# older diagnosis there is stale). A timestamped backup is written first.
# extract_job_criteria.R's upsert_status_table() keeps unknown columns for
# untouched rows and blanks them for groups it re-processes, so a diagnosis
# disappears automatically when its group is re-run.
# ------------------------------------------------------------------------------
dfv_write_back_status <- function(vac, config, stamp = format(Sys.time(), "%Y%m%d_%H%M%S")) {
  path <- config$status_table_path
  st <- utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character",
                        na.strings = character(0))
  newcols <- c("failure_cause", "failure_cause_detail", "failure_diagnosed_at")
  for (cc in newcols) if (!(cc %in% names(st))) st[[cc]] <- ""
  
  if (!isFALSE(config$status_backup)) {
    dir.create(config$diagnostics_dir, recursive = TRUE, showWarnings = FALSE)
    bak <- file.path(config$diagnostics_dir, sprintf("vacancy_status_BACKUP_%s.csv", stamp))
    file.copy(path, bak, overwrite = FALSE)
  }
  
  stale <- !(st$status %in% config$target_statuses)
  for (cc in newcols) st[[cc]][stale] <- ""
  
  cause <- ifelse(is.na(vac$primary_cause), "stale_status_extraction_present", vac$primary_cause)
  detail <- ifelse(is.na(vac$primary_cause),
                   "All postings now have an extraction; status row is out of date - re-run the merge pass.",
                   sprintf("%d of %d posting(s) need attention: %s",
                           vac$n_postings_needing_attention, vac$n_postings, vac$all_causes))
  hit <- match(vac$vacancy_number, st$vacancy_number)
  ok <- !is.na(hit)
  st$failure_cause[hit[ok]]        <- cause[ok]
  st$failure_cause_detail[hit[ok]] <- detail[ok]
  st$failure_diagnosed_at[hit[ok]] <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  tmp <- paste0(path, ".tmp")
  utils::write.csv(st, tmp, row.names = FALSE)
  file.copy(tmp, path, overwrite = TRUE); unlink(tmp)
  message(sprintf("Wrote failure_cause columns to %s: %d group(s) diagnosed, %d not matched, %d stale row(s) blanked. Backup: %s",
                  path, sum(ok), sum(!ok), sum(stale),
                  if (isFALSE(config$status_backup)) "none" else bak))
  invisible(st)
}

# ------------------------------------------------------------------------------
# Member table: one row per posting in a failed/partial group
# ------------------------------------------------------------------------------
dfv_build_members <- function(status_df, cfg) {
  targets <- status_df[status_df$status %in% cfg$target_statuses, , drop = FALSE]
  if (nrow(targets) == 0) return(NULL)
  rows <- lapply(seq_len(nrow(targets)), function(i) {
    r <- targets[i, ]
    files <- trimws(strsplit(r$source_files, ";\\s*")[[1]])
    files <- files[nzchar(files)]
    pages <- trimws(strsplit(r$pages_used_summary, ";\\s*")[[1]])
    if (length(pages) != length(files)) pages <- rep(NA_character_, length(files))
    data.frame(
      vacancy_number = r$vacancy_number, group_status = r$status,
      n_postings = r$n_postings, position_category = r$position_category,
      job_ref = r$job_ref, pdf = files, pages_used = pages,
      stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# Non-whitespace characters in a saved _EXTRACTED_TEXT.txt, excluding its first
# line (the "[Pages used: ...]" header written by extract_job_criteria.R).
dfv_extracted_chars <- function(path) {
  if (!file.exists(path)) return(NA_integer_)
  lines <- dfv_read_lines_safe(path)
  if (length(lines) <= 1) return(0L)
  as.integer(nchar(gsub("\\s+", "", paste(lines[-1], collapse = ""))))
}

dfv_txt_state <- function(path) {
  if (!file.exists(path)) return(list(exists = FALSE, n_bullets = NA_integer_))
  lines <- dfv_read_lines_safe(path)
  list(exists = TRUE, n_bullets = sum(grepl("^- ", lines)))
}

# ------------------------------------------------------------------------------
# Scan every extracted-text file and output .txt for garbled text
# ------------------------------------------------------------------------------
dfv_scan_all_outputs <- function(cfg, status_df) {
  ex  <- list.files(cfg$extracted_text_dir, pattern = "_EXTRACTED_TEXT\\.txt$", full.names = TRUE)
  out <- list.files(cfg$output_dir, pattern = "\\.txt$", full.names = TRUE)
  out <- out[!grepl("_RAW_RESPONSE\\.txt$", out)]
  dfv_log(sprintf("Garbled-text scan: %d extracted-text file(s), %d output .txt file(s)...", length(ex), length(out)))
  
  scan_one <- function(p, kind, strip) {
    v <- dfv_garble_verdict_file(p, cfg)
    data.frame(base = sub(strip, "", basename(p)), kind = kind, verdict = v$verdict,
               letters = v$letters, raw_rate = round(v$raw_rate, 1),
               best_shift = v$best_shift, stringsAsFactors = FALSE)
  }
  ex_df  <- if (length(ex))  do.call(rbind, lapply(ex,  scan_one, kind = "extracted", strip = "_EXTRACTED_TEXT\\.txt$")) else NULL
  out_df <- if (length(out)) do.call(rbind, lapply(out, scan_one, kind = "output",    strip = "\\.txt$")) else NULL
  if (is.null(ex_df) && is.null(out_df)) return(NULL)
  
  all_df <- rbind(ex_df, out_df)
  wide_ex  <- ex_df[, c("base", "verdict", "best_shift")]
  names(wide_ex)  <- c("base", "extracted_verdict", "extracted_shift")
  wide_out <- out_df[, c("base", "verdict")]
  names(wide_out) <- c("base", "output_verdict")
  
  merged <- merge(wide_ex, wide_out, by = "base", all = TRUE)
  merged$output_has_content <- NA
  bullets <- vapply(file.path(cfg$output_dir, paste0(merged$base, ".txt")),
                    function(p) dfv_txt_state(p)$n_bullets, integer(1))
  merged$output_has_content <- !is.na(bullets) & bullets > 0
  merged$scan_class <- ifelse(
    !is.na(merged$extracted_verdict) & merged$extracted_verdict == "garbled_shift",
    ifelse(is.na(merged$output_verdict), "garbled_input_no_output",
           ifelse(!merged$output_has_content, "garbled_input_empty_output",
                  ifelse(merged$output_verdict == "garbled_shift", "garbled_input_GARBLED_OUTPUT",
                         "garbled_input_output_readable_(model_decoded)"))),
    ifelse(!is.na(merged$output_verdict) & merged$output_verdict == "garbled_shift",
           "garbled_output_only", "not_garbled"))
  
  # which vacancy group / status does each file belong to?
  map <- do.call(rbind, lapply(seq_len(nrow(status_df)), function(i) {
    files <- trimws(strsplit(status_df$source_files[i], ";\\s*")[[1]])
    data.frame(base = dfv_base_name(files), vacancy_number = status_df$vacancy_number[i],
               group_status = status_df$status[i], stringsAsFactors = FALSE)
  }))
  merged <- merge(merged, map, by = "base", all.x = TRUE)
  merged
}

# ------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------
main_diagnose_failed_vacancies <- function(config = CONFIG_DIAG) {
  if (!file.exists(config$status_table_path)) {
    stop(sprintf("Status table not found: %s (run from the scripts folder, or edit CONFIG_DIAG paths).",
                 config$status_table_path))
  }
  status_df <- utils::read.csv(config$status_table_path, stringsAsFactors = FALSE,
                               colClasses = "character", na.strings = character(0))
  need <- c("vacancy_number", "status", "source_files", "pages_used_summary",
            "position_category", "job_ref", "n_postings")
  miss <- setdiff(need, names(status_df))
  if (length(miss) > 0) stop(sprintf("vacancy_status.csv is missing column(s): %s", paste(miss, collapse = ", ")))
  
  dfv_log(sprintf("Status table: %d vacancy group(s). By status: %s", nrow(status_df),
                  paste(sprintf("%s=%d", names(table(status_df$status)), as.integer(table(status_df$status))), collapse = ", ")))
  
  members <- dfv_build_members(status_df, config)
  if (is.null(members)) {
    dfv_log("No groups with the target statuses - nothing to diagnose.")
    return(invisible(NULL))
  }
  dfv_log(sprintf("Diagnosing %d posting(s) in %d group(s) with status in {%s}.",
                  nrow(members), length(unique(members$vacancy_number)),
                  paste(config$target_statuses, collapse = ", ")))
  
  # ---- 1. logs ---------------------------------------------------------------
  attempts <- dfv_parse_logs(config)
  latest <- dfv_latest_attempts(attempts)
  members <- merge(members, latest, by = "pdf", all.x = TRUE, sort = FALSE)
  
  # ---- 2. disk state ---------------------------------------------------------
  base <- dfv_base_name(members$pdf)
  txt_states <- lapply(file.path(config$output_dir, paste0(base, ".txt")), dfv_txt_state)
  members$txt_exists  <- vapply(txt_states, function(s) s$exists, logical(1))
  members$txt_bullets <- vapply(txt_states, function(s) s$n_bullets, integer(1))
  members$raw_response_exists <- file.exists(file.path(config$output_dir, paste0(base, "_RAW_RESPONSE.txt")))
  ex_paths <- file.path(config$extracted_text_dir, paste0(base, "_EXTRACTED_TEXT.txt"))
  members$extracted_exists <- file.exists(ex_paths)
  
  # Postings that already have a genuine extraction are fine (this is the
  # 'partial' group case) - they get no cause.
  has_genuine <- members$txt_exists & !is.na(members$txt_bullets) & members$txt_bullets > 0
  
  # Garbled check on the saved extracted text, only for postings that need a cause
  members$extracted_verdict <- NA_character_
  members$extracted_shift   <- NA_integer_
  members$extracted_chars <- vapply(ex_paths, dfv_extracted_chars, integer(1), USE.NAMES = FALSE)
  members$text_stub <- !is.na(members$extracted_chars) & members$extracted_chars < config$stub_max_chars
  for (i in which(!has_genuine & members$extracted_exists)) {
    v <- dfv_garble_verdict_file(ex_paths[i], config)
    members$extracted_verdict[i] <- v$verdict
    members$extracted_shift[i]   <- v$best_shift
  }
  
  # ---- 3. PDF probe ----------------------------------------------------------
  members$probe <- NA_character_; members$probe_detail <- NA_character_
  members$pdf_pages <- NA_integer_; members$pdf_pages_with_text <- NA_integer_
  need_probe <- switch(config$probe_scope,
                       all = !has_genuine,
                       unlogged_or_notext = !has_genuine &
                         (is.na(members$latest_outcome) | members$latest_outcome %in% c("no_text", "read_error")),
                       none = rep(FALSE, nrow(members)),
                       stop("CONFIG_DIAG$probe_scope must be 'all', 'unlogged_or_notext' or 'none'."))
  if (any(need_probe)) {
    if (!requireNamespace("pdftools", quietly = TRUE)) {
      dfv_log("pdftools is not installed - skipping the PDF probe. Postings with no log entry will be 'unknown'.")
    } else {
      dir.create(config$diagnostics_dir, recursive = TRUE, showWarnings = FALSE)
      cache_path <- file.path(config$diagnostics_dir, "pdf_probe_cache.csv")
      cache <- dfv_probe_cache_read(cache_path)
      idx <- which(need_probe)
      
      # reuse cached probes; mark manually skipped files
      cached_hit <- members$pdf[idx] %in% cache$pdf
      skipped    <- members$pdf[idx] %in% config$probe_skip_pdfs & !cached_hit
      dfv_log(sprintf("Probe: %d PDF(s) in scope (scope = %s); %d already in cache, %d manually skipped, %d to probe now.",
                      length(idx), config$probe_scope, sum(cached_hit), sum(skipped), sum(!cached_hit & !skipped)))
      
      todo <- idx[!cached_hit & !skipped]
      t0 <- Sys.time()
      interrupted <- FALSE
      tryCatch({
        for (k in seq_along(todo)) {
          i <- todo[k]
          p <- suppressWarnings(dfv_probe_pdf(file.path(config$pdf_dir, members$pdf[i]), config))
          cache <- rbind(cache, data.frame(
            pdf = members$pdf[i], probe = p$probe, detail = p$detail,
            n_pages = p$n_pages, pages_with_text = p$pages_with_text, stringsAsFactors = FALSE))
          if (k %% config$probe_checkpoint_every == 0) dfv_probe_cache_write(cache, cache_path)
          if (k %% 50 == 0) {
            dfv_log(sprintf("  probed %d / %d (%.1f min elapsed); last: %s", k, length(todo),
                            as.numeric(difftime(Sys.time(), t0, units = "mins")), members$pdf[i]))
          }
        }
      }, interrupt = function(e) { interrupted <<- TRUE },
      finally = dfv_probe_cache_write(cache, cache_path))   # also saves if anything unexpected errors
      if (interrupted) {
        stop(sprintf(paste("Interrupted. %d probe result(s) are saved in %s - just run again to resume.",
                           "If it hangs on the same file, add its name to CONFIG_DIAG$probe_skip_pdfs."),
                     nrow(cache), cache_path), call. = FALSE)
      }
      
      # fill members from the (now complete) cache
      for (i in idx) {
        if (members$pdf[i] %in% config$probe_skip_pdfs && !(members$pdf[i] %in% cache$pdf)) {
          members$probe[i] <- "probe_skipped_manual"
          next
        }
        j <- match(members$pdf[i], cache$pdf)
        members$probe[i]  <- cache$probe[j]; members$probe_detail[i] <- cache$detail[j]
        members$pdf_pages[i] <- cache$n_pages[j]; members$pdf_pages_with_text[i] <- cache$pages_with_text[j]
      }
    }
  }
  
  # ---- 4. classify -----------------------------------------------------------
  members$cause <- NA_character_; members$cause_source <- NA_character_
  for (i in seq_len(nrow(members))) {
    if (has_genuine[i]) { members$cause[i] <- "has_extraction_now"; members$cause_source[i] <- "disk"; next }
    cl <- dfv_classify_member(as.list(members[i, ]))
    members$cause[i] <- cl[1]; members$cause_source[i] <- cl[2]
  }
  members$recommended_action <- vapply(members$cause, function(cz) {
    if (cz == "has_extraction_now") "None - this posting already has an extraction." else dfv_recommended_action(cz)
  }, character(1))
  members$notes <- ""
  members$notes[members$latest_outcome %in% "no_text" & members$probe %in% "text_ok"] <-
    "Log says no text but the PDF has a text layer now - re-run may succeed."
  members$notes[members$latest_outcome %in% "parse_failed" & members$raw_response_exists == FALSE] <-
    "Parse failure logged but no _RAW_RESPONSE.txt on disk (deleted or moved?)."
  members$blank_category <- !nzchar(trimws(members$position_category))
  
  # Relabel postings the category filter excluded (not failures).
  decisions <- if (file.exists(config$category_filter_log_path)) {
    utils::read.csv(config$category_filter_log_path, stringsAsFactors = FALSE,
                    colClasses = "character", na.strings = character(0))
  } else { dfv_log("Category filter log not found - no postings will be relabelled as filter-excluded."); NULL }
  members <- dfv_relabel_filtered(members, decisions)
  
  # ---- 5. vacancy-level roll-up ---------------------------------------------
  vac <- dfv_vacancy_rollup(members)
  
  # ---- 6. garbled scan over ALL outputs --------------------------------------
  scan <- if (isTRUE(config$scan_all_outputs)) dfv_scan_all_outputs(config, status_df) else NULL
  
  # ---- 7. write + summarise --------------------------------------------------
  dir.create(config$diagnostics_dir, recursive = TRUE, showWarnings = FALSE)
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  front <- c("vacancy_number", "group_status", "pdf", "cause", "cause_source", "recommended_action",
             "filter_decision", "latest_outcome", "truncated", "parse_kind", "api_detail", "history", "n_attempts", "latest_log",
             "txt_exists", "txt_bullets", "raw_response_exists", "extracted_exists",
             "extracted_chars", "extracted_verdict", "extracted_shift", "probe", "probe_detail", "pdf_pages", "pdf_pages_with_text",
             "pages_used", "position_category", "blank_category", "job_ref", "n_postings", "notes")
  members <- members[order(members$cause, members$vacancy_number), front]
  
  p_members <- file.path(config$diagnostics_dir, sprintf("failed_member_diagnosis_%s.csv", stamp))
  p_vac     <- file.path(config$diagnostics_dir, sprintf("failed_vacancy_diagnosis_%s.csv", stamp))
  utils::write.csv(members, p_members, row.names = FALSE)
  utils::write.csv(vac, p_vac, row.names = FALSE)
  
  # summary: groups by primary cause, split by status and blank position_category
  vac_failed <- vac[!is.na(vac$primary_cause), ]
  vac_failed$blank_category <- !nzchar(trimws(vac_failed$position_category))
  summ <- as.data.frame(table(group_status = vac_failed$group_status, primary_cause = vac_failed$primary_cause,
                              blank_position_category = vac_failed$blank_category), stringsAsFactors = FALSE)
  summ <- summ[summ$Freq > 0, ]
  p_summ <- file.path(config$diagnostics_dir, sprintf("failed_cause_summary_%s.csv", stamp))
  utils::write.csv(summ, p_summ, row.names = FALSE)
  
  dfv_log("")
  dfv_log("=== Primary cause, vacancy groups (failed + partial) ===")
  tab <- sort(table(vac_failed$primary_cause), decreasing = TRUE)
  for (nm in names(tab)) dfv_log(sprintf("  %-28s %5d  (%4.1f%%)", nm, tab[[nm]], 100 * tab[[nm]] / sum(tab)))
  dfv_log(sprintf("  %-28s %5d", "TOTAL", sum(tab)))
  
  if (any(vac_failed$group_status == "failed")) {
    f <- vac_failed[vac_failed$group_status == "failed", ]
    dfv_log("")
    dfv_log(sprintf("=== 'failed' groups only (n = %d): blank vs populated position_category, by cause ===", nrow(f)))
    ct <- table(cause = f$primary_cause, blank_category = f$blank_category)
    print(ct)
  }
  
  dfv_log("")
  dfv_log("=== Evidence quality (postings needing a cause) ===")
  need <- members[members$cause != "has_extraction_now", ]
  src <- table(need$cause_source)
  for (nm in names(src)) dfv_log(sprintf("  cause from %-10s %5d", nm, src[[nm]]))
  n_nolog <- sum(is.na(need$latest_outcome))
  dfv_log(sprintf("  postings with no substantive outcome in any log (includes text-only runs): %d (of %d)", n_nolog, nrow(need)))
  
  if (!is.null(scan)) {
    p_scan <- file.path(config$diagnostics_dir, sprintf("garbled_text_scan_%s.csv", stamp))
    utils::write.csv(scan, p_scan, row.names = FALSE)
    dfv_log("")
    dfv_log("=== Garbled-text scan across ALL extracted/output files ===")
    st <- sort(table(scan$scan_class), decreasing = TRUE)
    for (nm in names(st)) dfv_log(sprintf("  %-46s %5d", nm, st[[nm]]))
    risky <- scan[scan$scan_class %in% c("garbled_input_GARBLED_OUTPUT", "garbled_output_only"), ]
    if (nrow(risky) > 0) {
      dfv_log(sprintf("  WARNING: %d file(s) have GARBLED OUTPUT presented as a normal extraction - these score as noise.", nrow(risky)))
    }
    decoded <- sum(scan$scan_class == "garbled_input_output_readable_(model_decoded)")
    if (decoded > 0) {
      dfv_log(sprintf("  NOTE: %d file(s) had garbled input but readable output: the model decoded the text itself (not a pipeline step). Result is not guaranteed to repeat.", decoded))
    }
    dfv_log(sprintf("Wrote %s", p_scan))
  }
  
  if (isTRUE(config$write_back_status)) {
    dfv_write_back_status(vac, config, stamp)
  } else {
    dfv_log("write_back_status = FALSE - vacancy_status.csv left untouched (set TRUE to add failure_cause columns).")
  }
  
  dfv_log("")
  dfv_log(sprintf("Wrote %s", p_members))
  dfv_log(sprintf("Wrote %s", p_vac))
  dfv_log(sprintf("Wrote %s", p_summ))
  invisible(list(members = members, vacancies = vac, summary = summ, scan = scan, attempts = attempts))
}

if (sys.nframe() == 0) {
  main_diagnose_failed_vacancies(CONFIG_DIAG)
}