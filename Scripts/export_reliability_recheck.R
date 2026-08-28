#!/usr/bin/env Rscript
# ==============================================================================
# export_reliability_recheck.R
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
#   Produce the SECOND review pass for the reliability subsample flagged by
#   draw_negative_validation_sample.R (a subset of rows spread across bands,
#   marked is_reliability_check == TRUE in the crosswalk). Two use cases,
#   same file:
#     - A colleague is available: they label this pass-2 file independently,
#       blind to your pass-1 labels - gives a real inter-rater agreement
#       figure.
#     - Working solo: label this pass-2 file yourself, ideally after some
#       days have passed, so it's a genuine self-recheck rather than just
#       recalling your first answer - gives an intra-rater (self-)agreement
#       figure instead. Recognized substitute in content-analysis
#       methodology when a second rater genuinely isn't available.
#
#   Row order is reshuffled with a DIFFERENT seed than the original draw, so
#   pass-2 position in the file doesn't echo pass-1's own shuffle.
#
#   The band-stratified draw that set is_reliability_check == TRUE happened
#   BEFORE any labels existed, so it has no way to know which specific
#   cases turn out to be worth a second look after the fact (a low-
#   confidence label, a disputed call). CONFIG2$always_include_review_ids
#   forces specific review_id(s) into pass 2 on top of whatever the draw
#   flagged - the auditable way to add them, rather than hand-editing the
#   crosswalk. Which rows came from which source is recorded in a separate
#   pass2_composition_AUDIT_*.csv file, deliberately NOT included in the
#   blind pass-2 file itself - flagging "this one was added because it's
#   disputed" to the reviewer would bias them exactly the way blinding is
#   supposed to prevent.
#
# Data scope (what this repository does and does not include):
#   This repository contains CODE ONLY. It does not include any drawn
#   sample, snapshot folder, or review file produced by
#   draw_negative_validation_sample.R. To run this script yourself, first
#   run draw_negative_validation_sample.R against your own corpus, label
#   its pass-1 blind review file, then point CONFIG2$snapshot_dir below at
#   that run's snapshot folder.
#
# Relationship to Script 1/2/negative-sampling:
#   Sourced draw_negative_validation_sample.R for its logging harness and
#   CONFIG conventions only - does NOT call main_draw_negative_sample() from that script (source()
#   does not trigger it; see that script's own header and
#   run_validation_batch.R's identical convention for why). Never re-reads
#   the live pipeline files - everything here comes from the already-frozen
#   snapshot folder a prior draw run produced.
#
# Usage:
#   1. Edit CONFIG2$snapshot_dir below to point at the specific dated
#      snapshot folder you want to recheck.
#   2. Make sure every reliability-flagged row in that snapshot's pass-1
#      blind file has a non-blank label (this script checks and stops
#      loudly, listing exactly which review_id(s) are still blank, rather
#      than silently producing a partial pass-2 file).
#   3. Rscript export_reliability_recheck.R
#   4. Label the resulting review_blind_pass2_<date>.csv the same way as
#      pass 1 (still blind - no score/band/vacancy_number in this file
#      either).
#   5. Once BOTH passes are labelled, call compare_passes() (defined below)
#      to get agreement stats - see its own header comment.
# ==============================================================================

source("draw_negative_validation_sample.R")   # safe - source() does not auto-run main_draw_negative_sample(); see header above

CONFIG2 <- list(
  # Point this at the exact snapshot folder produced by
  # draw_negative_validation_sample.R for the run you're rechecking - e.g.
  # file.path("..", "Output", "sampling_snapshots", "hard_negative_sample_2026-01-15").
  snapshot_dir = file.path("..", "Output", "sampling_snapshots", "hard_negative_sample_EXAMPLE_DATE"),

  crosswalk_filename    = "sample_key_CROSSWALK_do_not_open_until_review_complete.csv",
  pass1_blind_glob       = "review_blind_pass1_*.csv",   # auto-detected within snapshot_dir

  # Deliberately DIFFERENT from the draw's random_seed, so pass-2's row
  # order isn't a predictable function of the pass-1 shuffle.
  pass2_random_seed = 20260825L,

  # review_id(s) to force into pass 2 regardless of the crosswalk's
  # is_reliability_check flag - for cases that turn out to matter after the
  # fact (e.g. a low-confidence label discovered post-hoc) that the
  # original band-stratified draw didn't happen to select. The crosswalk's
  # flag was set BEFORE any labels existed, so it has no way to know which
  # specific cases turned out to be worth a second look - this is the
  # deliberate, auditable way to add them rather than hand-editing the
  # crosswalk file. Leave empty (character(0)) for a normal run.
  always_include_review_ids = character(0),   # e.g. c("R039", "R010")

  # If TRUE (default), refuses to produce a pass-2 file until every
  # reliability-flagged row has a non-blank pass-1 label - a partial pass-2
  # file would silently produce a meaningless/incomplete agreement figure
  # later. Set FALSE only if you deliberately want to recheck a subset
  # before pass 1 is fully done (not recommended for the reported figure).
  require_pass1_complete = TRUE,

  log_to_file = TRUE,
  log_dir     = file.path("..", "Output", "logs")
)

find_pass1_file <- function(snapshot_dir, glob_pattern) {
  candidates <- Sys.glob(file.path(snapshot_dir, glob_pattern))
  if (length(candidates) == 0) {
    stop(sprintf("No pass-1 blind review file found matching '%s' in %s.", glob_pattern, snapshot_dir))
  }
  if (length(candidates) > 1) {
    log_warn(sprintf("Multiple pass-1 files matched '%s' - using the most recently modified: %s", glob_pattern, candidates[which.max(file.mtime(candidates))]))
    return(candidates[which.max(file.mtime(candidates))])
  }
  candidates[1]
}

main2 <- function(config) {
  .log_con <<- setup_log_file(config)
  on.exit(if (!is.null(.log_con)) close(.log_con), add = TRUE)

  log_msg(sprintf("=== export_reliability_recheck.R run started: %s ===", Sys.time()))

  # colClasses = "character" throughout, na.strings = character(0): without
  # this, read.csv() infers an all-blank column (e.g. every "label" cell
  # still empty before review) as logical and silently turns "" into NA,
  # which would defeat the blank-label check below. Same convention already
  # established in this pipeline's read_status_table() (in
  # similarity_scoring.R), for exactly this reason.
  crosswalk_path <- file.path(config$snapshot_dir, config$crosswalk_filename)
  if (!file.exists(crosswalk_path)) stop(sprintf("Crosswalk not found: %s", crosswalk_path))
  crosswalk <- utils::read.csv(crosswalk_path, stringsAsFactors = FALSE, colClasses = "character", na.strings = character(0))

  pass1_path <- find_pass1_file(config$snapshot_dir, config$pass1_blind_glob)
  log_msg(sprintf("Using pass-1 file: %s", pass1_path))
  pass1 <- utils::read.csv(pass1_path, stringsAsFactors = FALSE, colClasses = "character", na.strings = character(0))

  drawn_reliability_ids <- crosswalk$review_id[toupper(trimws(as.character(crosswalk$is_reliability_check))) == "TRUE"]
  log_msg(sprintf("Reliability subsample flagged in the crosswalk (from the original draw): %d review_id(s).", length(drawn_reliability_ids)))

  override_ids <- config$always_include_review_ids %||% character(0)
  not_in_crosswalk <- setdiff(override_ids, crosswalk$review_id)
  if (length(not_in_crosswalk) > 0) {
    stop(sprintf(
      "always_include_review_ids contains review_id(s) not present in this crosswalk at all: %s. Check these belong to this snapshot_dir, or remove them from the override list.",
      paste(not_in_crosswalk, collapse = ", ")
    ))
  }
  newly_added <- setdiff(override_ids, drawn_reliability_ids)
  if (length(newly_added) > 0) {
    log_msg(sprintf("always_include_review_ids adding %d review_id(s) not originally flagged by the draw: %s.", length(newly_added), paste(newly_added, collapse = ", ")))
  }
  already_flagged <- intersect(override_ids, drawn_reliability_ids)
  if (length(already_flagged) > 0) {
    log_msg(sprintf("always_include_review_ids overlaps %d review_id(s) already flagged by the draw (no-op for these): %s.", length(already_flagged), paste(already_flagged, collapse = ", ")))
  }

  reliability_ids <- union(drawn_reliability_ids, override_ids)
  log_msg(sprintf("Total pass-2 set: %d review_id(s) (%d from the original draw + %d added via override).", length(reliability_ids), length(drawn_reliability_ids), length(newly_added)))

  pass1_rel <- pass1[pass1$review_id %in% reliability_ids, , drop = FALSE]
  if (nrow(pass1_rel) < length(reliability_ids)) {
    missing <- setdiff(reliability_ids, pass1_rel$review_id)
    stop(sprintf(
      "%d reliability-flagged review_id(s) are missing from the pass-1 file entirely (not just unlabelled): %s. Check that this is the right pass-1 file for this crosswalk.",
      length(missing), paste(missing, collapse = ", ")
    ))
  }

  blank_labels <- pass1_rel$review_id[!nzchar(trimws(pass1_rel$label %||% ""))]
  if (length(blank_labels) > 0) {
    msg <- sprintf(
      "%d of %d reliability-flagged row(s) still have a blank 'label' in the pass-1 file: %s.",
      length(blank_labels), nrow(pass1_rel), paste(blank_labels, collapse = ", ")
    )
    if (isTRUE(config$require_pass1_complete)) {
      stop(paste(msg, "Finish labelling pass 1 first, or set CONFIG2$require_pass1_complete = FALSE to override (not recommended)."))
    } else {
      log_warn(paste(msg, "Proceeding anyway because require_pass1_complete = FALSE - the eventual agreement figure will be incomplete."))
    }
  }

  set.seed(config$pass2_random_seed)
  
  # Output columns (review_blind_pass2_<date>.csv) - same shape as pass 1's
  # blind review file (see draw_negative_validation_sample.R's write_outputs()
  # column glossary for review_id/position_title/position_category/job_text/
  # label/confidence/justification/reviewer/date_reviewed), except:
  #   reviewer_pass  - fixed at 2 for this file
  pass2 <- pass1_rel[sample(nrow(pass1_rel)), c("review_id", "position_title", "position_category", "job_text"), drop = FALSE]
  pass2$label            <- ""
  pass2$confidence         <- ""
  pass2$justification       <- ""
  pass2$reviewer              <- ""
  pass2$date_reviewed          <- ""
  pass2$reviewer_pass            <- 2L
  # NOTE: deliberately NOT adding an "added_via_override" column here - this
  # file is what the reviewer actually labels, and flagging "this one was
  # specifically added because it's disputed" would bias them exactly the
  # way blinding is supposed to prevent. That audit info goes in a separate
  # file below instead (never opened until after labelling, same pattern as
  # the original crosswalk).

  stamp <- format(Sys.time(), "%Y-%m-%d")
  out_path <- file.path(config$snapshot_dir, sprintf("review_blind_pass2_%s.csv", stamp))
  utils::write.csv(pass2, out_path, row.names = FALSE)
  log_msg(sprintf("Wrote pass-2 blind file (%d rows, reshuffled with seed %d): %s", nrow(pass2), config$pass2_random_seed, out_path))

  # Output columns (pass2_composition_AUDIT_do_not_open_until_review_complete_<date>.csv)
  # - NOT shown to the reviewer; opens only after labelling is complete:
  #   review_id             - matches the pass-2 file's review_id
  #   added_via_override       - TRUE if this row was forced into pass 2 via
  #                             CONFIG2$always_include_review_ids rather than
  #                             originally flagged by the draw
  audit <- data.frame(review_id = pass1_rel$review_id, added_via_override = pass1_rel$review_id %in% newly_added)
  audit_path <- file.path(config$snapshot_dir, sprintf("pass2_composition_AUDIT_do_not_open_until_review_complete_%s.csv", stamp))
  utils::write.csv(audit, audit_path, row.names = FALSE)
  log_msg(sprintf("Wrote pass-2 composition audit file (not for the reviewer - which review_id(s) were override-added vs. originally drawn): %s", audit_path))

  log_msg("Done.")
  invisible(list(pass2_path = out_path, audit_path = audit_path))
}

# ==============================================================================
# compare_passes(): call this MANUALLY once both pass-1 and pass-2 files are
# fully labelled. Not run automatically - there's nothing to compare
# immediately after main2() produces an empty pass-2 file.
#
# Reports: raw agreement (%), a 3x3 confusion matrix over the No-DS/Partial
# DS/DS label, and lists the specific review_id(s) that disagreed (useful
# for a report appendix / for deciding whether a disagreement needs a
# "talk it through" discussion, a practice this project used for its own
# borderline cases).
# ==============================================================================
compare_passes <- function(pass1_path, pass2_path) {
  p1 <- utils::read.csv(pass1_path, stringsAsFactors = FALSE, colClasses = "character", na.strings = character(0))
  p2 <- utils::read.csv(pass2_path, stringsAsFactors = FALSE, colClasses = "character", na.strings = character(0))
  m <- merge(p1[, c("review_id", "label")], p2[, c("review_id", "label")], by = "review_id", suffixes = c("_pass1", "_pass2"))
  if (nrow(m) == 0) stop("No matching review_id(s) between the two files - check you passed the right pass-1/pass-2 pair.")

  agree <- m$label_pass1 == m$label_pass2
  cat(sprintf("Agreement: %d of %d (%.1f%%)\n", sum(agree), nrow(m), 100 * mean(agree)))
  cat("\nConfusion matrix (rows = pass 1, cols = pass 2):\n")
  print(table(pass1 = m$label_pass1, pass2 = m$label_pass2))
  if (any(!agree)) {
    cat("\nDisagreements:\n")
    print(m[!agree, ])
  }
  invisible(m)
}

if (sys.nframe() == 0) {
  main2(CONFIG2)
}
