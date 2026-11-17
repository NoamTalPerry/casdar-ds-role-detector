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
#          Jul-Aug 2026, and extended with Claude Sonnet 5.5 (Anthropic),
#          Oct-Nov 2026; reviewed and verified by the author.
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
#   6. If disagreements went to an adjudicator (a pass-3 file in the same
#      format, covering only the disputed review_ids), add it:
#        compare_passes(p1, p2, p3, sensitivity_exclude_ids = CONFIG2$always_include_review_ids)
#      then build_final_labels(p1, p2, p3, crosswalk_path, out_path) to get
#      one final label per review_id, joined to band/score, for analysis
#      (run_threshold_analysis.R reads this file when label_source = "final").
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
  always_include_review_ids = character(0),   # e.g. c("R001", "R002")
  
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
# Agreement + adjudication helpers (used by compare_passes() and
# build_final_labels() below).
# ==============================================================================
VALID_LABELS <- c("No-DS", "Partial DS", "DS")

# Reads one review pass file and validates it. Stops loudly (never silently
# coerces) on duplicate review_id(s) or any blank / unrecognised label, since
# a mislabelled or blank row would quietly distort the agreement figure.
# Matching to the canonical label spelling is case- and whitespace-insensitive.
read_pass <- function(path, what) {
  if (!file.exists(path)) stop(sprintf("%s file not found: %s", what, path))
  d <- utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character", na.strings = character(0))
  missing_cols <- setdiff(c("review_id", "label"), names(d))
  if (length(missing_cols) > 0) {
    stop(sprintf("%s file is missing column(s) %s: %s", what, paste(missing_cols, collapse = ", "), path))
  }
  d$review_id <- trimws(d$review_id)
  dups <- unique(d$review_id[duplicated(d$review_id)])
  if (length(dups) > 0) {
    stop(sprintf("%s file has duplicate review_id(s): %s", what, paste(dups, collapse = ", ")))
  }
  idx <- match(tolower(trimws(d$label)), tolower(VALID_LABELS))
  bad <- is.na(idx)
  if (any(bad)) {
    stop(sprintf(
      "%s file: %d row(s) have a blank or unrecognised label (valid: %s). review_id(s): %s",
      what, sum(bad), paste(VALID_LABELS, collapse = " / "), paste(d$review_id[bad], collapse = ", ")
    ))
  }
  d$label <- VALID_LABELS[idx]
  # Numeric confidence (1-5): takes a leading digit 1-5, so "4" and
  # "4 - unsure about gate 2" both parse as 4. Anything else (blank, free
  # text, missing column) becomes NA rather than a guess.
  conf <- if ("confidence" %in% names(d)) trimws(d$confidence) else rep("", nrow(d))
  d$confidence_num <- ifelse(grepl("^[1-5]([^0-9]|$)", conf), suppressWarnings(as.numeric(substr(conf, 1, 1))), NA_real_)
  d
}

# Collapses the 3-level label to 2 levels. Which side Partial DS belongs on is
# a design choice, so both are supported and compare_passes() reports both
# (cheap, and avoids silently committing to one).
#   "partial_with_ds": Partial DS -> DS   (levels No-DS / DS)
#   "partial_with_no": Partial DS -> No-DS (levels No-DS / DS)
collapse_labels <- function(x, scheme = c("partial_with_ds", "partial_with_no")) {
  scheme <- match.arg(scheme)
  to <- if (scheme == "partial_with_ds") "DS" else "No-DS"
  ifelse(x == "Partial DS", to, x)
}

# Cohen's kappa for two raters over the given label levels. weights = "linear"
# gives linearly weighted kappa (treats No-DS < Partial DS < DS as ordered, so
# a No-DS vs DS clash counts as a bigger disagreement than a Partial DS vs DS
# one). Returns NA when chance agreement is 1 (e.g. both raters used a single
# category throughout), where kappa is undefined.
cohen_kappa <- function(a, b, levels, weights = c("none", "linear")) {
  weights <- match.arg(weights)
  k <- length(levels)
  tab <- table(factor(a, levels = levels), factor(b, levels = levels))
  n <- sum(tab)
  if (n == 0) return(NA_real_)
  p <- tab / n
  rm <- rowSums(p)
  cm <- colSums(p)
  w <- if (weights == "linear" && k > 1) {
    1 - abs(outer(seq_len(k), seq_len(k), "-")) / (k - 1)
  } else {
    diag(k)
  }
  po <- sum(w * p)
  pe <- sum(w * outer(rm, cm))
  if (isTRUE(all.equal(pe, 1))) return(NA_real_)
  (po - pe) / (1 - pe)
}

fmt_kappa <- function(x) if (is.na(x)) "undefined" else sprintf("%.3f", x)

# Three-way-split confidence check. When pass 1, pass 2 and pass 3 all gave
# different labels, the adjudicator's label stands (the adjudicator's verdict
# is final), and the supporting condition is that the adjudicator's
# confidence is strictly higher than BOTH raters' confidence. This returns
# TRUE/FALSE/NA (NA when any confidence is missing or unparseable). The check
# does not change which label wins; it records whether the condition actually
# holds, so the resolution is documented and checkable rather than asserted.
# If you adopt a rule like this after seeing the labels, report it as a
# post-hoc deviation from your labelling protocol.
three_way_rule_met <- function(conf1, conf2, conf3) {
  conf3 > pmax(conf1, conf2)
}

# Prints and returns the agreement block for a merged pass1/pass2 table
# (columns label_pass1, label_pass2).
agreement_block <- function(m, title) {
  n <- nrow(m)
  agree <- m$label_pass1 == m$label_pass2
  cat(sprintf("\n--- %s (n = %d) ---\n", title, n))
  if (n == 0) { cat("No rows.\n"); return(invisible(NULL)) }
  
  res <- list(n = n, raw = mean(agree))
  res$kappa        <- cohen_kappa(m$label_pass1, m$label_pass2, VALID_LABELS, "none")
  res$kappa_linear <- cohen_kappa(m$label_pass1, m$label_pass2, VALID_LABELS, "linear")
  cat(sprintf("3-level raw agreement:     %d of %d (%.1f%%)\n", sum(agree), n, 100 * mean(agree)))
  cat(sprintf("3-level Cohen's kappa:     %s (linear-weighted: %s)\n", fmt_kappa(res$kappa), fmt_kappa(res$kappa_linear)))
  
  for (scheme in c("partial_with_ds", "partial_with_no")) {
    c1 <- collapse_labels(m$label_pass1, scheme)
    c2 <- collapse_labels(m$label_pass2, scheme)
    ag <- c1 == c2
    kp <- cohen_kappa(c1, c2, c("No-DS", "DS"), "none")
    res[[paste0("collapsed_", scheme)]] <- list(raw = mean(ag), kappa = kp)
    cat(sprintf(
      "Collapsed (%s): raw %d of %d (%.1f%%), kappa %s\n",
      if (scheme == "partial_with_ds") "Partial DS -> DS " else "Partial DS -> No-DS",
      sum(ag), n, 100 * mean(ag), fmt_kappa(kp)
    ))
  }
  invisible(res)
}

# ==============================================================================
# compare_passes(): call this MANUALLY once the pass-1 and pass-2 files are
# fully labelled (and, if you have it, the adjudication pass-3 file). Not run
# automatically - there's nothing to compare immediately after main2()
# produces an empty pass-2 file.
#
# Arguments:
#   pass1_path, pass2_path   the labelled pass-1 and pass-2 files.
#   pass3_path               optional: the adjudicator's file covering the
#                            pass-1/pass-2 disagreements. Adds the
#                            adjudication report below. It never changes the
#                            agreement figures, which are always the
#                            PRE-adjudication pass 1 vs pass 2 comparison (an
#                            agreement figure computed after one rater's
#                            verdict is final would be circular).
#   sensitivity_exclude_ids  optional character vector of review_id(s) to drop
#                            for an additional sensitivity line. Pass
#                            CONFIG2$always_include_review_ids here: those
#                            rows were added to pass 2 BECAUSE they looked
#                            disputed/low-confidence in pass 1, not drawn at
#                            random, so they probably pull agreement DOWN
#                            relative to the band-stratified reliability
#                            subsample alone. The headline figure still
#                            includes them; this just shows how much they
#                            matter.
#
# Reports (pass 1 vs pass 2, over the review_id(s) in both files):
#   - 3-level raw agreement, Cohen's kappa, linear-weighted kappa
#   - the same after collapsing to No-DS vs DS, under BOTH placements of
#     Partial DS (with DS, and with No-DS) - see collapse_labels()
#   - a 3x3 confusion matrix and the list of disagreeing review_id(s)
#   - with pass3_path: for each disputed row, what the adjudicator ruled and
#     which rater the adjudicator sided with (pass 1 / pass 2 / neither)
# Kappa on a small reliability subsample is a very rough estimate; treat it as
# indicative, not precise, and say so when reporting it.
#
# Returns (invisibly) a list: merged (pass-1/pass-2/pass-3 labels per row),
# agreement (the numbers above), adjudication (data.frame or NULL).
# ==============================================================================
compare_passes <- function(pass1_path, pass2_path, pass3_path = NULL, sensitivity_exclude_ids = NULL) {
  p1 <- read_pass(pass1_path, "Pass-1")
  p2 <- read_pass(pass2_path, "Pass-2")
  
  not_in_p1 <- setdiff(p2$review_id, p1$review_id)
  if (length(not_in_p1) > 0) {
    stop(sprintf("Pass-2 contains review_id(s) that are not in pass 1: %s. Check you passed the right pass-1/pass-2 pair.", paste(not_in_p1, collapse = ", ")))
  }
  
  m <- merge(p1[, c("review_id", "label")], p2[, c("review_id", "label")], by = "review_id", suffixes = c("_pass1", "_pass2"))
  if (nrow(m) == 0) stop("No matching review_id(s) between the two files - check you passed the right pass-1/pass-2 pair.")
  
  cat(sprintf("Pass 1 vs pass 2 (PRE-adjudication): %d review_id(s) in both files.\n", nrow(m)))
  
  agreement <- list(all = agreement_block(m, "All rows in pass 2"))
  
  if (!is.null(sensitivity_exclude_ids) && length(sensitivity_exclude_ids) > 0) {
    unknown <- setdiff(sensitivity_exclude_ids, m$review_id)
    if (length(unknown) > 0) {
      warning(sprintf("sensitivity_exclude_ids not found in the pass-1/pass-2 overlap (ignored): %s", paste(unknown, collapse = ", ")))
    }
    kept <- m[!(m$review_id %in% sensitivity_exclude_ids), , drop = FALSE]
    agreement$excluding_overrides <- agreement_block(
      kept,
      sprintf("Sensitivity: excluding %s", paste(intersect(sensitivity_exclude_ids, m$review_id), collapse = ", "))
    )
  }
  
  cat("\nConfusion matrix, all rows (rows = pass 1, cols = pass 2):\n")
  print(table(pass1 = factor(m$label_pass1, VALID_LABELS), pass2 = factor(m$label_pass2, VALID_LABELS)))
  
  disagree <- m$label_pass1 != m$label_pass2
  if (any(disagree)) {
    cat("\nDisagreements:\n")
    print(m[disagree, ], row.names = FALSE)
  }
  
  adjudication <- NULL
  if (!is.null(pass3_path)) {
    p3 <- read_pass(pass3_path, "Pass-3 (adjudication)")
    disputed <- m$review_id[disagree]
    
    unresolved <- setdiff(disputed, p3$review_id)
    not_disputed <- setdiff(p3$review_id, disputed)
    if (length(unresolved) > 0) {
      warning(sprintf("%d pass-1/pass-2 disagreement(s) have NO pass-3 ruling: %s", length(unresolved), paste(unresolved, collapse = ", ")))
    }
    if (length(not_disputed) > 0) {
      warning(sprintf("Pass 3 contains %d review_id(s) that were NOT pass-1/pass-2 disagreements: %s", length(not_disputed), paste(not_disputed, collapse = ", ")))
    }
    
    adjudication <- merge(m, p3[, c("review_id", "label")], by = "review_id")
    names(adjudication)[names(adjudication) == "label"] <- "label_pass3"
    adjudication$sided_with <- ifelse(
      adjudication$label_pass3 == adjudication$label_pass1, "pass1",
      ifelse(adjudication$label_pass3 == adjudication$label_pass2, "pass2", "neither")
    )
    adjudication <- adjudication[order(adjudication$review_id), , drop = FALSE]
    adjudication$conf_pass1 <- p1$confidence_num[match(adjudication$review_id, p1$review_id)]
    adjudication$conf_pass2 <- p2$confidence_num[match(adjudication$review_id, p2$review_id)]
    adjudication$conf_pass3 <- p3$confidence_num[match(adjudication$review_id, p3$review_id)]
    adjudication$split_type <- ifelse(adjudication$sided_with == "neither", "three_way", "two_way")
    
    cat(sprintf("\n--- Adjudication (pass 3, %d row(s); adjudicator's verdict is final) ---\n", nrow(adjudication)))
    print(adjudication, row.names = FALSE)
    cat("\nSided with:\n")
    print(table(factor(adjudication$sided_with, c("pass1", "pass2", "neither"))))
    cat("\nPass-3 ruling counts:\n")
    print(table(factor(adjudication$label_pass3, VALID_LABELS)))
    tw <- adjudication$split_type == "three_way"
    if (any(tw)) {
      cat("\nThree-way splits (confidence check: adjudicator confidence should exceed both raters'; label is the adjudicator's either way):\n")
      chk <- adjudication[tw, c("review_id", "label_pass1", "label_pass2", "label_pass3", "conf_pass1", "conf_pass2", "conf_pass3")]
      chk$rule_met <- three_way_rule_met(chk$conf_pass1, chk$conf_pass2, chk$conf_pass3)
      print(chk, row.names = FALSE)
    }
  }
  
  merged <- m
  if (!is.null(adjudication)) {
    merged$label_pass3 <- adjudication$label_pass3[match(merged$review_id, adjudication$review_id)]
  }
  invisible(list(merged = merged, agreement = agreement, adjudication = adjudication))
}

# ==============================================================================
# build_final_labels(): turns the three passes into ONE final label per
# review_id, then (optionally) joins the crosswalk so each row carries its
# vacancy_number, band and score_at_draw_time for the threshold analysis.
# Only call this after review is complete: the crosswalk is the file that
# reveals bands/scores.
#
# Resolution rule (label_source column records which applied):
#   adjudicated_pass3   row is in the pass-3 file: the adjudicator's label wins
#   agreed_pass1_pass2  row was in pass 2 and both raters gave the same label
#   pass1_only          row was never double-labelled: the pass-1 label stands
# Three-way splits (pass 1, 2 and 3 all different): the label is the
# adjudicator's, as for every adjudicated row, supported by the confidence
# check in three_way_rule_met() (adjudicator more confident than either
# rater). resolution_rule records, per row, whether that condition actually
# holds; a warning fires if it does not.
# A row that is in pass 2, disagrees, and has no pass-3 ruling is UNRESOLVED:
# the function stops and lists them rather than guessing.
#
# It also records label_pass1/label_pass2/label_pass3 and changed_from_pass1,
# and prints every row whose final label differs from pass 1 (those are the
# rows that can move per-band proportions or the recall floor). Never edits
# the input files. Refuses to overwrite an existing out_path unless
# overwrite = TRUE.
# ==============================================================================
build_final_labels <- function(pass1_path, pass2_path, pass3_path, crosswalk_path = NULL, out_path = NULL, overwrite = FALSE) {
  p1 <- read_pass(pass1_path, "Pass-1")
  p2 <- read_pass(pass2_path, "Pass-2")
  p3 <- read_pass(pass3_path, "Pass-3 (adjudication)")
  
  for (nm in c("p2", "p3")) {
    extra <- setdiff(get(nm)$review_id, p1$review_id)
    if (length(extra) > 0) {
      stop(sprintf("%s has review_id(s) not in pass 1: %s", if (nm == "p2") "Pass-2" else "Pass-3", paste(extra, collapse = ", ")))
    }
  }
  
  final <- data.frame(review_id = p1$review_id, label_pass1 = p1$label, stringsAsFactors = FALSE)
  final$label_pass2 <- p2$label[match(final$review_id, p2$review_id)]
  final$label_pass3 <- p3$label[match(final$review_id, p3$review_id)]
  final$conf_pass1 <- p1$confidence_num[match(final$review_id, p1$review_id)]
  final$conf_pass2 <- p2$confidence_num[match(final$review_id, p2$review_id)]
  final$conf_pass3 <- p3$confidence_num[match(final$review_id, p3$review_id)]
  
  in_p2 <- !is.na(final$label_pass2)
  in_p3 <- !is.na(final$label_pass3)
  disagree <- in_p2 & final$label_pass1 != final$label_pass2
  
  unresolved <- final$review_id[disagree & !in_p3]
  if (length(unresolved) > 0) {
    stop(sprintf("%d pass-1/pass-2 disagreement(s) have no pass-3 ruling, so no final label can be set: %s", length(unresolved), paste(unresolved, collapse = ", ")))
  }
  odd <- final$review_id[in_p3 & !disagree]
  if (length(odd) > 0) {
    warning(sprintf("Pass 3 covers %d row(s) that were not pass-1/pass-2 disagreements (adjudicator's label still applied): %s", length(odd), paste(odd, collapse = ", ")))
  }
  
  final$label_final <- ifelse(in_p3, final$label_pass3, final$label_pass1)
  final$label_source <- ifelse(in_p3, "adjudicated_pass3", ifelse(in_p2, "agreed_pass1_pass2", "pass1_only"))
  final$changed_from_pass1 <- final$label_final != final$label_pass1
  
  # Split type and resolution rule. A three-way split (all of pass 1, 2, 3
  # differ) is resolved by the adjudicator's label under the three-way
  # confidence check (see three_way_rule_met()); the check is recorded per row,
  # including when its condition is NOT met, which is warned about below.
  final$split_type <- ifelse(
    in_p3 & in_p2 & final$label_pass3 != final$label_pass1 & final$label_pass3 != final$label_pass2,
    "three_way", ifelse(in_p3, "two_way", NA_character_)
  )
  rule_met <- three_way_rule_met(final$conf_pass1, final$conf_pass2, final$conf_pass3)
  final$resolution_rule <- ifelse(
    !in_p3, NA_character_,
    ifelse(final$split_type %in% "three_way",
           ifelse(rule_met %in% TRUE, "adjudicator_final+three_way_confidence_rule", "adjudicator_final;three_way_confidence_rule_NOT_met"),
           "adjudicator_final")
  )
  tw_bad <- final$review_id[final$split_type %in% "three_way" & !(rule_met %in% TRUE)]
  if (length(tw_bad) > 0) {
    warning(sprintf("Three-way split row(s) where the adjudicator's confidence is NOT strictly above both raters' (or a confidence is missing): %s. The adjudicator's label is still used (the adjudicator's verdict is final), but the confidence condition is not met, so check these and disclose accordingly.", paste(tw_bad, collapse = ", ")))
  }
  
  cat(sprintf("Final labels for %d review_id(s).\n", nrow(final)))
  cat("\nLabel source:\n")
  print(table(final$label_source))
  if (any(final$split_type %in% "three_way")) {
    cat("\nThree-way splits (confidence check):\n")
    print(final[final$split_type %in% "three_way", c("review_id", "label_pass1", "label_pass2", "label_pass3", "conf_pass1", "conf_pass2", "conf_pass3", "resolution_rule")], row.names = FALSE)
  }
  cat("\nFinal label counts:\n")
  print(table(factor(final$label_final, VALID_LABELS)))
  cat("\nPass-1 label counts (for comparison):\n")
  print(table(factor(final$label_pass1, VALID_LABELS)))
  if (any(final$changed_from_pass1)) {
    cat("\nRows whose final label differs from pass 1:\n")
    print(final[final$changed_from_pass1, c("review_id", "label_pass1", "label_pass2", "label_pass3", "label_final")], row.names = FALSE)
  } else {
    cat("\nNo row's final label differs from pass 1.\n")
  }
  
  if (!is.null(crosswalk_path)) {
    if (!file.exists(crosswalk_path)) stop(sprintf("Crosswalk not found: %s", crosswalk_path))
    cw <- utils::read.csv(crosswalk_path, stringsAsFactors = FALSE, colClasses = "character", na.strings = character(0))
    no_key <- setdiff(final$review_id, cw$review_id)
    if (length(no_key) > 0) {
      stop(sprintf("%d review_id(s) are not in the crosswalk: %s. Check it belongs to the same snapshot as the pass files.", length(no_key), paste(no_key, collapse = ", ")))
    }
    keep <- intersect(c("vacancy_number", "band", "score_at_draw_time", "position_category", "is_reliability_check"), names(cw))
    idx <- match(final$review_id, cw$review_id)
    for (col in keep) final[[col]] <- cw[[col]][idx]
    final <- final[, c("review_id", keep, setdiff(names(final), c("review_id", keep))), drop = FALSE]
    if ("band" %in% names(final)) {
      cat("\nFinal label by band:\n")
      print(table(band = final$band, label = factor(final$label_final, VALID_LABELS)))
    }
  }
  final <- final[order(final$review_id), , drop = FALSE]
  
  if (!is.null(out_path)) {
    if (file.exists(out_path) && !isTRUE(overwrite)) {
      stop(sprintf("%s already exists; pass overwrite = TRUE to replace it.", out_path))
    }
    utils::write.csv(final, out_path, row.names = FALSE)
    cat(sprintf("\nWrote %s\n", out_path))
  }
  invisible(final)
}

if (sys.nframe() == 0) {
  main2(CONFIG2)
}