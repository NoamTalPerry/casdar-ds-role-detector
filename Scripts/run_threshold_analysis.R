#!/usr/bin/env Rscript
# ==============================================================================
# run_threshold_analysis.R
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
#   Build the ROC and precision-recall picture needed to pick an operating
#   threshold, from the labelled negative-validation sample (No-DS/Partial
#   DS/DS) plus a current external positive set. The external set used by
#   this project is Liu, R. (2026a). Data steward job descriptions: A
#   European dataset (Version 1.0) [Dataset]. Zenodo.
#   https://doi.org/10.5281/zenodo.20636091 - curated and kindly shared by
#   the author; see also the companion gap-analysis report, Liu, R. (2026b),
#   https://doi.org/10.5281/zenodo.20780043. Substitute your own external
#   positive set's citation if you adapt this script.
#
#   Supports comparing MULTIPLE score definitions in one run (e.g. document
#   vs. section vs. item granularity, or a single sub-score like
#   score_responsibilities alone) - see CONFIG$score_specs. No new scoring
#   or embedding work needed for this: Script 2 already computes all three
#   granularities for every vacancy, so this just reads different columns
#   out of files that already exist.
#
# Data scope (what this repository does and does not include):
#   This repository contains CODE ONLY. It does not include job-ad PDFs,
#   the master template, any similarity_scores.csv output, or any sampling
#   snapshot. To run this script yourself, first produce a snapshot with
#   draw_negative_validation_sample.R and score your own corpus with
#   similarity_scoring.R.
#
# Methodology (read this before changing the weighting logic):
#
#   The main-corpus sample is a DISPROPORTIONATE STRATIFIED draw, stratified
#   BY DOCUMENT-LEVEL SCORE specifically (see draw_negative_validation_sample.R).
#   Naively pooling all rows unweighted biases not just precision but
#   TPR/FPR too, since a true-DS job that happened to score high (drawn
#   from Band 1 at a higher sampling probability) is over-represented
#   relative to one that scored low (drawn from Band 4 at a lower
#   probability) if you just count rows. Every rate computed from the
#   main-corpus sample - TPR, FPR, precision, recall, F1, AUC - uses one
#   consistent inverse-probability-weighted confusion matrix, not a naive
#   count.
#
#   IMPORTANT for the multi-granularity comparison specifically: the band
#   weights depend only on which band a vacancy was DRAWN INTO (a fact about
#   the sampling design, fixed at draw time) - NOT on which score you
#   evaluate afterward. So the same weights are valid whether you're
#   evaluating document, section, or item-level scores on this sample. What
#   changes per spec is only the score column fed into the sweep - band
#   recomputation happens ONCE, not once per spec. One real caveat: since
#   the sample was drawn dense near the DOCUMENT-level boundary, a
#   granularity that doesn't correlate well with document-level score may
#   get a noisier (higher-variance) estimate here, not a biased one - the
#   sample just isn't necessarily dense where THAT granularity's boundary
#   sits.
#
#   The external positive set (Liu, 2026a - see the Purpose section above)
#   is NOT part of this sampling frame - no defined inclusion probability
#   relative to the main-corpus sample, so it's reported as a separate,
#   unweighted cross-check column, never folded into the weighted estimate.
#
#   Two DIFFERENT files both called "validation scores" show up here:
#     - validation_similarity_scores_snapshot.csv (inside the dated snapshot
#       folder) = the ORIGINAL external-positive scores file, frozen at
#       draw time. Used ONLY to recompute the Band 1 floor exactly as it
#       was when the sample was actually drawn.
#     - CONFIG$current_validation_scores_path (live, current external
#       positive set) = used ONLY for the external-recall cross-check
#       column, across all requested score_specs.
#
# Output (all in CONFIG$output_dir):
#   - threshold_sweep_all.csv        - ONE aggregated table, every score_spec
#                                        x partial-scheme combination, keyed
#                                        by score_spec/partial_scheme columns
#   - auc_summary.csv                 - one row per score_spec x scheme: AUC
#                                        + how many sampled vacancies had a
#                                        usable score for that spec
#   - main_labeled_with_all_scores.csv - wide export: one row per
#                                        sampled vacancy, one column per
#                                        requested score_spec, plus
#                                        band/weight/label
#   - threshold_metrics_<spec>_<scheme>.png - one plot per spec x scheme
#                                        (precision/recall/F1/external
#                                        recall vs. threshold)
#   - roc_comparison_<scheme>.png     - ONE plot per scheme, all score_specs
#                                        overlaid - the "which granularity
#                                        wins" comparison view
#
#   All five of these have CONFIG$version_suffix (if set) appended before
#   the extension - e.g. "v2_adjacent_role" -> threshold_sweep_all_v2_adjacent_role.csv.
#   Same mechanism/semantics as every other script in the
#   run_master_template_version.R chain - lets you re-run this script under
#   different template versions/toggles and pull the resulting files
#   together for a side-by-side comparison without one run's output
#   overwriting another's.
#
# Usage:
#   Fill in CONFIG below, then: Rscript run_threshold_analysis.R
# ==============================================================================

CONFIG <- list(
  # The dated snapshot folder from the original draw run - contains the
  # crosswalk, the completed pass-1 blind file, and the frozen input copies.
  # e.g. file.path("..", "Output", "sampling_snapshots", "hard_negative_sample_2026-01-15")
  #
  snapshot_dir = file.path("..", "Output", "sampling_snapshots", "hard_negative_sample_EXAMPLE_DATE"),
  
  
  # The CURRENT (live) positive validation set - NOT inside the snapshot
  # folder. Used only for the external-recall cross-check column.
  current_validation_scores_path = file.path("..", "Validation_Output",
                                             "validation_similarity_scores.csv"),
  
  crosswalk_filename = "sample_key_CROSSWALK_do_not_open_until_review_complete.csv",
  pass1_blind_glob    = "review_blind_pass1_*.csv",
  
  # WHICH LABELS feed the analysis (the bands/weights never depend on this):
  #   "final" (default) = the one adjudicated label per review_id written by
  #       build_final_labels() in export_reliability_recheck.R (pass 1, with
  #       pass-2 agreements and the adjudicator's pass-3 rulings resolved;
  #       three-way splits go to the adjudicator - see three_way_rule_met()
  #       there). Read from final_labels_glob in
  #       snapshot_dir, column final_label_column.
  #   "pass1" = the original single-reviewer pass-1 labels, i.e. the
  #       PRE-adjudication analysis. Use this if there was no second pass /
  #       adjudication, or to see how much adjudication moved the thresholds.
  # Both write the same output filenames, so give each run its own
  # version_suffix (e.g. "pass1_labels" / "final_labels") or move the previous
  # output_dir aside first, otherwise the second run overwrites the first.
  label_source      = "final",
  final_labels_glob  = "final_labels_*.csv",
  final_label_column  = "label_final",
  vacancy_status_snapshot_filename    = "vacancy_status_snapshot.csv",
  similarity_scores_snapshot_filename  = "similarity_scores_snapshot.csv",
  validation_scores_snapshot_filename   = "validation_similarity_scores_snapshot.csv",
  manifest_filename                      = "manifest.txt",
  
  # Fixed at "document" - this is what the ORIGINAL draw was stratified on,
  # and band population/weights are only meaningful relative to that. Do not
  # change this to test other granularities - use score_specs below instead.
  granularity = "document",
  
  # Must match what the original draw used, or population recomputation
  # won't line up with how the sample was actually drawn.
  eligible_statuses = c("complete", "partial"),
  exclude_flagged_for_review = TRUE,
  
  # --- What to actually compare -----------------------------------------------
  # One entry per score definition you want swept. 'name' is used in
  # filenames/columns - keep it short and filename-safe. 'granularity' picks
  # the row (document/section/item) out of similarity_scores.csv and the
  # current validation scores file; 'score_column' picks which column on
  # that row to use (usually "score_overall"; "score_responsibilities" or
  # "score_skills" are only non-NA at section/item granularity, not
  # document). Add or remove entries freely - the whole pipeline loops over
  # this list, no code changes needed to test a different combination.
  #
  # Optional per-spec 'scores_path'/'validation_scores_path': by default a
  # spec reads main-corpus scores from the frozen snapshot
  # (similarity_scores_snapshot.csv) and external-positive scores from
  # CONFIG$current_validation_scores_path below - the same files every
  # other spec uses. Set one or both on a specific spec to instead read
  # from a DIFFERENT file entirely - e.g. scores computed under a modified
  # master template (the ADJACENT_ROLE ablation), scored separately via
  # similarity_scoring.R's only_vacancy_numbers (main-corpus side) and
  # validation_mode = TRUE (external side) into their own output files.
  # Band weights are unaffected by this - they depend only on how the
  # sample was SAMPLED (frozen, historical), never on which scores you
  # evaluate on them afterward, so the same weights stay valid for a spec
  # using entirely different score files. See
  # recompute_bands_and_population() - nothing here touches that.
  score_specs = list(
    list(name = "document", granularity = "document", score_column = "score_overall"),
    list(name = "section",  granularity = "section",  score_column = "score_overall"),
    list(name = "item",     granularity = "item",     score_column = "score_overall"),
    list(name = "section_responsibilities", granularity = "section", score_column = "score_responsibilities"),
    list(name = "section_skills",           granularity = "section", score_column = "score_skills")
    
    # Example: same score_skills definition, but scored under a DIFFERENT
    # master template (e.g. master_template_v2.txt with the ADJACENT_ROLE
    # pool toggled on) - point at wherever THAT run's output went, instead
    # of the frozen snapshot / current live file every other spec uses:
    # , list(name = "section_skills_v2", granularity = "section", score_column = "score_skills",
    #        scores_path = file.path("..", "Output", "similarity_scores_v2.csv"),
    #        validation_scores_path = file.path("..", "Validation_Output", "validation_similarity_scores_v2.csv"))
  ),
  
  output_dir = file.path("..", "Output", "threshold_analysis"),
  
  # Which MASTER TEMPLATE VERSION (or, more generally, which run of this
  # script) produced the CSV/PNG outputs listed above - appended via
  # apply_version_suffix() to every one of them. Same mechanism/semantics
  # as every other script in the run_master_template_version.R chain (see
  # analyze_item_coverage.R's version_suffix comment). Set automatically by
  # that wrapper; NULL (no suffix) for a plain standalone run.
  version_suffix = NULL,
  
  log_to_file = TRUE,
  log_dir     = file.path("..", "Output", "logs")
)

# ==============================================================================
# Logging (same harness as the other scripts in this pipeline)
# ==============================================================================
.log_con <- NULL
log_msg <- function(text) { message(text); if (!is.null(.log_con)) cat(text, "\n", file = .log_con, sep = "") }
log_warn <- function(text) log_msg(paste("WARNING:", text))
setup_log_file <- function(config) {
  if (!isTRUE(config$log_to_file)) return(NULL)
  if (!dir.exists(config$log_dir)) dir.create(config$log_dir, recursive = TRUE)
  log_path <- file.path(config$log_dir, sprintf("threshold_analysis_run_log_%s.txt", format(Sys.time(), "%Y%m%d_%H%M%S")))
  con <- file(log_path, open = "at")
  message(sprintf("Logging this run to: %s", log_path))
  con
}

find_one_file <- function(dir, glob_pattern, label) {
  candidates <- Sys.glob(file.path(dir, glob_pattern))
  if (length(candidates) == 0) stop(sprintf("No %s found matching '%s' in %s.", label, glob_pattern, dir))
  if (length(candidates) > 1) {
    log_warn(sprintf("Multiple %s matched - using the most recently modified: %s", label, candidates[which.max(file.mtime(candidates))]))
    return(candidates[which.max(file.mtime(candidates))])
  }
  candidates[1]
}

sanitize_name <- function(x) gsub("[^A-Za-z0-9_-]", "_", x)

`%||%` <- function(a, b) if (is.null(a)) b else a

# Same append-before-extension behavior duplicated identically across every
# script in the run_master_template_version.R chain (analyze_item_coverage.R,
# triangulate_item_coverage.R, similarity_scoring.R). This is deliberate -
# it keeps each script standalone and runnable on its own; duplicating an
# IDENTICAL implementation under the same name across scripts is safe, only
# a DIFFERING implementation under a shared name would risk a silent
# collision if two scripts were ever sourced together. Distinct from this
# project's other suffix concept (analyze_item_coverage.R's output_suffix,
# which labels GROUP membership) - version_suffix labels which MASTER
# TEMPLATE VERSION produced the file, a different axis entirely. NULL/empty
# suffix is a no-op.
apply_version_suffix <- function(path, suffix) {
  if (is.null(path) || is.null(suffix) || !nzchar(suffix)) return(path)
  ext  <- tools::file_ext(path)
  base <- tools::file_path_sans_ext(path)
  paste0(base, "_", suffix, if (nzchar(ext)) paste0(".", ext) else "")
}

# ==============================================================================
# Step 1: Recompute band cutoffs + population sizes from the FROZEN snapshot
# inputs - must reproduce exactly what the original draw used, since the
# weights depend on it. Also returns the raw scores table so per-spec score
# lookups (Step 4) don't need to re-read the file.
# ==============================================================================
recompute_bands_and_population <- function(config) {
  status_path <- file.path(config$snapshot_dir, config$vacancy_status_snapshot_filename)
  scores_path <- file.path(config$snapshot_dir, config$similarity_scores_snapshot_filename)
  origvalid_path <- file.path(config$snapshot_dir, config$validation_scores_snapshot_filename)
  for (p in c(status_path, scores_path, origvalid_path)) {
    if (!file.exists(p)) stop(sprintf("Expected frozen snapshot file not found: %s", p))
  }
  
  status <- utils::read.csv(status_path, stringsAsFactors = FALSE, colClasses = "character", na.strings = character(0))
  scores <- utils::read.csv(scores_path, stringsAsFactors = FALSE, na.strings = "NA")
  scores$vacancy_number <- as.character(scores$vacancy_number)
  origvalid <- utils::read.csv(origvalid_path, stringsAsFactors = FALSE, na.strings = "NA")
  
  ov <- origvalid[origvalid$granularity == config$granularity & !is.na(origvalid$score_overall), ]
  if (nrow(ov) == 0) stop("Frozen validation_similarity_scores_snapshot.csv has no usable rows at this granularity - can't reconstruct the original Band 1 floor.")
  band1_floor <- min(ov$score_overall)
  
  keep_status <- status$status %in% config$eligible_statuses
  flagged <- toupper(trimws(status$flagged_for_review)) == "TRUE"
  keep_flag <- if (isTRUE(config$exclude_flagged_for_review)) !flagged else rep(TRUE, nrow(status))
  elig_status <- status[keep_status & keep_flag, , drop = FALSE]
  
  sc_doc <- scores[scores$granularity == config$granularity & !is.na(scores$score_overall), , drop = FALSE]
  
  merged <- merge(
    elig_status[, "vacancy_number", drop = FALSE],
    sc_doc[, c("vacancy_number", "score_overall")],
    by = "vacancy_number"
  )
  merged <- merged[!duplicated(merged$vacancy_number), , drop = FALSE]
  
  q <- stats::quantile(merged$score_overall, probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE)
  band3_floor <- q[1]
  band2_floor <- q[2]
  
  merged$band <- with(merged, ifelse(
    score_overall >= band1_floor, "band_1", ifelse(
      score_overall >= band2_floor, "band_2", ifelse(
        score_overall >= band3_floor, "band_3", "band_4"
      ))))
  
  population <- table(merged$band)
  log_msg(sprintf(
    "Recomputed from frozen snapshot: band_1 >= %.4f | band_2 [%.4f, %.4f) | band_3 [%.4f, %.4f) | band_4 < %.4f",
    band1_floor, band2_floor, band1_floor, band3_floor, band2_floor, band3_floor
  ))
  log_msg("Recomputed eligible population per band:")
  print(population)
  
  list(
    cutoffs = c(band_1_floor = band1_floor, band_2_floor = band2_floor, band_3_floor = band3_floor),
    population = population,
    raw_scores = scores
  )
}

verify_against_manifest <- function(config, recomputed) {
  manifest_path <- file.path(config$snapshot_dir, config$manifest_filename)
  if (!file.exists(manifest_path)) {
    log_warn(sprintf("manifest.txt not found at %s - skipping the cross-check against the originally-logged cutoffs.", manifest_path))
    return(invisible(NULL))
  }
  lines <- readLines(manifest_path, warn = FALSE)
  extract_num <- function(pattern) {
    hit <- grep(pattern, lines, value = TRUE)
    if (length(hit) == 0) return(NA_real_)
    as.numeric(sub(".*?([0-9]+\\.[0-9]+).*", "\\1", hit[1]))
  }
  logged <- c(
    band_1_floor = extract_num("band_1 floor"),
    band_2_floor = extract_num("band_2 floor"),
    band_3_floor = extract_num("band_3 floor")
  )
  mismatch <- abs(logged - recomputed$cutoffs) > 1e-4
  mismatch[is.na(mismatch)] <- TRUE
  if (any(mismatch)) {
    log_warn(sprintf(
      "Recomputed cutoffs don't match manifest.txt's logged values (recomputed: %s | manifest: %s) - double-check this is the right snapshot_dir before trusting the weights.",
      paste(sprintf("%s=%.4f", names(recomputed$cutoffs), recomputed$cutoffs), collapse = ", "),
      paste(sprintf("%s=%.4f", names(logged), logged), collapse = ", ")
    ))
  } else {
    log_msg("Recomputed band cutoffs match manifest.txt exactly - snapshot integrity confirmed.")
  }
}

# ==============================================================================
# Step 2: Load the labelled main-corpus sample (crosswalk + labels, joined on
# review_id; labels come from CONFIG$label_source - the adjudicated final file
# by default, or the original pass-1 file) and attach the inverse-probability
# weight per band. Does NOT
# attach a score here - that happens per score_spec in Step 4, since this
# base sample is reused across every spec.
# ==============================================================================
load_labeled_sample_base <- function(config, population) {
  crosswalk_path <- file.path(config$snapshot_dir, config$crosswalk_filename)
  if (!file.exists(crosswalk_path)) stop(sprintf("Crosswalk not found: %s", crosswalk_path))
  crosswalk <- utils::read.csv(crosswalk_path, stringsAsFactors = FALSE, colClasses = "character", na.strings = character(0))
  
  label_source <- config$label_source %||% "final"
  if (identical(label_source, "pass1")) {
    labels_path <- find_one_file(config$snapshot_dir, config$pass1_blind_glob, "pass-1 blind review file")
    log_msg(sprintf("Label source = pass1 (PRE-adjudication). Using pass-1 file: %s", labels_path))
    lab <- utils::read.csv(labels_path, stringsAsFactors = FALSE, colClasses = "character", na.strings = character(0))
    label_col <- "label"
  } else if (identical(label_source, "final")) {
    labels_path <- find_one_file(config$snapshot_dir, config$final_labels_glob, "final labels file (from build_final_labels())")
    log_msg(sprintf("Label source = final (adjudicated). Using final labels file: %s", labels_path))
    lab <- utils::read.csv(labels_path, stringsAsFactors = FALSE, colClasses = "character", na.strings = character(0))
    label_col <- config$final_label_column
  } else {
    stop(sprintf("CONFIG$label_source must be 'final' or 'pass1', got '%s'.", label_source))
  }
  if (!all(c("review_id", label_col) %in% names(lab))) {
    stop(sprintf("Labels file %s must have columns review_id and %s.", labels_path, label_col))
  }
  lab$review_id <- trimws(lab$review_id)
  dup_ids <- unique(lab$review_id[duplicated(lab$review_id)])
  if (length(dup_ids) > 0) stop(sprintf("Labels file has duplicate review_id(s): %s", paste(dup_ids, collapse = ", ")))
  lab$label <- lab[[label_col]]
  
  blank <- !nzchar(trimws(lab$label))
  if (any(blank)) {
    stop(sprintf(
      "%d row(s) in the labels file still have a blank label (%s) - finish labelling before running the threshold analysis.",
      sum(blank), paste(lab$review_id[blank], collapse = ", ")
    ))
  }
  valid_labels <- c("No-DS", "Partial DS", "DS")
  bad <- !(trimws(lab$label) %in% valid_labels)
  if (any(bad)) {
    stop(sprintf(
      "%d row(s) have a label that isn't exactly one of %s: %s (review_id: %s) - fix these before proceeding, this script won't guess.",
      sum(bad), paste(valid_labels, collapse = "/"),
      paste(unique(lab$label[bad]), collapse = ", "),
      paste(lab$review_id[bad], collapse = ", ")
    ))
  }
  
  # Integrity check: if the final file carries vacancy_number/band, they must
  # agree with the crosswalk, or the labels belong to a different snapshot.
  if (all(c("vacancy_number", "band") %in% names(lab))) {
    chk <- merge(crosswalk[, c("review_id", "vacancy_number", "band")], lab[, c("review_id", "vacancy_number", "band")], by = "review_id", suffixes = c("_cw", "_lab"))
    mismatch <- chk$review_id[chk$vacancy_number_cw != chk$vacancy_number_lab | chk$band_cw != chk$band_lab]
    if (length(mismatch) > 0) {
      stop(sprintf("vacancy_number/band in the labels file disagree with the crosswalk for review_id(s): %s - wrong snapshot?", paste(mismatch, collapse = ", ")))
    }
  }
  
  # Optional audit columns carried through to the wide export when present.
  extra_cols <- intersect(c("label_pass1", "label_source"), names(lab))
  m <- merge(crosswalk, lab[, c("review_id", "label", extra_cols)], by = "review_id")
  if (nrow(m) != nrow(crosswalk)) {
    log_warn(sprintf("Crosswalk has %d rows but only %d matched a labelled pass-1 row - check the labels file is the right one for this crosswalk.", nrow(crosswalk), nrow(m)))
  }
  
  m$score_document_at_draw_time <- as.numeric(m$score_at_draw_time)
  drawn_n <- table(m$band)
  bands <- names(drawn_n)
  weight_lookup <- setNames(as.numeric(population[bands]) / as.numeric(drawn_n[bands]), bands)
  m$weight <- weight_lookup[m$band]
  
  log_msg("Drawn n per band (from crosswalk) and resulting inverse-probability weight:")
  print(data.frame(band = bands, population = as.numeric(population[bands]), drawn = as.numeric(drawn_n[bands]), weight = weight_lookup[bands]))
  log_msg("Raw (unweighted) label counts by band - small-n bands here mean a wide-uncertainty weighted estimate for that stratum:")
  print(table(m$band, m$label))
  log_msg("Label counts used in this run:")
  print(table(factor(m$label, valid_labels)))
  if ("label_pass1" %in% names(m)) {
    n_changed <- sum(m$label != m$label_pass1)
    log_msg(sprintf("%d of %d rows have a label that differs from the pass-1 label (adjudication / second-rater effect).", n_changed, nrow(m)))
  }
  
  m
}

# ==============================================================================
# Step 3: Load the CURRENT external positive set (see Liu, 2026a in the
# header comment - all granularities) - unweighted, used only for the
# cross-check column. Read once, filtered per spec in Step 4.
# ==============================================================================
load_validation_raw <- function(config) {
  if (!file.exists(config$current_validation_scores_path)) stop(sprintf("Current validation scores file not found: %s", config$current_validation_scores_path))
  val <- utils::read.csv(config$current_validation_scores_path, stringsAsFactors = FALSE, na.strings = "NA")
  log_msg(sprintf("External positive set loaded: %d vacancies x %d granularity rows each.", length(unique(val$vacancy_number)), length(unique(val$granularity))))
  val
}

# ==============================================================================
# Step 4: Per-spec score attachment. Pulls the requested (granularity,
# score_column) out of the raw tables for the main-corpus sample and the
# external positives. Drops rows with no usable score for this spec,
# logging how many.
# ==============================================================================

# Loads an alternate scores file (spec$scores_path / spec$validation_scores_path)
# on first use, cached thereafter by resolved path so multiple specs
# pointing at the same override file don't reload it repeatedly. alt_cache
# is a plain environment, created once in main_threshold_analysis() and threaded through -
# same exists()/get()/assign() pattern used for similarity_scoring.R's own
# embedding cache, for anyone comparing the two scripts side by side.
load_alt_scores_cached <- function(path, alt_cache) {
  key <- normalizePath(path, mustWork = FALSE)
  if (exists(key, envir = alt_cache, inherits = FALSE)) return(get(key, envir = alt_cache))
  if (!file.exists(path)) stop(sprintf("score_spec references a scores_path/validation_scores_path that doesn't exist: %s", path))
  df <- utils::read.csv(path, stringsAsFactors = FALSE, na.strings = "NA")
  df$vacancy_number <- as.character(df$vacancy_number)
  assign(key, df, envir = alt_cache)
  log_msg(sprintf("Loaded alternate scores file (spec override): %s (%d rows).", path, nrow(df)))
  df
}

attach_score_for_spec <- function(labeled_sample_base, spec, raw_scores, alt_cache = NULL) {
  source_table <- if (!is.null(spec$scores_path)) load_alt_scores_cached(spec$scores_path, alt_cache) else raw_scores
  sc <- source_table[source_table$granularity == spec$granularity, c("vacancy_number", spec$score_column), drop = FALSE]
  names(sc)[2] <- "score"
  sc <- sc[!duplicated(sc$vacancy_number), , drop = FALSE]
  
  d <- merge(labeled_sample_base, sc, by = "vacancy_number", all.x = TRUE)
  n_missing <- sum(is.na(d$score))
  if (n_missing > 0) {
    log_warn(sprintf(
      "score_spec '%s': %d of %d main-corpus sample rows have no usable '%s' at granularity '%s'%s - dropped from this spec's analysis.",
      spec$name, n_missing, nrow(d), spec$score_column, spec$granularity,
      if (!is.null(spec$scores_path)) sprintf(" (source: %s)", spec$scores_path) else ""
    ))
  }
  d[!is.na(d$score), , drop = FALSE]
}

attach_score_for_spec_external <- function(validation_raw, spec, alt_cache = NULL) {
  source_table <- if (!is.null(spec$validation_scores_path)) load_alt_scores_cached(spec$validation_scores_path, alt_cache) else validation_raw
  sc <- source_table[source_table$granularity == spec$granularity, spec$score_column]
  sc <- sc[!is.na(sc)]
  if (length(sc) == 0) {
    log_warn(sprintf(
      "score_spec '%s': external validation set has no usable '%s' at granularity '%s'%s.",
      spec$name, spec$score_column, spec$granularity,
      if (!is.null(spec$validation_scores_path)) sprintf(" (source: %s)", spec$validation_scores_path) else ""
    ))
  }
  sc
}

# ==============================================================================
# Step 5: Build the main-corpus positive/negative class per partial-handling
# scheme. Unchanged by the multi-spec extension - operates on 'label', not
# on the score column.
# ==============================================================================
build_scheme_data <- function(labeled_sample, scheme) {
  if (scheme == "exclude") {
    d <- labeled_sample[labeled_sample$label != "Partial DS", , drop = FALSE]
    d$class <- ifelse(d$label == "DS", "positive", "negative")
  } else if (scheme == "collapse") {
    d <- labeled_sample
    d$class <- ifelse(d$label %in% c("DS", "Partial DS"), "positive", "negative")
  } else {
    stop("scheme must be 'exclude' or 'collapse'")
  }
  d
}

# ==============================================================================
# Step 6: Weighted confusion-matrix sweep across a threshold grid, plus the
# external-recall cross-check evaluated at the same grid points. Operates on
# the generic 'score' column now (attached per-spec in Step 4).
# ==============================================================================
weighted_confusion_sweep <- function(scheme_data, external_scores) {
  scores <- scheme_data$score
  eps <- .Machine$double.eps * 100
  thresholds <- sort(unique(c(min(scores) - eps, scores, max(scores) + eps)))
  
  pos <- scheme_data[scheme_data$class == "positive", , drop = FALSE]
  neg <- scheme_data[scheme_data$class == "negative", , drop = FALSE]
  pos_weight_total <- sum(pos$weight)
  neg_weight_total <- sum(neg$weight)
  
  rows <- lapply(thresholds, function(t) {
    wtp <- sum(pos$weight[pos$score >= t])
    wfn <- pos_weight_total - wtp
    wfp <- sum(neg$weight[neg$score >= t])
    wtn <- neg_weight_total - wfp
    
    tpr <- if (pos_weight_total > 0) wtp / pos_weight_total else NA_real_
    fpr <- if (neg_weight_total > 0) wfp / neg_weight_total else NA_real_
    precision <- if ((wtp + wfp) > 0) wtp / (wtp + wfp) else NA_real_
    recall <- tpr
    f1 <- if (!is.na(precision) && !is.na(recall) && (precision + recall) > 0) 2 * precision * recall / (precision + recall) else NA_real_
    
    ext_recall <- if (length(external_scores) > 0) mean(external_scores >= t) else NA_real_
    
    # Output columns (threshold_sweep_all.csv), one row per (score_spec,
    # partial_scheme, threshold) combination swept:
    #   score_spec / partial_scheme  - added by the caller: which score
    #                                 definition and which Partial-DS handling
    #                                 scheme ("exclude" or "collapse") this row
    #                                 belongs to
    #   threshold                      - the candidate score cutoff this row
    #                                    evaluates
    #   weighted_TPR / weighted_FPR /    - inverse-probability-weighted
    #   weighted_precision /               confusion-matrix rates at this
    #   weighted_recall / weighted_F1        threshold (see this script's
    #                                        Methodology section for why
    #                                        weighting is required)
    #   external_recall_at_threshold           - unweighted recall on the
    #                                          external positive set at this
    #                                          threshold (see the Purpose
    #                                          section for what that set is)
    #   n_TP_raw / n_FN_raw / n_FP_raw /         - RAW (unweighted) counts at
    #   n_TN_raw                                   this threshold - diagnostic
    #                                              only, never used for
    #                                              TPR/FPR/precision/recall
    #                                              themselves
    data.frame(
      threshold = t,
      weighted_TPR = tpr, weighted_FPR = fpr,
      weighted_precision = precision, weighted_recall = recall, weighted_F1 = f1,
      external_recall_at_threshold = ext_recall,
      n_TP_raw = sum(pos$score >= t), n_FN_raw = sum(pos$score < t),
      n_FP_raw = sum(neg$score >= t), n_TN_raw = sum(neg$score < t)
    )
  })
  do.call(rbind, rows)
}

weighted_auc <- function(sweep_df) {
  d <- sweep_df[order(sweep_df$weighted_FPR, sweep_df$weighted_TPR), ]
  d <- d[!is.na(d$weighted_FPR) & !is.na(d$weighted_TPR), ]
  if (nrow(d) < 2) return(NA_real_)
  sum(diff(d$weighted_FPR) * (head(d$weighted_TPR, -1) + tail(d$weighted_TPR, -1)) / 2)
}

# ==============================================================================
# Plots. Base R graphics only - no new package dependency.
#   - threshold-metrics: one per (spec x scheme).
#   - roc-comparison: one per scheme, ALL specs overlaid.
# Both accept an optional version_suffix, appended via apply_version_suffix()
# to their own filename, same mechanism as every other output in this script.
# ==============================================================================
generate_threshold_metrics_plot <- function(sweep_df, label, output_dir, version_suffix = NULL) {
  path <- file.path(output_dir, sprintf("threshold_metrics_%s.png", sanitize_name(label)))
  path <- apply_version_suffix(path, version_suffix)
  grDevices::png(path, width = 1200, height = 800, res = 150)
  on.exit(grDevices::dev.off())
  d <- sweep_df[order(sweep_df$threshold), ]
  graphics::plot(d$threshold, d$weighted_precision, type = "l", col = "#c0392b", lwd = 2,
                 ylim = c(0, 1), xlab = "Candidate threshold", ylab = "Value",
                 main = sprintf("Precision / recall / F1 vs. threshold - %s", label))
  graphics::lines(d$threshold, d$weighted_recall, col = "#1b6ca8", lwd = 2)
  graphics::lines(d$threshold, d$weighted_F1, col = "#27ae60", lwd = 2)
  graphics::lines(d$threshold, d$external_recall_at_threshold, col = "#8e44ad", lwd = 2, lty = 2)
  graphics::legend("topright",
                   legend = c("Weighted precision", "Weighted recall (TPR)", "Weighted F1", "External recall (current positive set)"),
                   col = c("#c0392b", "#1b6ca8", "#27ae60", "#8e44ad"), lwd = 2, lty = c(1, 1, 1, 2), bty = "o", bg = "white", cex = 0.8)
  log_msg(sprintf("Wrote threshold-metrics plot: %s", path))
}

generate_roc_comparison_plot <- function(sweep_list, auc_list, scheme, output_dir, version_suffix = NULL) {
  path <- file.path(output_dir, sprintf("roc_comparison_%s.png", sanitize_name(scheme)))
  path <- apply_version_suffix(path, version_suffix)
  grDevices::png(path, width = 1000, height = 1000, res = 150)
  on.exit(grDevices::dev.off())
  graphics::plot(NA, xlim = c(0, 1), ylim = c(0, 1), asp = 1,
                 xlab = "Weighted FPR (1 - specificity)", ylab = "Weighted TPR (recall)",
                 main = sprintf("ROC comparison across score specs - '%s' scheme", scheme))
  graphics::abline(0, 1, lty = 2, col = "grey60")
  palette <- c("#1b6ca8", "#c0392b", "#27ae60", "#8e44ad", "#e67e22", "#16a085")
  specs <- names(sweep_list)
  colors <- setNames(palette[(seq_along(specs) - 1) %% length(palette) + 1], specs)
  for (spec_name in specs) {
    d <- sweep_list[[spec_name]]
    d <- d[order(d$weighted_FPR, d$weighted_TPR), ]
    graphics::lines(d$weighted_FPR, d$weighted_TPR, col = colors[[spec_name]], lwd = 2)
  }
  graphics::legend("bottomright",
                   legend = sprintf("%s (AUC = %.3f)", specs, unlist(auc_list[specs])),
                   col = colors[specs], lwd = 2, bty = "n")
  log_msg(sprintf("Wrote ROC comparison plot: %s", path))
}

# ==============================================================================
# Main
# ==============================================================================
main_threshold_analysis <- function(config) {
  .log_con <<- setup_log_file(config)
  on.exit(if (!is.null(.log_con)) close(.log_con), add = TRUE)
  log_msg(sprintf("=== run_threshold_analysis.R run started: %s ===", Sys.time()))
  
  band_info <- recompute_bands_and_population(config)
  verify_against_manifest(config, band_info)
  
  labeled_base <- load_labeled_sample_base(config, band_info$population)
  validation_raw <- load_validation_raw(config)
  alt_scores_cache <- new.env(parent = emptyenv())   # per-run cache for any spec-level scores_path/validation_scores_path overrides - see attach_score_for_spec()
  
  if (!dir.exists(config$output_dir)) dir.create(config$output_dir, recursive = TRUE)
  
  # Output columns (main_labeled_with_all_scores.csv), one row per
  # sampled vacancy:
  #   review_id / vacancy_number  - identifiers (review_id is the blinded ID
  #                                used during labelling; vacancy_number is
  #                                unblinded here since this file is for
  #                                analysis, not review)
  #   band                          - which sampling band this vacancy was
  #                                  drawn into (see draw_negative_validation_sample.R)
  #   position_category               - HR-derived job category
  #   is_reliability_check              - TRUE if this row was part of the
  #                                      second-pass reliability recheck
  #   label                                - the No-DS/Partial DS/DS label used
  #                                        (per CONFIG$label_source)
  #   label_pass1 / label_source             - only when label_source = "final":
  #                                          the original pass-1 label, and how
  #                                          the final label was resolved (see
  #                                          build_final_labels())
  #   weight                                 - this row's inverse-probability
  #                                           weight (population in its band
  #                                           / number drawn from that band)
  #   score_<spec name> (one column per         - the requested score, from
  #   CONFIG$score_specs entry)                    that spec's granularity/
  #                                                score_column, for this
  #                                                vacancy
  wide <- labeled_base[, c("review_id", "vacancy_number", "band", "position_category", "is_reliability_check", "label", intersect(c("label_pass1", "label_source"), names(labeled_base)), "weight")]
  for (spec in config$score_specs) {
    sc <- attach_score_for_spec(labeled_base, spec, band_info$raw_scores, alt_scores_cache)[, c("vacancy_number", "score")]
    names(sc)[2] <- paste0("score_", spec$name)
    wide <- merge(wide, sc, by = "vacancy_number", all.x = TRUE)
  }
  wide_path <- apply_version_suffix(file.path(config$output_dir, "main_labeled_with_all_scores.csv"), config$version_suffix)
  utils::write.csv(wide, wide_path, row.names = FALSE)
  log_msg(sprintf("Wrote wide labelled+scores export: %s", wide_path))
  
  all_sweeps <- list()
  auc_rows <- list()
  schemes <- c("exclude", "collapse")
  
  for (spec in config$score_specs) {
    labeled_spec <- attach_score_for_spec(labeled_base, spec, band_info$raw_scores, alt_scores_cache)
    external_spec <- attach_score_for_spec_external(validation_raw, spec, alt_scores_cache)
    
    for (scheme in schemes) {
      scheme_data <- build_scheme_data(labeled_spec, scheme)
      n_pos <- sum(scheme_data$class == "positive")
      n_neg <- sum(scheme_data$class == "negative")
      combo_label <- sprintf("%s_%s", spec$name, scheme)
      log_msg(sprintf("--- spec '%s', scheme '%s': %d positive-labelled, %d negative-labelled rows ---", spec$name, scheme, n_pos, n_neg))
      if (n_pos == 0 || n_neg == 0) {
        log_warn(sprintf("Skipping '%s' - zero rows in one class after filtering (spec's missing-score drops may be the cause).", combo_label))
        next
      }
      
      sweep <- weighted_confusion_sweep(scheme_data, external_spec)
      auc <- weighted_auc(sweep)
      sweep$score_spec <- spec$name
      sweep$partial_scheme <- scheme
      all_sweeps[[combo_label]] <- sweep
      # Output columns (auc_summary.csv), one row per (score_spec, partial_scheme):
      #   score_spec / partial_scheme  - which score definition and Partial-DS
      #                                 handling scheme
      #   weighted_AUC                   - area under the weighted ROC curve for
      #                                    this combination
      #   n_positive / n_negative          - how many main-corpus sample rows had a
      #                                     usable score and fell in each class
      #                                     for this spec/scheme (this can vary
      #                                     across specs due to per-spec missing-
      #                                     score drops - see attach_score_for_spec())
      auc_rows[[combo_label]] <- data.frame(score_spec = spec$name, partial_scheme = scheme, weighted_AUC = auc, n_positive = n_pos, n_negative = n_neg)
      log_msg(sprintf("'%s': weighted AUC = %.4f", combo_label, auc))
      
      generate_threshold_metrics_plot(sweep, combo_label, config$output_dir, config$version_suffix)
    }
  }
  
  if (length(all_sweeps) == 0) stop("No spec/scheme combination produced usable results - check score_specs and the snapshot data.")
  
  aggregated <- do.call(rbind, all_sweeps)
  rownames(aggregated) <- NULL
  aggregated <- aggregated[, c("score_spec", "partial_scheme", setdiff(names(aggregated), c("score_spec", "partial_scheme")))]
  sweep_path <- apply_version_suffix(file.path(config$output_dir, "threshold_sweep_all.csv"), config$version_suffix)
  utils::write.csv(aggregated, sweep_path, row.names = FALSE)
  log_msg(sprintf("Wrote aggregated sweep table (%d rows, %d spec x scheme combinations): %s", nrow(aggregated), length(all_sweeps), sweep_path))
  
  auc_summary <- do.call(rbind, auc_rows)
  rownames(auc_summary) <- NULL
  auc_summary_path <- apply_version_suffix(file.path(config$output_dir, "auc_summary.csv"), config$version_suffix)
  utils::write.csv(auc_summary, auc_summary_path, row.names = FALSE)
  log_msg(sprintf("Wrote AUC summary: %s", auc_summary_path))
  print(auc_summary)
  
  for (scheme in schemes) {
    scheme_sweeps <- all_sweeps[grepl(paste0("_", scheme, "$"), names(all_sweeps))]
    names(scheme_sweeps) <- sub(paste0("_", scheme, "$"), "", names(scheme_sweeps))
    scheme_aucs <- setNames(auc_summary$weighted_AUC[auc_summary$partial_scheme == scheme], auc_summary$score_spec[auc_summary$partial_scheme == scheme])
    if (length(scheme_sweeps) >= 2) {
      generate_roc_comparison_plot(scheme_sweeps, as.list(scheme_aucs), scheme, config$output_dir, config$version_suffix)
    } else if (length(scheme_sweeps) == 1) {
      log_msg(sprintf("Only one usable score_spec for scheme '%s' - skipping the comparison plot (nothing to compare).", scheme))
    }
  }
  
  log_msg("=== Done. ===")
  invisible(list(band_info = band_info, aggregated = aggregated, auc_summary = auc_summary))
}

if (sys.nframe() == 0) {
  main_threshold_analysis(CONFIG)
}