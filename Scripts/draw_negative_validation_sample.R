#!/usr/bin/env Rscript
# ==============================================================================
# draw_negative_validation_sample.R
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
#   Draw a reproducible, stratified sample of vacancies from the main study
#   corpus for MANUAL negative-validation labelling (No-DS / Partial DS / DS),
#   to complement a small set of externally-sourced positive validation jobs.
#   Rationale:
#     - The positive validation set had to sit OUTSIDE the corpus to avoid
#       ground-truth circularity (can't verify "genuinely DS" using the same
#       pipeline you're validating).
#     - That concern barely applies to NEGATIVE labelling ("is this Lab
#       Technician posting data stewardship?" doesn't need external
#       adjudication) - so this script draws negatives FROM the main corpus,
#       but deliberately oversamples the score band near the known-positive
#       floor, since that's where a threshold decision actually lives or
#       dies. A flat random sample would mostly hit the bulk band, which is
#       already expected to be almost entirely negative and teaches you
#       little.
#
# Data scope (what this repository does and does not include):
#   This repository contains CODE ONLY. It does not include job-ad PDFs, the
#   master template, any vacancy_status.csv/similarity_scores.csv output, or
#   any drawn sample/snapshot folder. To run this script yourself, first
#   produce vacancy_status.csv and similarity_scores.csv by running
#   extract_job_criteria.R then similarity_scoring.R against your own
#   corpus and master template.
#
# Relationship to other scripts:
#   Pure CONSUMER of Script 1 (extract_job_criteria.R) and Script 2
#   (similarity_scoring.R) output - never touches PDFs, TXTs/, or triggers
#   any extraction/scoring. Reuses merged-file parsing logic identical to
#   similarity_scoring.R's parse_merged_job_text()/merged_file_path(), kept
#   as verbatim local copies (not sourced) so this script stays standalone,
#   matching the project's existing convention (see analyze_item_coverage.R's
#   own header comment for the same rationale).
#
# Reproducibility:
#   The live pipeline files (vacancy_status.csv, similarity_scores.csv,
#   master_template.txt, validation_similarity_scores.csv) will keep changing
#   as more vacancies are processed, partial/failed rows get fixed, and the
#   master template gets tuned. This script's FIRST action is to snapshot
#   frozen copies of everything the draw depends on into a dated,
#   never-touched-again folder, and hash them - so the sample is always
#   reproducible against the exact state it was drawn from, independent of
#   whatever the live files look like later. See snapshot_inputs() and
#   write_manifest().
#
# Objectivity / review design:
#   - The blind review file NEVER contains the similarity score, the band,
#     or vacancy_number - only an opaque review_id, so a reviewer (including
#     a SOLO reviewer) can't trivially cross-reference back to the score
#     without deliberately opening a separately-named crosswalk file.
#   - Row order in the blind file is shuffled (not grouped by band), so
#     position in the file doesn't leak band membership either.
#   - A reliability subsample is flagged for a second pass (self or
#     colleague) - see the companion script export_reliability_recheck.R,
#     which sources this file to reuse its helper functions, mirroring how
#     run_validation_batch.R sources extract_job_criteria.R.
#
# Usage:
#   Rscript draw_negative_validation_sample.R
#   Or edit CONFIG below and run interactively / source() then
#   main_draw_negative_sample(CONFIG).
#
# Requirements:
#   install.packages("digest")   # base R covers everything else used here
# ==============================================================================

suppressPackageStartupMessages({
  library(digest)
})

# ------------------------------------------------------------------------------
# CONFIG
# ------------------------------------------------------------------------------
CONFIG <- list(
  # --- Live pipeline inputs (read once, then frozen into the snapshot) -------
  status_table_path      = file.path("..", "Output", "vacancy_status.csv"),
  scores_path             = file.path("..", "Output", "similarity_scores.csv"),
  master_template_path     = file.path("..", "Data", "master_template.txt"),
  merged_dir                = file.path("..", "TXTs_merged"),
  validation_scores_path     = file.path("..", "Validation_Output", "validation_similarity_scores.csv"),

  # --- Snapshot / output location ---------------------------------------------
  snapshot_root = file.path("..", "Output", "sampling_snapshots"),
  sample_label  = "hard_negative_sample",   # used in the snapshot folder name and output filenames

  # --- Reproducibility -----------------------------------------------------------
  # Fixed and recorded in manifest.txt. Change deliberately (and re-run the
  # whole draw) if you ever need a different sample - never edit results by
  # hand instead.
  random_seed = 20260811L,

  # --- Which granularity's score_overall defines the bands --------------------
  # "document" chosen as the primary stratifier: natural 1:1 join to
  # vacancy_number, and empirically the second-best-separating granularity
  # after section-level (a substantially smaller proportion of the corpus
  # overlaps the validation floor at section level than at document level,
  # and a much larger proportion overlaps at item level - item-level scores
  # are noisier here because they average over many small bullet-vs-bullet
  # comparisons rather than one pooled comparison). Section-level score is
  # still recorded per sampled row for a secondary robustness check later.
  granularity_for_bands = "document",

  # --- Eligibility filters -----------------------------------------------------
  eligible_statuses = c("complete", "partial"),
  # Excludes the pending-grouping-review vacancies - their grouping
  # correctness is a separate open question from negative/positive labelling
  # and shouldn't be tangled into this exercise.
  exclude_flagged_for_review = TRUE,

  # --- Band 1 floor: anchored to the current validation positives, not a
  # round number. Falls back to a manually-set value (logged loudly) if the
  # validation scores file is missing/empty at the configured granularity -
  # e.g. before any validation run has happened yet. -------------------------
  fallback_band1_floor = 0.708,

  # --- Sample allocation --------------------------------------------------------
  # Deliberately disproportionate: dense sampling where the decision is
  # actually uncertain (bands 1-2, near the positive-validation floor),
  # thin sampling where the outcome is already predictable (bands 3-4, well
  # below it). This means any pooled "% of corpus" estimate MUST be
  # inverse-probability-weighted by band, never averaged naively. That
  # reweighting is left to the analysis step (not this script) once labels
  # exist, but band sizes are recorded in manifest.txt precisely so that
  # weighting is possible later.
  band_targets = c(band_1 = 20L, band_2 = 15L, band_3 = 15L, band_4 = 10L),

  # No single position_category may exceed this fraction of any band's
  # drawn sample - a soft cap (redraw within the band, not a hard grid), so
  # a thin category (e.g. Library) never forces empty cells.
  category_cap_fraction = 0.6,

  # --- Reliability subsample ---------------------------------------------------
  # A designated subset of the same drawn sample, spread across all bands,
  # flagged for a second independent pass (a real second rater if a
  # colleague is available; a self-recheck after some days, reshuffled, if
  # working solo). NOT extra items - a subset of the band_targets totals
  # above. Allocated roughly proportional to band size, minimum 2 per band
  # so every band gets SOME reliability check.
  n_reliability_check = 18L,

  log_to_file = TRUE,
  log_dir     = file.path("..", "Output", "logs")
)

# ==============================================================================
# Logging (identical harness to the other scripts in this pipeline, duplicated
# to keep this script standalone - same convention as similarity_scoring.R /
# analyze_item_coverage.R)
# ==============================================================================
.log_con <- NULL
log_msg <- function(text) {
  message(text)
  if (!is.null(.log_con)) cat(text, "\n", file = .log_con, sep = "")
}
log_warn <- function(text) log_msg(paste("WARNING:", text))
`%||%` <- function(a, b) if (is.null(a)) b else a

setup_log_file <- function(config) {
  if (!isTRUE(config$log_to_file)) return(NULL)
  if (!dir.exists(config$log_dir)) dir.create(config$log_dir, recursive = TRUE)
  log_path <- file.path(config$log_dir, sprintf("negative_sample_run_log_%s.txt", format(Sys.time(), "%Y%m%d_%H%M%S")))
  con <- file(log_path, open = "at")
  message(sprintf("Logging this run to: %s", log_path))
  con
}

# ==============================================================================
# Merged-file parsing - VERBATIM copies of similarity_scoring.R's
# parse_merged_job_text()/merged_file_path(), kept in sync by hand (not
# sourced) so this script stays standalone. If you ever change the merged
# .txt format in Script 1, update both copies.
# ==============================================================================
parse_merged_job_text <- function(path) {
  empty <- list(responsibilities = character(0), skills = character(0))
  if (!file.exists(path)) return(empty)
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")

  resp_start  <- which(lines == "RESPONSIBILITIES")
  skill_start <- which(grepl("^SKILLS", lines))
  if (length(resp_start) == 0 || length(skill_start) == 0) return(empty)

  resp_lines  <- lines[(resp_start[1] + 2):(skill_start[1] - 1)]
  skill_lines <- lines[(skill_start[1] + 2):length(lines)]

  parse_block <- function(block) {
    if (length(block) == 0) return(character(0))
    block <- block[nzchar(trimws(block))]
    block <- block[block != "(none extracted)"]
    sub("^-\\s*", "", block)
  }

  list(responsibilities = parse_block(resp_lines), skills = parse_block(skill_lines))
}

merged_file_path <- function(vacancy_number, config) {
  file.path(config$merged_dir, paste0("vacancy_", vacancy_number, "_MERGED.txt"))
}

# ==============================================================================
# Step 1: Snapshot the live inputs into a frozen, hashed, dated folder.
# This is the FIRST thing main_draw_negative_sample() does - the draw itself always reads back
# from these frozen copies, never from the live paths again, so the snapshot
# folder's existence is part of the reproducible run, not a manual step
# someone can forget.
# ==============================================================================
snapshot_inputs <- function(config) {
  stamp <- format(Sys.time(), "%Y-%m-%d")
  snap_dir <- file.path(config$snapshot_root, sprintf("%s_%s", config$sample_label, stamp))
  if (dir.exists(snap_dir)) {
    log_warn(sprintf(
      "Snapshot folder already exists for today (%s) - reusing it rather than overwriting. Delete it first if you deliberately want a fresh draw for the same date.",
      snap_dir
    ))
  } else {
    dir.create(snap_dir, recursive = TRUE)
  }

  copy_one <- function(src, dest_name, required = TRUE) {
    dest <- file.path(snap_dir, dest_name)
    if (!file.exists(src)) {
      msg <- sprintf("Input not found: %s", src)
      if (required) stop(msg) else { log_warn(msg); return(NA_character_) }
    }
    file.copy(src, dest, overwrite = TRUE)
    dest
  }

  paths <- list(
    status    = copy_one(config$status_table_path, "vacancy_status_snapshot.csv"),
    scores    = copy_one(config$scores_path, "similarity_scores_snapshot.csv"),
    master    = copy_one(config$master_template_path, sprintf("master_template_v1_%s.txt", stamp)),
    valscores = copy_one(config$validation_scores_path, "validation_similarity_scores_snapshot.csv", required = FALSE)
  )

  log_msg(sprintf("Snapshotted inputs into: %s", snap_dir))
  list(dir = snap_dir, stamp = stamp, paths = paths)
}

hash_file <- function(path) {
  if (is.na(path) || !file.exists(path)) return(NA_character_)
  digest::digest(file = path, algo = "sha256")
}

# ==============================================================================
# Step 2: Load the frozen snapshot copies (never the live files) and join.
# ==============================================================================
load_snapshotted_data <- function(snap) {
  status <- utils::read.csv(snap$paths$status, stringsAsFactors = FALSE, colClasses = "character", na.strings = character(0))
  scores <- utils::read.csv(snap$paths$scores, stringsAsFactors = FALSE, na.strings = "NA")
  list(status = status, scores = scores)
}

load_validation_floor <- function(snap, config) {
  if (is.na(snap$paths$valscores) || !file.exists(snap$paths$valscores)) {
    log_warn(sprintf(
      "No validation scores file available - falling back to CONFIG$fallback_band1_floor = %.3f. Confirm this is intentional (e.g. before any validation run has happened).",
      config$fallback_band1_floor
    ))
    return(config$fallback_band1_floor)
  }
  val <- utils::read.csv(snap$paths$valscores, stringsAsFactors = FALSE, na.strings = "NA")
  val_g <- val[val$granularity == config$granularity_for_bands & !is.na(val$score_overall), ]
  if (nrow(val_g) == 0) {
    log_warn(sprintf(
      "Validation scores file has no '%s'-granularity rows - falling back to CONFIG$fallback_band1_floor = %.3f.",
      config$granularity_for_bands, config$fallback_band1_floor
    ))
    return(config$fallback_band1_floor)
  }
  floor_val <- min(val_g$score_overall)
  log_msg(sprintf(
    "Band 1 floor derived from %d current validation-positive score(s) at '%s' granularity: %.4f",
    nrow(val_g), config$granularity_for_bands, floor_val
  ))
  floor_val
}

# ==============================================================================
# Step 3: Build the eligible population and assign bands.
# ==============================================================================
build_eligible_population <- function(data, band1_floor, config) {
  status <- data$status
  scores <- data$scores

  keep_status <- status$status %in% config$eligible_statuses
  flagged <- toupper(trimws(status$flagged_for_review %||% "")) == "TRUE"
  keep_flag <- if (isTRUE(config$exclude_flagged_for_review)) !flagged else rep(TRUE, nrow(status))

  elig_status <- status[keep_status & keep_flag, , drop = FALSE]
  log_msg(sprintf(
    "Eligibility filter: %d of %d vacancy_status.csv rows kept (status in {%s}%s).",
    nrow(elig_status), nrow(status), paste(config$eligible_statuses, collapse = ", "),
    if (isTRUE(config$exclude_flagged_for_review)) ", flagged_for_review == FALSE" else ""
  ))

  sc <- scores[scores$granularity == config$granularity_for_bands, , drop = FALSE]
  sc <- sc[!is.na(sc$score_overall), , drop = FALSE]
  sc$vacancy_number <- as.character(sc$vacancy_number)

  merged <- merge(
    elig_status[, c("vacancy_number", "position_category", "position_title")],
    sc[, c("vacancy_number", "score_overall")],
    by = "vacancy_number", all.x = FALSE, all.y = FALSE
  )
  # Dedup safety: should be one document-level score per vacancy_number, but
  # guard against an unexpected duplicate row rather than silently double-
  # counting a vacancy in the draw.
  if (any(duplicated(merged$vacancy_number))) {
    n_dup <- sum(duplicated(merged$vacancy_number))
    log_warn(sprintf(
      "%d vacancy_number(s) had more than one '%s'-granularity score row after the join - keeping the first occurrence only. Worth checking similarity_scores.csv for an unexpected duplicate.",
      n_dup, config$granularity_for_bands
    ))
    merged <- merged[!duplicated(merged$vacancy_number), , drop = FALSE]
  }

  log_msg(sprintf(
    "Eligible population with a current '%s'-granularity score: %d vacancies (of %d eligible by status/flag).",
    config$granularity_for_bands, nrow(merged), nrow(elig_status)
  ))

  q <- stats::quantile(merged$score_overall, probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE)
  band3_floor <- q[1]
  band2_floor <- q[2]

  merged$band <- with(merged, ifelse(
    score_overall >= band1_floor, "band_1", ifelse(
      score_overall >= band2_floor, "band_2", ifelse(
        score_overall >= band3_floor, "band_3", "band_4"
      ))))

  cutoffs <- c(band_1_floor = band1_floor, band_2_floor = band2_floor, band_3_floor = band3_floor)
  log_msg(sprintf(
    "Band cutoffs (score_overall, '%s' granularity): band_1 >= %.4f | band_2 [%.4f, %.4f) | band_3 [%.4f, %.4f) | band_4 < %.4f",
    config$granularity_for_bands, band1_floor, band2_floor, band1_floor, band3_floor, band2_floor, band3_floor
  ))
  print(table(merged$band))

  list(population = merged, cutoffs = cutoffs)
}

# ==============================================================================
# Step 4: Seeded, category-capped stratified draw within each band.
# ==============================================================================
draw_band_sample <- function(band_pool, target_n, category_cap_fraction) {
  n_avail <- nrow(band_pool)
  if (n_avail == 0) return(band_pool[0, , drop = FALSE])
  if (n_avail <= target_n) {
    if (n_avail < target_n) {
      log_warn(sprintf(
        "Band has only %d eligible vacancy(ies), fewer than the target of %d - taking all of them.",
        n_avail, target_n
      ))
    }
    return(band_pool)
  }

  # Deterministic pre-sort (by vacancy_number) before shuffling, so the
  # shuffle result depends only on the seed, never on incoming row order.
  band_pool <- band_pool[order(band_pool$vacancy_number), , drop = FALSE]
  shuffled_idx <- sample(seq_len(n_avail))
  pool <- band_pool[shuffled_idx, , drop = FALSE]

  cap <- max(1L, ceiling(target_n * category_cap_fraction))
  cat_counts <- table(character(0))
  picked <- integer(0)

  for (i in seq_len(nrow(pool))) {
    cat_i <- pool$position_category[i]
    cat_i_key <- if (is.na(cat_i) || !nzchar(cat_i)) "(blank)" else cat_i
    current <- if (cat_i_key %in% names(cat_counts)) cat_counts[[cat_i_key]] else 0L
    if (current >= cap) next
    picked <- c(picked, i)
    cat_counts[cat_i_key] <- current + 1L
    if (length(picked) >= target_n) break
  }

  if (length(picked) < target_n) {
    log_warn(sprintf(
      "Category cap (%.0f%% of %d = %d per category) left this band %d short of its target after one pass - relaxing the cap and filling the remainder from whatever's left.",
      category_cap_fraction * 100, target_n, cap, target_n - length(picked)
    ))
    remaining_idx <- setdiff(seq_len(nrow(pool)), picked)
    n_fill <- min(length(remaining_idx), target_n - length(picked))
    if (n_fill > 0) picked <- c(picked, remaining_idx[seq_len(n_fill)])
  }

  pool[picked, , drop = FALSE]
}

draw_full_sample <- function(elig, config) {
  set.seed(config$random_seed)
  band_names <- c("band_1", "band_2", "band_3", "band_4")
  parts <- lapply(band_names, function(b) {
    target <- config$band_targets[[b]]
    pool <- elig$population[elig$population$band == b, , drop = FALSE]
    drawn <- draw_band_sample(pool, target, config$category_cap_fraction)
    log_msg(sprintf("%s: drew %d of target %d (category cap %.0f%%).", b, nrow(drawn), target, config$category_cap_fraction * 100))
    drawn
  })
  sample_df <- do.call(rbind, parts)
  sample_df <- sample_df[order(sample_df$band, sample_df$vacancy_number), , drop = FALSE]
  rownames(sample_df) <- NULL
  sample_df
}

# ==============================================================================
# Step 5: Flag a reliability subsample, spread across bands.
# ==============================================================================
flag_reliability_subsample <- function(sample_df, config) {
  sample_df$is_reliability_check <- FALSE
  band_names <- c("band_1", "band_2", "band_3", "band_4")
  n_bands <- length(band_names)

  # Every band gets at least a floor of 2 (if it has that many rows), then
  # the remainder from n_reliability_check is handed out one-at-a-time to
  # band_1 first, then band_2, etc. - bands 1-2 are where the decision
  # boundary actually lives, so if the total doesn't divide evenly across 4
  # bands, that's where the extra reliability checks should go.
  base_floor <- 2L
  quotas <- setNames(rep(base_floor, n_bands), band_names)
  remainder <- config$n_reliability_check - sum(quotas)
  i <- 1L
  while (remainder > 0 && i <= 1000L) {
    b <- band_names[((i - 1L) %% n_bands) + 1L]
    quotas[b] <- quotas[b] + 1L
    remainder <- remainder - 1L
    i <- i + 1L
  }

  for (b in band_names) {
    idx <- which(sample_df$band == b)
    if (length(idx) == 0) next
    n_take <- min(quotas[[b]], length(idx))
    if (n_take < quotas[[b]]) {
      log_warn(sprintf("%s has only %d row(s), fewer than its reliability quota of %d - taking all of them.", b, length(idx), quotas[[b]]))
    }
    take <- sample(idx, n_take)   # uses the same seeded RNG stream as the draw above - reproducible given random_seed
    sample_df$is_reliability_check[take] <- TRUE
  }

  n_flagged <- sum(sample_df$is_reliability_check)
  log_msg(sprintf("Reliability subsample: %d of %d rows flagged (target was %d).", n_flagged, nrow(sample_df), config$n_reliability_check))
  sample_df
}

# ==============================================================================
# Step 6: Pull merged job text for each sampled vacancy (content only - no
# score, no band - for the blind review file).
# ==============================================================================
format_job_text <- function(parsed) {
  resp <- if (length(parsed$responsibilities) == 0) "(none extracted)" else paste0("- ", parsed$responsibilities, collapse = "\n")
  skl  <- if (length(parsed$skills) == 0) "(none extracted)" else paste0("- ", parsed$skills, collapse = "\n")
  paste0("RESPONSIBILITIES\n", resp, "\n\nSKILLS\n", skl)
}

attach_job_text <- function(sample_df, config) {
  texts <- vapply(sample_df$vacancy_number, function(vn) {
    path <- merged_file_path(vn, config)
    if (!file.exists(path)) {
      log_warn(sprintf("Merged file not found for vacancy_number '%s' (expected at %s) - job_text will be blank for this row.", vn, path))
      return(NA_character_)
    }
    format_job_text(parse_merged_job_text(path))
  }, character(1))
  sample_df$job_text <- texts
  n_missing <- sum(is.na(texts))
  if (n_missing > 0) {
    log_warn(sprintf("%d of %d sampled vacancies had no readable merged file - these rows need manual attention before review (check merge_dir / vacancy_number formatting).", n_missing, nrow(sample_df)))
  }
  sample_df
}

# ==============================================================================
# Step 7: Write the crosswalk (key) and blind review files, plus the manifest.
# ==============================================================================
write_outputs <- function(sample_df, elig, snap, config) {
  n <- nrow(sample_df)
  sample_df$review_id <- sprintf("R%03d", seq_len(n))

  # --- Crosswalk: the ONLY file that ties review_id back to vacancy_number,
  # band, and score. Named so its purpose (don't open until review is done)
  # is obvious without needing to read this script. ---------------------------
  
  # Output columns (sample_key_CROSSWALK_do_not_open_until_review_complete.csv)
  # - the ONLY file tying review_id back to identifying/score information;
  # do not open until review is complete:
  #   review_id               - the blinded ID used in the review file(s)
  #   vacancy_number             - the real, unblinded job-ad identifier
  #   band                          - which sampling band this vacancy was
  #                                  drawn into
  #   score_at_draw_time              - this vacancy's document-level score
  #                                    at the moment of drawing (frozen; will
  #                                    drift from a live rescore later)
  #   position_category                 - HR-derived job category
  #   is_reliability_check                 - TRUE if flagged for the second,
  #                                        blind reliability-check pass
  crosswalk <- sample_df[, c("review_id", "vacancy_number", "band", "score_overall", "position_category", "is_reliability_check")]
  names(crosswalk)[names(crosswalk) == "score_overall"] <- "score_at_draw_time"
  crosswalk_path <- file.path(snap$dir, "sample_key_CROSSWALK_do_not_open_until_review_complete.csv")
  utils::write.csv(crosswalk, crosswalk_path, row.names = FALSE)

  # --- Blind review file: shuffled row order, no score/band/vacancy_number. --
  blind_order <- sample(seq_len(n))
  
  # Output columns (review_blind_pass1_<date>.csv) - the file a reviewer
  # actually labels; deliberately excludes score/band/vacancy_number:
  #   review_id                  - blinded ID; look this up in the crosswalk
  #                               only after review is complete
  #   position_title /              - the only identifying context given to
  #   position_category                the reviewer
  #   job_text                           - the merged RESPONSIBILITIES/SKILLS
  #                                      text for this vacancy
  #   label                                 - reviewer fills in: No-DS /
  #                                          Partial DS / DS
  #   confidence                              - reviewer fills in: High /
  #                                            Needs expert review
  #   justification                             - reviewer fills in: one line
  #                                              citing the rubric criterion
  #                                              that drove the call
  #   reviewer / date_reviewed                    - reviewer fills in: who
  #                                                and when
  #   reviewer_pass                                 - fixed at 1 for this file
  #                                                  (2 for the pass-2 file
  #                                                  from export_reliability_recheck.R)
  blind <- sample_df[blind_order, c("review_id", "position_title", "position_category", "job_text")]
  blind$label            <- ""   # No-DS / Partial DS / DS - filled in by the reviewer
  blind$confidence         <- "" # High / Needs expert review
  blind$justification       <- "" # one line citing the rubric criterion that drove the call
  blind$reviewer              <- ""
  blind$date_reviewed          <- ""
  blind$reviewer_pass            <- 1L
  blind_path <- file.path(snap$dir, sprintf("review_blind_pass1_%s.csv", snap$stamp))
  utils::write.csv(blind, blind_path, row.names = FALSE)

  log_msg(sprintf("Wrote crosswalk (%d rows): %s", nrow(crosswalk), crosswalk_path))
  log_msg(sprintf("Wrote blind review file, pass 1 (%d rows): %s", nrow(blind), blind_path))

  list(crosswalk_path = crosswalk_path, blind_path = blind_path)
}

write_manifest <- function(snap, elig, sample_df, band1_floor, config, out_paths) {
  hashes <- vapply(snap$paths, hash_file, character(1))
  band_counts <- table(sample_df$band)

  lines <- c(
    sprintf("=== Negative validation sample: manifest ==="),
    sprintf("Drawn: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    sprintf("random_seed: %d", config$random_seed),
    sprintf("granularity_for_bands: %s", config$granularity_for_bands),
    "",
    "--- Eligibility filters ---",
    sprintf("eligible_statuses: %s", paste(config$eligible_statuses, collapse = ", ")),
    sprintf("exclude_flagged_for_review: %s", config$exclude_flagged_for_review),
    sprintf("eligible population size (with current score): %d", nrow(elig$population)),
    "",
    "--- Band cutoffs (score_overall) ---",
    sprintf("band_1 floor (validation-derived, or fallback): %.4f", band1_floor),
    sprintf("band_2 floor (corpus 75th pct): %.4f", elig$cutoffs[["band_2_floor"]]),
    sprintf("band_3 floor (corpus 25th pct): %.4f", elig$cutoffs[["band_3_floor"]]),
    "",
    "--- Allocation ---",
    paste(sprintf("%s target=%d drawn=%d", names(config$band_targets), config$band_targets,
                   sapply(names(config$band_targets), function(b) band_counts[[b]] %||% 0L)),
          collapse = " | "),
    sprintf("category_cap_fraction: %.2f", config$category_cap_fraction),
    sprintf("n_reliability_check target: %d, actual: %d", config$n_reliability_check, sum(sample_df$is_reliability_check)),
    "",
    "--- Snapshotted input files (SHA-256) ---",
    sprintf("vacancy_status_snapshot.csv: %s", hashes[["status"]]),
    sprintf("similarity_scores_snapshot.csv: %s", hashes[["scores"]]),
    sprintf("master_template (v1): %s", hashes[["master"]]),
    sprintf("validation_similarity_scores_snapshot.csv: %s", hashes[["valscores"]]),
    "",
    "--- Environment ---",
    sprintf("R version: %s", R.version.string),
    sprintf("digest package version: %s", as.character(utils::packageVersion("digest"))),
    "",
    "--- Output files in this folder ---",
    sprintf("crosswalk: %s", basename(out_paths$crosswalk_path)),
    sprintf("blind review (pass 1): %s", basename(out_paths$blind_path)),
    "",
    "--- Reweighting note for analysis ---",
    "Band allocation above is DISPROPORTIONATE (dense near the decision",
    "boundary, thin in the bulk/floor bands). Any pooled 'proportion of the",
    "corpus that is genuinely DS' estimate must be inverse-probability-",
    "weighted by (band population size / band drawn size), not averaged",
    "naively across the labelled rows. Per-band proportions are directly",
    "usable as-is for threshold placement."
  )
  manifest_path <- file.path(snap$dir, "manifest.txt")
  writeLines(lines, manifest_path)
  log_msg(sprintf("Wrote manifest: %s", manifest_path))
  manifest_path
}

# ==============================================================================
# Main
# ==============================================================================
main_draw_negative_sample <- function(config) {
  .log_con <<- setup_log_file(config)
  on.exit(if (!is.null(.log_con)) close(.log_con), add = TRUE)

  log_msg(sprintf("=== draw_negative_validation_sample.R run started: %s ===", Sys.time()))

  snap <- snapshot_inputs(config)
  data <- load_snapshotted_data(snap)
  band1_floor <- load_validation_floor(snap, config)
  elig <- build_eligible_population(data, band1_floor, config)

  sample_df <- draw_full_sample(elig, config)
  sample_df <- flag_reliability_subsample(sample_df, config)
  sample_df <- attach_job_text(sample_df, config)

  out_paths <- write_outputs(sample_df, elig, snap, config)
  write_manifest(snap, elig, sample_df, band1_floor, config, out_paths)

  log_msg(sprintf(
    "Done. %d vacancies sampled across %d bands. Snapshot + outputs in: %s",
    nrow(sample_df), length(unique(sample_df$band)), snap$dir
  ))
  invisible(list(snap = snap, sample_df = sample_df))
}

if (sys.nframe() == 0) {
  main_draw_negative_sample(CONFIG)
}
