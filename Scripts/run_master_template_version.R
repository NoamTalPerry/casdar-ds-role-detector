#!/usr/bin/env Rscript
# ==============================================================================
# run_master_template_version.R  (general wrapper - NOT specific to any one
# ablation, e.g. the ADJACENT_ROLE toggle. Any master template variant -
# items dropped, items reworded, a whole new pool, or the ADJACENT_ROLE
# toggle - reuses this same script, just a different candidate_template_path
# and (if relevant) different sim_config_overrides.)
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
#   Scores BOTH the Cambridge sample and the external validation set under
#   a candidate master template, then (optionally) runs the full downstream
#   analysis chain - threshold sweep, item-level coverage by label,
#   triangulation - all landing in ONE dedicated, versioned output folder.
#   Nothing from this run touches Output/ or Validation_Output/'s existing
#   files, by construction (a distinct folder entirely), not just by
#   filename suffix.
#
#   Every CSV/PNG this run produces also gets CONFIG_VERSION$version_suffix
#   appended to its filename (defaulting to version_name - see that field's
#   comment below) via each downstream script's own apply_version_suffix()
#   mechanism. This is a COMPLEMENT to the versioned-folder design, not a
#   replacement for it: the folder is what keeps different versions from
#   ever touching each other's files, while the filename suffix is what
#   keeps individual files self-identifying once they're pulled OUT of
#   their folder and gathered together for a side-by-side comparison (e.g.
#   dropping threshold_sweep_all.csv from three different version runs into
#   one flat folder to diff them).
#
# Data scope (what this repository does and does not include):
#   This repository contains CODE ONLY. It does not include job-ad PDFs,
#   the master template, any scored output, or any sampling snapshot.
#   CONFIG_VERSION$cambridge_vacancy_numbers below is left empty
#   (character(0), meaning "score the whole corpus") rather than the real
#   subsample this project actually used for a given version run - fill it
#   in with your own vacancy_number subset, or leave it empty to score
#   everything.
#
# Why full chaining is safe: similarity_scoring.R, analyze_item_coverage.R,
# run_threshold_analysis.R, and triangulate_item_coverage.R each define a
# distinctly-named main function (main_similarity_scoring,
# main_analyze_item_coverage, main_threshold_analysis,
# main_triangulate_item_coverage) rather than a generic "main" - sourcing
# all of them into one environment (as this wrapper does) would otherwise
# let the last-sourced "main" silently win, with no error. The same fix is
# applied to resolve_paths_for_mode, which would otherwise collide between
# similarity_scoring.R and analyze_item_coverage.R with GENUINELY DIFFERENT
# implementations (the more dangerous kind of collision - silent
# wrong-logic, not just silent wrong-script) - each now has its own
# script-specific name (resolve_paths_for_mode_similarity_scoring(),
# resolve_paths_for_mode_item_coverage()).
#
# apply_version_suffix() follows the OPPOSITE rule deliberately: it is
# IDENTICAL across every script that defines it (similarity_scoring.R,
# run_threshold_analysis.R, triangulate_item_coverage.R,
# analyze_item_coverage.R), so - per the same collision-safety principle -
# letting the last-sourced copy "win" is fine, since every copy behaves the
# same way. The general rule: an identical duplicated implementation
# sharing a name across scripts sourced together is safe; a DIFFERING
# implementation under a shared name is the actual risk.
#
# Provenance: a per-row master_template_digest column in
# similarity_scoring.R's own output was deliberately not added there, since
# that would mean changing shared, load-bearing row-writing logic that
# every future run depends on. This wrapper's manifest is the provenance
# record instead - kept inside the SAME versioned folder as everything else
# it produced, not a separate location (see the manifest-writing code near
# the end of run_version() below).
#
# Usage:
#   Fill in CONFIG_VERSION below, then: Rscript run_master_template_version.R
# ==============================================================================

# Source every script this wrapper might chain into, snapshotting each
# one's own CONFIG under a distinct name immediately after sourcing it -
# every one of these scripts defines a top-level object literally called
# CONFIG, so capturing it before the NEXT source() call overwrites it is
# required, not just tidy. run_item_coverage_by_label.R is sourced LAST
# because it transitively sources analyze_item_coverage.R itself (and
# creates base_item_coverage_config as a side effect) - no need to source
# analyze_item_coverage.R separately, it comes along for free and this
# avoids yet another CONFIG-snapshot juggling step for the exact same file.
source("similarity_scoring.R")
base_sim_config <- CONFIG

source("run_threshold_analysis.R")
base_threshold_config <- CONFIG

source("triangulate_item_coverage.R")
base_triangulate_config <- CONFIG

source("run_item_coverage_by_label.R")   # brings in main_analyze_item_coverage(), run_wrapper(), and base_item_coverage_config as side effects

CONFIG_VERSION <- list(
  # Short, filename-safe label for this version - becomes the versioned
  # output folder's name. e.g. "v2_adjacent_role", "v3_dropped_items".
  version_name = "v2_example_variant",

  # Appended (via apply_version_suffix(), the same mechanism used
  # identically across every script in this chain) to every CSV/PNG this
  # run produces, so files pulled out of their version folder and compared
  # side by side (e.g. threshold_sweep_all.csv from two different versions,
  # dropped into one flat folder) stay distinguishable by filename alone -
  # a COMPLEMENT to, not a replacement for, this wrapper's folder-based
  # provenance record (see the header comment above). Hand-renaming
  # filenames alone, without this mechanism, is the risky part on its own -
  # a filename that gets manually retagged (or is forgotten and left stale)
  # can silently drift out of sync with which version it actually came
  # from. Defaults to version_name when left NULL (the common case) - set
  # independently only if you want several distinct suffixed runs to share
  # one version_name's folder (e.g. re-running threshold analysis alone
  # with different threshold_score_specs against the SAME already-scored
  # template).
  version_suffix = NULL,

  # The candidate template being tested this run.
  candidate_template_path = file.path("..", "Data", "master_template_v2.txt"),

  # Any similarity_scoring.R CONFIG fields to override for BOTH calls this
  # run - e.g. list(use_adjacent_pool = TRUE, apply_adjacent_pool_to_responsibilities = FALSE)
  # for an ADJACENT_ROLE-toggle version. Leave as list() for a version
  # whose difference from v1 is purely template CONTENT (items dropped/
  # reworded) rather than a scoring-behavior toggle - nothing else in this
  # script needs to know or care which kind of version this is.
  sim_config_overrides = list(),

  # Which Cambridge vacancy_numbers to score under the candidate template.
  # Leave empty (character(0)) to score the WHOLE main corpus - expensive,
  # only do this once a sample-scale version has already looked worthwhile.
  # For a smaller trial run, supply your own subset here, e.g.
  # c("EXAMPLE1", "EXAMPLE2", "EXAMPLE3").
  cambridge_vacancy_numbers = character(0),

  granularity_modes = c("document", "section", "item"),

  # Root under which every version gets its own dedicated subfolder -
  # nothing here ever touches the live Output/Validation_Output trees.
  versions_root = file.path("..", "Output", "master_template_versions"),

  # --- Downstream analysis chaining -------------------------------------------
  run_downstream_threshold_analysis = TRUE,
  run_downstream_item_coverage = TRUE,

  # For threshold analysis: the ORIGINAL historical draw snapshot (unchanged
  # by which template version is being tested - band weights depend only on
  # how the sample was actually drawn, never on what you score them with
  # afterward). Required if run_downstream_threshold_analysis is TRUE. Point
  # this at your own snapshot folder produced by
  # draw_negative_validation_sample.R.
  snapshot_dir = file.path("..", "Output", "sampling_snapshots",
                           "hard_negative_sample_EXAMPLE_DATE"),

  # Which score definition(s) to sweep for this version - same shape as
  # run_threshold_analysis.R's own score_specs, minus name/scores_path/
  # validation_scores_path (auto-filled to point at this version's own
  # output, and auto-named from granularity+score_column if not given).
  threshold_score_specs = list(
    list(granularity = "section", score_column = "score_skills")
  ),

  # For item coverage: which label(s) define each group - same shape as
  # run_item_coverage_by_label.R's own CONFIG_WRAPPER$groups. Needs exactly
  # 2 groups unless item_coverage_primary_positive_group/negative_group
  # (below) are also set explicitly.
  item_coverage_groups = list(
    cambridge_ds   = c("DS"),
    cambridge_partial = c("Partial DS"),
    cambridge_nods = c("No-DS")
  ),
  item_coverage_primary_positive_group = "cambridge_ds",   # auto-derived from item_coverage_groups if exactly 2 groups and this is left NULL
  item_coverage_primary_negative_group = "cambridge_nods",

  # Only needed if run_downstream_threshold_analysis is FALSE (otherwise
  # the wide export from that step is used automatically - see main
  # loop). Needs 'vacancy_number' and 'label' columns.
  labeled_data_path = NULL,

  log_to_file = TRUE,
  log_dir     = file.path("..", "Output", "logs")
)

# ==============================================================================
# Small helpers
# ==============================================================================

# Warns rather than blocks if the version folder already exists (e.g.
# re-running to pick up more vacancies) - same convention
# draw_negative_validation_sample.R already uses for its own snapshot-
# folder reuse.
prepare_version_dir <- function(version_dir) {
  if (dir.exists(version_dir)) {
    log_warn(sprintf(
      "Version folder already exists: %s - reusing it. Files from a previous run of the SAME version_name AND version_suffix will be overwritten by this run where filenames match; use a different version_suffix (or delete the folder first) if you want the previous run's files to survive alongside this one.",
      version_dir
    ))
  } else {
    dir.create(version_dir, recursive = TRUE)
  }
}

# Derives the primary positive/negative group names for triangulation's
# discrimination_gap, from CONFIG_VERSION$item_coverage_groups if not set
# explicitly - only unambiguous when there are exactly 2 groups.
resolve_primary_groups <- function(config) {
  if (!is.null(config$item_coverage_primary_positive_group) && !is.null(config$item_coverage_primary_negative_group)) {
    return(list(positive = config$item_coverage_primary_positive_group, negative = config$item_coverage_primary_negative_group))
  }
  group_names <- names(config$item_coverage_groups)
  if (length(group_names) != 2) {
    stop(sprintf(
      "item_coverage_groups has %d group(s) - item_coverage_primary_positive_group/negative_group must be set explicitly when it isn't exactly 2 (ambiguous which pair defines the discrimination gap otherwise).",
      length(group_names)
    ))
  }
  list(positive = group_names[1], negative = group_names[2])
}

# ==============================================================================
# Main
# ==============================================================================
run_version <- function(config) {
  wrapper_log_con <- setup_log_file(config)
  .log_con <<- wrapper_log_con
  on.exit(if (!is.null(wrapper_log_con)) close(wrapper_log_con), add = TRUE)

  log_msg(sprintf("=== run_master_template_version.R wrapper started: %s ===", Sys.time()))
  log_msg(sprintf("Version: %s", config$version_name))

  if (!nzchar(trimws(config$version_name %||% ""))) stop("CONFIG_VERSION$version_name is required and cannot be empty.")
  if (!file.exists(config$candidate_template_path)) stop(sprintf("candidate_template_path not found: %s", config$candidate_template_path))
  if (isTRUE(config$run_downstream_threshold_analysis) && is.null(config$snapshot_dir)) {
    stop("snapshot_dir is required when run_downstream_threshold_analysis is TRUE.")
  }

  if (!nzchar(trimws(config$version_suffix %||% ""))) {
    config$version_suffix <- config$version_name
    log_msg(sprintf("CONFIG_VERSION$version_suffix not set - defaulting to version_name ('%s').", config$version_suffix))
  } else {
    log_msg(sprintf("version_suffix: %s", config$version_suffix))
  }

  version_dir <- file.path(config$versions_root, config$version_name)
  prepare_version_dir(version_dir)

  # --- Snapshot + hash the candidate template - the provenance record ------
  template_snapshot_path <- file.path(version_dir, "master_template_snapshot.txt")
  file.copy(config$candidate_template_path, template_snapshot_path, overwrite = TRUE)
  template_hash <- digest::digest(file = config$candidate_template_path, algo = "sha256")
  log_msg(sprintf("Snapshotted candidate template to %s (SHA-256: %s)", template_snapshot_path, template_hash))

  # Base (unsuffixed) filenames - similarity_scoring.R appends
  # CONFIG$version_suffix itself, inside resolve_paths_for_mode_similarity_scoring(),
  # the same way every other script in this chain suffixes its own output.
  # The RESOLVED (suffixed) equivalents below are computed here purely for
  # THIS wrapper's own downstream references (threshold analysis specs,
  # item-coverage calls) - never re-assigned back onto cam_config/
  # val_config, so the suffix is never applied twice.
  cambridge_scores_base        <- file.path(version_dir, "similarity_scores.csv")
  cambridge_item_matches_base  <- file.path(version_dir, "similarity_item_matches.csv")
  validation_scores_base       <- file.path(version_dir, "validation_similarity_scores.csv")
  validation_item_matches_base <- file.path(version_dir, "validation_similarity_item_matches.csv")

  cambridge_scores_path        <- apply_version_suffix(cambridge_scores_base, config$version_suffix)
  cambridge_item_matches_path  <- apply_version_suffix(cambridge_item_matches_base, config$version_suffix)
  validation_scores_path       <- apply_version_suffix(validation_scores_base, config$version_suffix)
  validation_item_matches_path <- apply_version_suffix(validation_item_matches_base, config$version_suffix)

  # --- Step 1: Cambridge-side scoring ------------------------------------------
  log_msg("--- Scoring Cambridge sample under the candidate template ---")
  cam_config <- base_sim_config
  cam_config$validation_mode <- FALSE
  cam_config$master_template_path <- config$candidate_template_path
  for (nm in names(config$sim_config_overrides)) cam_config[[nm]] <- config$sim_config_overrides[[nm]]
  cam_config$granularity_modes <- config$granularity_modes
  cam_config$only_vacancy_numbers <- config$cambridge_vacancy_numbers
  cam_config$scores_path <- cambridge_scores_base
  cam_config$item_matches_path <- cambridge_item_matches_base
  cam_config$version_suffix <- config$version_suffix
  cam_config$log_to_file <- FALSE
  main_similarity_scoring(cam_config)
  .log_con <<- wrapper_log_con   # similarity_scoring.R's main_similarity_scoring() unconditionally reassigns .log_con on every call

  # --- Step 2: external validation-side scoring --------------------------------
  log_msg("--- Scoring external validation set under the candidate template ---")
  val_config <- base_sim_config
  val_config$validation_mode <- TRUE
  val_config$master_template_path <- config$candidate_template_path
  for (nm in names(config$sim_config_overrides)) val_config[[nm]] <- config$sim_config_overrides[[nm]]
  val_config$granularity_modes <- config$granularity_modes
  val_config$validation_scores_path <- validation_scores_base
  val_config$validation_item_matches_path <- validation_item_matches_base
  val_config$version_suffix <- config$version_suffix
  val_config$log_to_file <- FALSE
  main_similarity_scoring(val_config)
  .log_con <<- wrapper_log_con

  labeled_data_path <- config$labeled_data_path
  threshold_output_dir <- NULL

  # --- Step 3 (optional): threshold analysis -----------------------------------
  if (isTRUE(config$run_downstream_threshold_analysis)) {
    log_msg("--- Running threshold analysis for this version ---")
    threshold_output_dir <- file.path(version_dir, "threshold_analysis")
    threshold_config <- base_threshold_config
    threshold_config$snapshot_dir <- config$snapshot_dir
    threshold_config$current_validation_scores_path <- validation_scores_path
    threshold_config$output_dir <- threshold_output_dir
    threshold_config$score_specs <- lapply(config$threshold_score_specs, function(spec) {
      spec_name <- if (!is.null(spec$name)) spec$name else paste0(spec$granularity, "_", spec$score_column)
      list(name = spec_name, granularity = spec$granularity, score_column = spec$score_column,
           scores_path = cambridge_scores_path, validation_scores_path = validation_scores_path)
    })
    threshold_config$version_suffix <- config$version_suffix
    threshold_config$log_to_file <- FALSE
    main_threshold_analysis(threshold_config)
    .log_con <<- wrapper_log_con
    labeled_data_path <- apply_version_suffix(file.path(threshold_output_dir, "cambridge_labeled_with_all_scores.csv"), config$version_suffix)
  }

  # --- Step 4 (optional): item-level coverage, both sides, then triangulate ---
  if (isTRUE(config$run_downstream_item_coverage)) {
    if (is.null(labeled_data_path)) {
      stop("run_downstream_item_coverage is TRUE but no labeled_data_path is available - either enable run_downstream_threshold_analysis (its wide export is used automatically), or set CONFIG_VERSION$labeled_data_path directly.")
    }
    item_cov_dir <- file.path(version_dir, "item_coverage")

    log_msg("--- Running item coverage by label (Cambridge side) ---")
    item_cov_config <- list(
      labeled_data_path = labeled_data_path,
      groups = config$item_coverage_groups,
      filter_output_dir = file.path(item_cov_dir, "groups"),
      item_matches_path = cambridge_item_matches_path,
      summary_output_dir = item_cov_dir,
      match_rate_thresholds = base_item_coverage_config$match_rate_thresholds,
      make_plots = TRUE,
      version_suffix = config$version_suffix,
      log_to_file = FALSE,
      log_dir = file.path(version_dir, "logs")
    )
    group_paths <- run_wrapper(item_cov_config)
    .log_con <<- wrapper_log_con

    log_msg("--- Running item coverage for the external validation set ---")
    val_cov_summary_base <- file.path(item_cov_dir, "validation_item_coverage_summary.csv")
    val_cov_plot_base     <- file.path(item_cov_dir, "validation_item_coverage_plot.png")
    val_cov_config <- base_item_coverage_config
    val_cov_config$validation_mode <- FALSE   # bypass its automatic path-switching entirely - explicit paths below make it unnecessary
    val_cov_config$item_matches_path <- validation_item_matches_path
    val_cov_config$summary_output_path <- val_cov_summary_base
    val_cov_config$plot_output_path <- val_cov_plot_base
    val_cov_config$version_suffix <- config$version_suffix
    val_cov_config$log_to_file <- FALSE
    main_analyze_item_coverage(val_cov_config)
    .log_con <<- wrapper_log_con
    val_cov_summary_path <- apply_version_suffix(val_cov_summary_base, config$version_suffix)   # matches what analyze_item_coverage.R actually wrote

    log_msg("--- Triangulating coverage across groups ---")
    primary <- resolve_primary_groups(config)
    triangulate_config <- base_triangulate_config
    triangulate_config$coverage_inputs <- c(group_paths, list(validation_positive = val_cov_summary_path))
    triangulate_config$primary_positive_group <- primary$positive
    triangulate_config$primary_negative_group <- primary$negative
    triangulate_config$output_path <- file.path(item_cov_dir, "item_coverage_triangulated.csv")
    triangulate_config$plot_output_path <- file.path(item_cov_dir, "item_coverage_triangulated_plot.png")
    triangulate_config$version_suffix <- config$version_suffix
    triangulate_config$log_to_file <- FALSE
    main_triangulate_item_coverage(triangulate_config)
    .log_con <<- wrapper_log_con
  }

  # --- Manifest -----------------------------------------------------------------
  manifest_path <- file.path(version_dir, "manifest.txt")
  lines <- c(
    sprintf("=== Master template version: %s ===", config$version_name),
    sprintf("version_suffix (appended to every CSV/PNG filename this run produced): %s", config$version_suffix),
    sprintf("Run at: %s", Sys.time()),
    "",
    sprintf("Candidate template: %s", config$candidate_template_path),
    sprintf("Template SHA-256: %s", template_hash),
    sprintf("Template snapshot copy: %s", template_snapshot_path),
    sprintf("sim_config_overrides: %s", if (length(config$sim_config_overrides) == 0) "(none - template-content-only version)" else paste(sprintf("%s=%s", names(config$sim_config_overrides), unlist(config$sim_config_overrides)), collapse = ", ")),
    sprintf("granularity_modes: %s", paste(config$granularity_modes, collapse = ", ")),
    sprintf("Cambridge vacancy_numbers requested: %s", if (length(config$cambridge_vacancy_numbers) == 0) "ALL (full corpus)" else sprintf("%d specific vacancies", length(config$cambridge_vacancy_numbers))),
    "",
    "--- Everything this run produced lives under this same folder ---",
    sprintf("Version folder: %s", version_dir),
    sprintf("  similarity_scores.csv / similarity_item_matches.csv           (Cambridge side)"),
    sprintf("  validation_similarity_scores.csv / validation_similarity_item_matches.csv (external side)"),
    if (isTRUE(config$run_downstream_threshold_analysis)) "  threshold_analysis/   (weighted sweep, AUC summary, plots)" else "  threshold_analysis/   NOT RUN this run",
    if (isTRUE(config$run_downstream_item_coverage)) "  item_coverage/        (per-group + validation coverage summaries, triangulated comparison)" else "  item_coverage/        NOT RUN this run",
    "",
    "--- Note on provenance ---",
    "This manifest is the record of which template (and which similarity_scoring.R",
    "overrides) produced everything in this folder - the CSVs themselves have no",
    "per-row template identifier (see this script's header comment for why).",
    "Keep this manifest inside the folder if it's ever moved or shared - it already is.",
    "",
    "Every CSV/PNG in this folder also has version_suffix (above) baked into its",
    "own filename, so individual files pulled out of this folder for a side-by-side",
    "comparison against another version's files stay self-identifying even once",
    "they're no longer sitting next to this manifest."
  )
  writeLines(lines, manifest_path)
  log_msg(sprintf("Wrote manifest: %s", manifest_path))

  log_msg(sprintf("Done. Everything for this version is under: %s", version_dir))
  invisible(list(version_dir = version_dir, manifest_path = manifest_path))
}

if (sys.nframe() == 0) {
  run_version(CONFIG_VERSION)
}
