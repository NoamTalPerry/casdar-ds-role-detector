#!/usr/bin/env Rscript
# ==============================================================================
# run_item_coverage_by_label.R  (wrapper around analyze_item_coverage.R)
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
#   Runs analyze_item_coverage.R once per label group (default: Cambridge
#   DS+Partial DS combined vs. Cambridge No-DS, matching the "collapse"
#   scheme already used throughout this project's threshold analysis),
#   deriving each group's vacancy list directly from a labelled-sample file
#   rather than requiring separate manual filter CSVs and separate manual
#   runs. Produces one durable trace of the whole operation: which
#   vacancies went into each group (written to disk, not just used
#   in-memory), plus each underlying analyze_item_coverage.R run's own
#   detailed log, plus a wrapper-level overview log tying it together.
#
# Data scope (what this repository does and does not include):
#   This repository contains CODE ONLY. It does not include job-ad PDFs,
#   the master template, any similarity_item_matches.csv output, or any
#   labelled sample file. To run this script yourself, first produce
#   similarity_item_matches.csv (extract_job_criteria.R then
#   similarity_scoring.R, with "item" in granularity_modes) and your own
#   labelled sample (a CSV with vacancy_number and label columns) against
#   your own corpus.
#
# Relationship to analyze_item_coverage.R:
#   Sources it to reuse main_analyze_item_coverage()/log_msg()/etc. rather than duplicating that
#   logic - same pattern as export_reliability_recheck.R sourcing
#   draw_negative_validation_sample.R. Two things worth knowing if you ever
#   edit this file:
#     - Naming the wrapper's own top-level settings CONFIG_WRAPPER (not
#       CONFIG) is deliberate - analyze_item_coverage.R's own CONFIG is a
#       real object in the shared environment once sourced, and reusing the
#       name would silently overwrite it.
#     - analyze_item_coverage.R's main_analyze_item_coverage() unconditionally reassigns the
#       shared .log_con on every call (even when log_to_file = FALSE, it
#       still gets set to NULL). That would silently kill the wrapper's own
#       log connection after the first group's run. Handled explicitly
#       below (search "restore wrapper log connection") rather than left as
#       a subtle bug - don't remove that line if you're editing this loop.
#
# Usage:
#   Fill in CONFIG_WRAPPER below, then: Rscript run_item_coverage_by_label.R
# ==============================================================================

source("analyze_item_coverage.R")   # brings in main_analyze_item_coverage(), log_msg(), setup_log_file(), apply_output_suffix(), apply_version_suffix(), etc. - does NOT auto-run (see that script's own sys.nframe() guard)
base_item_coverage_config <- CONFIG  # snapshot analyze_item_coverage.R's own CONFIG as a template, before defining our own settings below

CONFIG_WRAPPER <- list(
  # The labelled sample to split into groups - needs 'vacancy_number' and
  # 'label' columns. E.g. cambridge_labeled_with_all_scores.csv.
  labeled_data_path = file.path("..", "Output", "threshold_analysis",
                                "cambridge_labeled_with_all_scores.csv"),

  # Named list: group name -> label value(s) that define membership. Any
  # number of groups. Default matches the "collapse" scheme (DS + Partial DS
  # combined vs. No-DS) already anchoring this project's threshold analysis
  # - edit directly for a different split (e.g. three-way DS / Partial DS /
  # No-DS, or excluding Partial DS entirely to mirror the "exclude" scheme).
  groups = list(
    cambridge_ds   = c("DS"),
    cambridge_partial = c("Partial DS"),
    cambridge_nods = c("No-DS")
  ),

  # Where the derived per-group vacancy-number lists get written - kept as
  # their own small files (not just used in-memory) so there's a durable,
  # inspectable record of exactly which vacancies went into each group.
  filter_output_dir = file.path("..", "Output", "item_coverage_groups"),

  # Passed through to analyze_item_coverage.R for every group's run -
  # validation_mode is forced to FALSE regardless of what's set here (the
  # Cambridge sample lives in the main corpus's output, never the
  # validation corpus's - see analyze_item_coverage.R's own CONFIG for what
  # validation_mode actually switches).
  item_matches_path      = file.path("..", "Output", "similarity_item_matches.csv"),
  summary_output_dir      = file.path("..", "Output"),
  match_rate_thresholds     = c(0.3, 0.4, 0.5, 0.6, 0.7),
  make_plots = TRUE,

  # Passed straight through to each group's analyze_item_coverage.R run as
  # its own version_suffix - a second, independent suffix axis from the
  # per-group output_suffix set automatically below (see
  # analyze_item_coverage.R's version_suffix comment for the full
  # rationale: which MASTER TEMPLATE VERSION produced this run, not which
  # label group). Set automatically by run_master_template_version.R to its
  # own CONFIG_VERSION$version_suffix; NULL (no suffix) for a plain
  # standalone run.
  version_suffix = NULL,

  log_to_file = TRUE,
  log_dir     = file.path("..", "Output", "logs")
)

# ==============================================================================
# Validate the labelled data and CONFIG_WRAPPER$groups against each other
# before doing anything - catches a typo'd label name or an overlapping/
# missing assignment up front rather than silently producing an empty or
# wrong group.
# ==============================================================================
read_labeled_data <- function(path) {
  if (!file.exists(path)) stop(sprintf("labeled_data_path not found: %s", path))
  d <- utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character", na.strings = character(0))
  required <- c("vacancy_number", "label")
  missing <- setdiff(required, names(d))
  if (length(missing) > 0) {
    stop(sprintf("labeled_data_path is missing required column(s): %s. Columns present: %s", paste(missing, collapse = ", "), paste(names(d), collapse = ", ")))
  }
  d
}

validate_groups <- function(groups, labeled_data) {
  actual_labels <- unique(labeled_data$label)
  configured_labels <- unique(unlist(groups))

  bad <- setdiff(configured_labels, actual_labels)
  if (length(bad) > 0) {
    stop(sprintf(
      "CONFIG_WRAPPER$groups references label(s) not present in the data: %s. Labels actually present: %s",
      paste(bad, collapse = ", "), paste(actual_labels, collapse = ", ")
    ))
  }

  unassigned <- setdiff(actual_labels, configured_labels)
  if (length(unassigned) > 0) {
    log_warn(sprintf(
      "Label(s) present in the data but not assigned to any group (excluded from every group's analysis): %s",
      paste(unassigned, collapse = ", ")
    ))
  }

  label_to_groups <- list()
  for (g in names(groups)) for (l in groups[[g]]) label_to_groups[[l]] <- c(label_to_groups[[l]], g)
  overlaps <- Filter(function(gs) length(gs) > 1, label_to_groups)
  for (l in names(overlaps)) {
    log_warn(sprintf(
      "Label '%s' is assigned to multiple groups (%s) - each group's vacancy list will include it, it won't be split between them.",
      l, paste(overlaps[[l]], collapse = ", ")
    ))
  }
}

write_group_filter <- function(vacancy_numbers, group_name, dir) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  path <- file.path(dir, sprintf("vacancy_filter_%s.csv", group_name))
  utils::write.csv(data.frame(vacancy_number = vacancy_numbers), path, row.names = FALSE)
  path
}

# ==============================================================================
# Main
# ==============================================================================
run_wrapper <- function(config) {
  wrapper_log_con <- setup_log_file(config)
  .log_con <<- wrapper_log_con
  on.exit(if (!is.null(wrapper_log_con)) close(wrapper_log_con), add = TRUE)

  log_msg(sprintf("=== run_item_coverage_by_label.R wrapper started: %s ===", Sys.time()))
  log_msg(sprintf("Groups configured: %s", paste(names(config$groups), collapse = ", ")))

  labeled <- read_labeled_data(config$labeled_data_path)
  log_msg(sprintf("Loaded labelled data: %d row(s) from %s.", nrow(labeled), config$labeled_data_path))
  validate_groups(config$groups, labeled)

  result_paths <- list()

  for (group_name in names(config$groups)) {
    label_values <- config$groups[[group_name]]
    ids <- unique(labeled$vacancy_number[labeled$label %in% label_values])
    filter_path <- write_group_filter(ids, group_name, config$filter_output_dir)
    log_msg(sprintf("Group '%s' (labels: %s): %d vacancy_number(s) -> %s", group_name, paste(label_values, collapse = "/"), length(ids), filter_path))

    if (length(ids) == 0) {
      log_warn(sprintf("Group '%s' has zero vacancies - skipping its analyze_item_coverage.R run entirely.", group_name))
      next
    }

    group_config <- base_item_coverage_config
    group_config$validation_mode        <- FALSE
    group_config$item_matches_path       <- config$item_matches_path
    group_config$vacancy_filter_ids        <- NULL
    group_config$vacancy_filter_path         <- filter_path
    group_config$vacancy_filter_column         <- "vacancy_number"
    group_config$output_suffix = group_name
    group_config$version_suffix = config$version_suffix
    group_config$summary_output_path = file.path(config$summary_output_dir, "item_coverage_summary.csv")
    group_config$plot_output_path = if (isTRUE(config$make_plots)) file.path(config$summary_output_dir, "item_coverage_plot.png") else NULL
    group_config$match_rate_thresholds = config$match_rate_thresholds
    group_config$log_to_file = TRUE   # each group still gets its own detailed log from analyze_item_coverage.R itself
    group_config$log_dir = file.path(config$log_dir, group_name)   # distinct subdirectory per group - guarantees a separate log file every run, not just when timestamps happen to differ (they're seconds-precision and these calls can land in the same second)

    log_msg(sprintf("--- Running analyze_item_coverage.R for group '%s' ---", group_name))
    group_result <- main_analyze_item_coverage(group_config)
    .log_con <<- wrapper_log_con   # restore wrapper log connection - main_analyze_item_coverage() unconditionally reassigns .log_con on every call, see header comment

    # Recompute the SAME final path main_analyze_item_coverage() just wrote
    # to (output_suffix = group name first, version_suffix last - see
    # analyze_item_coverage.R's resolve_paths_for_mode_item_coverage() for
    # why this order), so what's returned/logged here always matches what's
    # actually on disk.
    summary_path <- apply_output_suffix(group_config$summary_output_path, group_name)
    summary_path <- apply_version_suffix(summary_path, config$version_suffix)
    result_paths[[group_name]] <- summary_path
    log_msg(sprintf("Group '%s' done: %d master item(s) summarized, written to %s", group_name, nrow(group_result), summary_path))
  }

  log_msg("=== All groups done. Ready-to-paste coverage_inputs for triangulate_item_coverage.R: ===")
  log_msg("coverage_inputs = list(")
  group_lines <- sprintf('  %s = "%s"', names(result_paths), unlist(result_paths))
  log_msg(paste(group_lines, collapse = ",\n"))
  log_msg(")")

  log_msg("Done.")
  invisible(result_paths)
}

if (sys.nframe() == 0) {
  run_wrapper(CONFIG_WRAPPER)
}
