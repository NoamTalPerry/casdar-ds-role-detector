#!/usr/bin/env Rscript
# ==============================================================================
# triangulate_item_coverage.R  (companion to analyze_item_coverage.R)
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
#   Joins two or more already-aggregated item_coverage_summary.csv files
#   (each produced by analyze_item_coverage.R, typically one per label group
#   via its vacancy_filter_path/output_suffix options - see that script) into
#   one wide table per master item, so it's possible to actually answer "does
#   this item discriminate DS from non-DS" rather than looking at each
#   group's coverage in isolation.
#
# Data scope (what this repository does and does not include):
#   This repository contains CODE ONLY. It does not include any
#   item_coverage_summary.csv output. To run this script yourself, first
#   produce one summary per group you want to compare via
#   analyze_item_coverage.R (see Usage below) against your own corpus.
#
# Why this needs its own script rather than just re-slicing one summary:
#   analyze_item_coverage.R's output is already aggregated across whichever
#   vacancies were in scope for that run - there's no per-vacancy score left
#   to split by label afterward. The split has to happen BEFORE aggregation
#   (i.e. run analyze_item_coverage.R once per label group, using its
#   vacancy_filter_path + output_suffix), and this script's job starts after
#   that: joining the resulting summaries together for comparison.
#
# Usage:
#   1. Run analyze_item_coverage.R once per group you want to compare (e.g.
#      once with a DS-labelled vacancy filter, once with a No-DS-labelled
#      filter, plus an existing validation-set summary from an external
#      positive set).
#   2. Fill in CONFIG$coverage_inputs below with each resulting file, and
#      Rscript triangulate_item_coverage.R
#
# Requirements: base R only, no new packages.
# ==============================================================================

CONFIG <- list(
  # Named list, one entry per coverage-summary file to compare - two
  # entries minimum, any number above that. The names become column suffixes
  # in the output (keep them short, filename-safe) and plot labels. Order
  # here is preserved in the output column order and the plot legend.
  coverage_inputs = list(
    validation_positive = file.path("..", "Validation_Output",
                                    "validation_item_coverage_summary.csv"),   # e.g. "../Validation_Output/validation_item_coverage_summary.csv"
    main_ds         = file.path("..", "Output",
                                "item_coverage_summary_main_ds.csv"),
    main_partial    = file.path("..", "Output",
                                "item_coverage_summary_main_partial.csv"),
    main_nods       = file.path("..", "Output",
                                "item_coverage_summary_main_nods.csv")
  ),
  
  # Which columns to carry through from each input, beyond the join keys
  # (section, master_item_id, master_item_text). mean_similarity is the
  # primary comparison metric; n_vacancies_compared lets you see how much
  # each group's number is actually resting on (a large gap backed by 2
  # vacancies is a different kind of finding than one backed by 20).
  # sd_similarity is needed to turn discrimination_gap into a standard
  # error (see compute_gap_uncertainty() below) - it's already computed by
  # analyze_item_coverage.R's summarize_master_item() for every input file.
  # Remove it from this
  # list to fall back to raw-gap-only output (discrimination_gap_se/_z are
  # then skipped, with a log note, rather than erroring).
  carry_columns = c("mean_similarity", "n_vacancies_compared", "match_rate_at_0.6", "sd_similarity"),
  
  # Which two group names (must match coverage_inputs' names) to compute
  # the primary discrimination gap from: gap = mean_similarity[positive_group]
  # - mean_similarity[negative_group]. Output is sorted by this gap
  # ascending (worst/most-concerning discriminators first, matching
  # analyze_item_coverage.R's own "worst first" convention).
  primary_positive_group = "main_ds",
  primary_negative_group = "main_nods",
  
  output_path = file.path("..", "Output", "item_coverage_triangulated.csv"),
  plot_output_path = file.path("..", "Output", "item_coverage_triangulated_plot.png"),
  
  # Which MASTER TEMPLATE VERSION produced the inputs being triangulated
  # here (e.g. "v2_adjacent_role") - appended to output_path/plot_output_path
  # via apply_version_suffix(), the same mechanism/semantics used
  # identically across every script in the run_master_template_version.R
  # chain (see analyze_item_coverage.R's own version_suffix comment). Set
  # automatically by that wrapper; NULL (no suffix) for a plain standalone
  # run.
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
  log_path <- file.path(config$log_dir, sprintf("triangulate_item_coverage_run_log_%s.txt", format(Sys.time(), "%Y%m%d_%H%M%S")))
  con <- file(log_path, open = "at")
  message(sprintf("Logging this run to: %s", log_path))
  con
}

# Same append-before-extension behavior duplicated identically across every
# script in the run_master_template_version.R chain (analyze_item_coverage.R,
# run_threshold_analysis.R, similarity_scoring.R). This is deliberate - it
# keeps each script standalone and runnable on its own; duplicating an
# IDENTICAL implementation under the same name across scripts is safe, only
# a DIFFERING implementation under a shared name would risk a silent
# collision if two scripts were ever sourced together. NULL/empty suffix is
# a no-op.
apply_version_suffix <- function(path, suffix) {
  if (is.null(path) || is.null(suffix) || !nzchar(suffix)) return(path)
  ext  <- tools::file_ext(path)
  base <- tools::file_path_sans_ext(path)
  paste0(base, "_", suffix, if (nzchar(ext)) paste0(".", ext) else "")
}

# ==============================================================================
# Load one coverage-summary file, tag its carry_columns with the group name.
# ==============================================================================
load_one_summary <- function(group_name, path, carry_columns) {
  if (!file.exists(path)) stop(sprintf("coverage_inputs['%s'] file not found: %s", group_name, path))
  df <- utils::read.csv(path, stringsAsFactors = FALSE)
  missing_cols <- setdiff(carry_columns, names(df))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "coverage_inputs['%s'] (%s) is missing requested carry_columns: %s. Columns present: %s",
      group_name, path, paste(missing_cols, collapse = ", "), paste(names(df), collapse = ", ")
    ))
  }
  keep <- df[, c("section", "master_item_id", "master_item_text", carry_columns), drop = FALSE]
  names(keep)[names(keep) %in% carry_columns] <- paste0(carry_columns, "__", group_name)
  log_msg(sprintf("Loaded '%s' from %s: %d master item(s).", group_name, path, nrow(keep)))
  keep
}

# ==============================================================================
# Optional: standard error / z-like score for the primary discrimination
# gap, using a Welch (unequal-variance) two-sample SE:
#   se   = sqrt(sd_pos^2 / n_pos  +  sd_neg^2 / n_neg)
#   z    = discrimination_gap / se
#
# This is a small-sample SCREENING aid, not a formal per-item significance
# test - with a small group size, per-item power is low, and a |z| >= ~2
# shouldn't be read as "proven real" any more than a |z| < 1 should be read
# as "proven noise". Use it to RANK/FLAG candidates worth a closer look
# (and, ultimately, to decide what's worth testing via an actual
# master-template ablation through run_master_template_version.R +
# compare_master_template_versions.R) - not to accept or reject any single
# item's gap on this number alone.
#
# discrimination_gap_se / discrimination_gap_z are always added to the
# output (so downstream code and CSV consumers see a stable schema); if
# sd_similarity/n_vacancies_compared aren't available for both primary
# groups (e.g. 'sd_similarity' was removed from CONFIG$carry_columns), both
# columns are filled with NA and a warning is logged instead of erroring.
# An individual item can also come out NA even when the columns are
# present - e.g. if n_vacancies_compared <= 1 for that item in one group,
# analyze_item_coverage.R's summarize_master_item() itself never computed a
# usable sd_similarity for it (sd of 0 or 1 value is undefined/NA there).
# ==============================================================================
compute_gap_uncertainty <- function(merged, config) {
  pos <- config$primary_positive_group
  neg <- config$primary_negative_group
  needed <- c(
    sprintf("sd_similarity__%s", c(pos, neg)),
    sprintf("n_vacancies_compared__%s", c(pos, neg))
  )
  missing_cols <- setdiff(needed, names(merged))
  if (length(missing_cols) > 0) {
    log_warn(sprintf(
      "Skipping discrimination_gap_se/discrimination_gap_z (both filled with NA) - missing column(s): %s. Add 'sd_similarity' to CONFIG$carry_columns to enable this.",
      paste(missing_cols, collapse = ", ")
    ))
    merged$discrimination_gap_se <- NA_real_
    merged$discrimination_gap_z  <- NA_real_
    return(merged)
  }
  
  sd_pos <- merged[[sprintf("sd_similarity__%s", pos)]]
  sd_neg <- merged[[sprintf("sd_similarity__%s", neg)]]
  n_pos  <- merged[[sprintf("n_vacancies_compared__%s", pos)]]
  n_neg  <- merged[[sprintf("n_vacancies_compared__%s", neg)]]
  
  # NA propagates naturally here (e.g. an item with n_vacancies_compared <= 1
  # in one group has NA sd_similarity coming in) - an item with an unusable
  # SE surfaces as NA in the output, never as a misleadingly precise number.
  se <- sqrt((sd_pos^2 / n_pos) + (sd_neg^2 / n_neg))
  merged$discrimination_gap_se <- se
  merged$discrimination_gap_z  <- ifelse(!is.na(se) & se > 0, merged$discrimination_gap / se, NA_real_)
  
  n_unusable <- sum(is.na(se))
  if (n_unusable > 0) {
    log_warn(sprintf(
      "%d master item(s) have no usable discrimination_gap_se (sd_similarity unavailable for at least one primary group - typically n_vacancies_compared <= 1 for that item there).",
      n_unusable
    ))
  }
  
  low_z <- merged[!is.na(merged$discrimination_gap_z) & abs(merged$discrimination_gap_z) < 1,
                  c("section", "master_item_id", "discrimination_gap", "discrimination_gap_se", "discrimination_gap_z"), drop = FALSE]
  if (nrow(low_z) > 0) {
    log_msg(sprintf(
      "%d master item(s) have |discrimination_gap_z| < 1 (gap smaller than its own standard error - not distinguishable from noise at this sample size, regardless of sign):",
      nrow(low_z)
    ))
    print(low_z[order(low_z$discrimination_gap_z), ])
  }
  
  merged
}

# ==============================================================================
# Main
# ==============================================================================
main_triangulate_item_coverage <- function(config) {
  .log_con <<- setup_log_file(config)
  on.exit(if (!is.null(.log_con)) close(.log_con), add = TRUE)
  log_msg(sprintf("=== triangulate_item_coverage.R run started: %s ===", Sys.time()))
  
  groups <- names(config$coverage_inputs)
  if (length(groups) < 2) stop("coverage_inputs needs at least 2 entries to triangulate anything.")
  
  tables <- lapply(groups, function(g) load_one_summary(g, config$coverage_inputs[[g]], config$carry_columns))
  
  # Join everything on (section, master_item_id, master_item_text). Using a
  # full outer join throughout, so a master item present in one group's
  # template but missing from another's output (shouldn't normally happen -
  # the master template is the same everywhere - but worth not silently
  # dropping rows if it does) stays visible with NAs rather than disappearing.
  merged <- tables[[1]]
  for (i in seq(2, length(tables))) {
    merged <- merge(merged, tables[[i]], by = c("section", "master_item_id", "master_item_text"), all = TRUE)
  }
  
  n_incomplete <- sum(!stats::complete.cases(merged))
  if (n_incomplete > 0) {
    log_warn(sprintf(
      "%d of %d master item(s) are missing data from at least one group (shows as NA) - check the input files cover the same master template.",
      n_incomplete, nrow(merged)
    ))
  }
  
  # --- Primary discrimination gap ---------------------------------------------
  pos_col <- paste0("mean_similarity__", config$primary_positive_group)
  neg_col <- paste0("mean_similarity__", config$primary_negative_group)
  if (!(pos_col %in% names(merged)) || !(neg_col %in% names(merged))) {
    stop(sprintf(
      "primary_positive_group ('%s') and primary_negative_group ('%s') must both be in coverage_inputs and 'mean_similarity' must be in carry_columns.",
      config$primary_positive_group, config$primary_negative_group
    ))
  }
  merged$discrimination_gap <- merged[[pos_col]] - merged[[neg_col]]
  
  # --- Uncertainty around that gap (SE / z) - see compute_gap_uncertainty() --
  merged <- compute_gap_uncertainty(merged, config)
  
  # Worst (smallest/most negative gap) first - the most actionable ordering,
  # same convention as analyze_item_coverage.R's own sort. Deliberately still
  # sorted by the RAW gap, not by z - a small, precisely-estimated gap and a
  # small, noisy one both belong near the top of this ordering; z is there
  # alongside it so you can tell the two apart, not to replace the sort.
  merged <- merged[order(merged$discrimination_gap, na.last = TRUE), ]
  
  output_path <- apply_version_suffix(config$output_path, config$version_suffix)
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  
  # Output columns (item_coverage_triangulated.csv), one row per master item:
  #   section / master_item_id / master_item_text  - which master criterion
  #                                                  (join key across groups)
  #   <carry_column>__<group_name>  - one column per (CONFIG$carry_columns x
  #                                  CONFIG$coverage_inputs) combination, e.g.
  #                                  mean_similarity__main_ds,
  #                                  n_vacancies_compared__main_nods -
  #                                  see analyze_item_coverage.R's own column
  #                                  glossary for what each carried metric
  #                                  means within one group
  #   discrimination_gap              - mean_similarity[primary_positive_group]
  #                                    minus mean_similarity[primary_negative_group];
  #                                    sorted ascending (weakest discriminators
  #                                    first)
  #   discrimination_gap_se             - Welch standard error of that gap;
  #                                      NA if sd_similarity/n_vacancies_compared
  #                                      weren't carried through for both
  #                                      primary groups, or unusable for a
  #                                      given item (see compute_gap_uncertainty())
  #   discrimination_gap_z                - discrimination_gap / discrimination_gap_se;
  #                                        a small-sample screening aid, not
  #                                        a formal significance test - see
  #                                        compute_gap_uncertainty()'s header
  utils::write.csv(merged, output_path, row.names = FALSE)
  log_msg(sprintf("Wrote %d-row triangulated table to %s", nrow(merged), output_path))
  
  report_cols <- c("section", "master_item_id", "discrimination_gap", "discrimination_gap_se", "discrimination_gap_z")
  worst <- utils::head(merged[!is.na(merged$discrimination_gap), report_cols], 5)
  log_msg("Weakest discriminators (smallest DS-vs-No-DS gap):")
  print(worst)
  best <- utils::tail(merged[!is.na(merged$discrimination_gap), report_cols], 5)
  log_msg("Strongest discriminators (largest DS-vs-No-DS gap):")
  print(best)
  
  # --- Optional plot: grouped bars, one cluster per master item --------------
  if (!is.null(config$plot_output_path)) {
    tryCatch({
      plot_df <- merged[stats::complete.cases(merged[, paste0("mean_similarity__", groups)]), ]
      if (nrow(plot_df) == 0) {
        log_warn("No items with complete data across all groups - skipping plot.")
      } else {
        mat <- t(as.matrix(plot_df[, paste0("mean_similarity__", groups)]))
        colnames(mat) <- paste0(plot_df$master_item_id, " (", substr(plot_df$section, 1, 4), ")")
        plot_output_path <- apply_version_suffix(config$plot_output_path, config$version_suffix)
        dir.create(dirname(plot_output_path), recursive = TRUE, showWarnings = FALSE)
        grDevices::png(plot_output_path, width = max(1000, 45 * ncol(mat)), height = 700, res = 130)
        op <- graphics::par(mar = c(9, 4, 3, 2))
        palette <- c("#27ae60", "#1b6ca8", "#c0392b", "#8e44ad", "#e67e22")
        graphics::barplot(
          mat, beside = TRUE, las = 2, cex.names = 0.65,
          col = palette[seq_along(groups)],
          ylim = c(0, 1), ylab = "Mean similarity",
          main = "Item coverage triangulated across groups"
        )
        graphics::legend("topright", legend = groups, fill = palette[seq_along(groups)], bty = "n", cex = 0.8)
        graphics::par(op)
        grDevices::dev.off()
        log_msg(sprintf("Wrote plot: %s", plot_output_path))
      }
    }, error = function(e) log_warn(sprintf("Could not save plot (%s) - continuing without it.", conditionMessage(e))))
  }
  
  log_msg("Done.")
  invisible(merged)
}

if (sys.nframe() == 0) {
  main_triangulate_item_coverage(CONFIG)
}