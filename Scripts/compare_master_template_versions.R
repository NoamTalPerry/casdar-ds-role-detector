#!/usr/bin/env Rscript
# ==============================================================================
# compare_master_template_versions.R
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
#   Compares N completed runs of run_master_template_version.R against each
#   other, side by side, one column per version:
#     Step 1 - the headline AUC comparison, from each version's own
#              auc_summary.csv.
#     Step 2 - a threshold-sweep summary comparison, collapsing each
#              version's threshold_sweep_all.csv down to two numbers that
#              are more informative than the AUC alone for explaining an
#              AUC change: where the F1 peak sits (the confident-positive
#              threshold), and the recall-floor threshold (the highest
#              threshold that still achieves full weighted recall - the
#              "worth-reviewing" lower bound). An AUC delta can come from
#              either moving independently, and conflating them is a real
#              risk when comparing two template versions by eye.
#
#   This is a pure downstream comparison across already-completed version
#   runs - it does not run similarity_scoring.R or run_threshold_analysis.R
#   itself, it only reads their output. Nothing here writes into any
#   version's own folder.
#
# Data scope (what this repository does and does not include):
#   This repository contains CODE ONLY. It does not include any
#   run_master_template_version.R output (auc_summary.csv,
#   threshold_sweep_all.csv, or anything else under
#   Output/master_template_versions/). CONFIG$versions below is an
#   illustrative example - fill it in with your own version names/labels
#   once you have completed two or more run_master_template_version.R runs
#   to compare.
#
# Important precondition: every version being compared must have been run
# with the SAME threshold_score_specs (CONFIG_VERSION$threshold_score_specs
# in run_master_template_version.R) - otherwise a (score_spec,
# partial_scheme) combination present in one version's output and absent in
# another's just shows up as NA in that version's column, which is easy to
# misread as "worse" rather than "not swept". check_spec_coverage() below
# warns (does not stop) if this happens; check the log.
#
# Usage:
#   Fill in CONFIG$versions below - add, remove, reorder, or relabel entries
#   freely, any number >= 2, no code changes needed - then:
#     Rscript compare_master_template_versions.R
#
# Requirements: base R only, no new packages (matches the rest of this
# pipeline's convention of avoiding new dependencies for analysis scripts).
# ==============================================================================

CONFIG <- list(
  # One entry per version to compare, in the order you want as columns.
  #   version_name   - must match the folder name under versions_root
  #                    (Output/master_template_versions/<version_name>/).
  #   version_suffix - defaults to version_name if left NULL, same default
  #                    run_master_template_version.R itself uses. Only set
  #                    this explicitly if that version's run overrode
  #                    CONFIG_VERSION$version_suffix to something other
  #                    than its version_name.
  #   label          - column-name / plot-legend label. Defaults to
  #                    version_name if left NULL. Must be unique across
  #                    entries.
  #
  # The four entries below are a placeholder example (a baseline plus three
  # variants) - replace with your own version_name/label values once you
  # have real run_master_template_version.R output to compare.
  versions = list(
    list(version_name = "v1_baseline", version_suffix = NULL,
         label = "Baseline"),
    list(version_name = "v2_variant_a", version_suffix = NULL,
         label = "Variant A"),
    list(version_name = "v3_variant_b", version_suffix = NULL,
         label = "Variant B"),
    list(version_name = "v4_candidate", version_suffix = NULL,
         label = "Candidate master")
  ),

  versions_root = file.path("..", "Output", "master_template_versions"),

  # Which version's numbers every delta_vs_baseline__* column is computed
  # against. Must match one of the version_name values above.
  baseline_version = "v4_candidate",

  # --- Step toggles --------------------------------------------------------
  run_auc_comparison   = TRUE,   # Step 1
  run_sweep_comparison = TRUE,   # Step 2
  make_plots = TRUE,             # grouped-bar AUC comparison, one PNG per
                                  # partial_scheme (exclude / collapse)

  output_dir = file.path("..", "Output", "master_template_comparisons",
                         "v4_candidate_vs_baseline"),

  log_to_file = TRUE,
  log_dir     = file.path("..", "Output", "logs")
)

# ==============================================================================
# Logging (same harness duplicated identically across every script in this
# pipeline. This is deliberate - it keeps each script standalone and
# runnable on its own; duplicating an IDENTICAL implementation under the
# same name across scripts is safe, only a DIFFERING implementation under a
# shared name would risk a silent collision if two scripts were ever
# sourced together).
# ==============================================================================
.log_con <- NULL
log_msg <- function(text) { message(text); if (!is.null(.log_con)) cat(text, "\n", file = .log_con, sep = "") }
log_warn <- function(text) log_msg(paste("WARNING:", text))
setup_log_file <- function(config) {
  if (!isTRUE(config$log_to_file)) return(NULL)
  if (!dir.exists(config$log_dir)) dir.create(config$log_dir, recursive = TRUE)
  log_path <- file.path(config$log_dir, sprintf("compare_master_template_versions_run_log_%s.txt", format(Sys.time(), "%Y%m%d_%H%M%S")))
  con <- file(log_path, open = "at")
  message(sprintf("Logging this run to: %s", log_path))
  con
}

# Duplicated verbatim from every other script in the run_master_template_version.R
# chain - see the note above the logging harness for why identical
# duplicated implementations under a shared name are safe here.
apply_version_suffix <- function(path, suffix) {
  if (is.null(path) || is.null(suffix) || !nzchar(suffix)) return(path)
  ext  <- tools::file_ext(path)
  base <- tools::file_path_sans_ext(path)
  paste0(base, "_", suffix, if (nzchar(ext)) paste0(".", ext) else "")
}

# ==============================================================================
# Resolve each CONFIG$versions entry into concrete input paths + a display
# label. This is the only place that needs to know the on-disk layout
# run_master_template_version.R produces.
# ==============================================================================
resolve_version <- function(v, versions_root) {
  if (is.null(v$version_name) || !nzchar(v$version_name)) {
    stop("Every entry in CONFIG$versions needs a non-empty version_name.")
  }
  suffix <- if (is.null(v$version_suffix) || !nzchar(v$version_suffix)) v$version_name else v$version_suffix
  label  <- if (is.null(v$label) || !nzchar(v$label)) v$version_name else v$label
  version_dir   <- file.path(versions_root, v$version_name)
  threshold_dir <- file.path(version_dir, "threshold_analysis")
  list(
    version_name = v$version_name,
    label = label,
    auc_summary_path     = apply_version_suffix(file.path(threshold_dir, "auc_summary.csv"), suffix),
    threshold_sweep_path = apply_version_suffix(file.path(threshold_dir, "threshold_sweep_all.csv"), suffix)
  )
}

# ==============================================================================
# Step 1: AUC comparison
# ==============================================================================
load_auc_summary <- function(rv) {
  if (!file.exists(rv$auc_summary_path)) {
    stop(sprintf("[%s] auc_summary not found: %s (did run_master_template_version.R's threshold-analysis step run for this version?)", rv$label, rv$auc_summary_path))
  }
  df <- utils::read.csv(rv$auc_summary_path, stringsAsFactors = FALSE)
  df$version <- rv$label
  df
}

# Warns (does not stop) if the set of (score_spec, partial_scheme)
# combinations differs across versions - the comparability precondition
# documented in this file's header.
check_spec_coverage <- function(long_df) {
  long_df$spec_scheme <- paste(long_df$score_spec, long_df$partial_scheme, sep = " / ")
  by_version  <- split(long_df$spec_scheme, long_df$version)
  all_combos  <- sort(unique(long_df$spec_scheme))
  for (v in names(by_version)) {
    missing <- setdiff(all_combos, unique(by_version[[v]]))
    if (length(missing) > 0) {
      log_warn(sprintf(
        "version '%s' is missing (score_spec / partial_scheme) combination(s) present in other versions: %s. Those cells will show NA in the comparison, not \"worse\" - check that version's threshold_score_specs matched the others.",
        v, paste(missing, collapse = "; ")
      ))
    }
  }
}

# Light sanity check: n_positive/n_negative for a given (score_spec,
# partial_scheme) reflect which sampled vacancies have a non-missing score
# for that spec - a property of the job text, not of which master template
# is being scored against - so they should normally match across versions.
# A difference is unusual enough to flag, though not necessarily a bug
# (e.g. a template change could plausibly alter section parsing).
check_n_consistency <- function(long_df) {
  key <- paste(long_df$score_spec, long_df$partial_scheme)
  for (k in unique(key)) {
    sub <- long_df[key == k, c("version", "n_positive", "n_negative")]
    if (length(unique(paste(sub$n_positive, sub$n_negative))) > 1) {
      log_warn(sprintf(
        "n_positive/n_negative varies across versions for '%s': %s",
        k, paste(sprintf("%s=%d/%d", sub$version, sub$n_positive, sub$n_negative), collapse = "; ")
      ))
    }
  }
}

run_auc_comparison <- function(resolved_versions, baseline_label, output_dir) {
  log_msg("--- Step 1: AUC comparison ---")
  long_df <- do.call(rbind, lapply(resolved_versions, load_auc_summary))
  check_spec_coverage(long_df)
  check_n_consistency(long_df)

  # Wide comparison, one AUC column per version - built with an explicit
  # merge loop (not stats::reshape, which is fragile in base R and isn't
  # used elsewhere in this pipeline; triangulate_item_coverage.R uses the
  # same merge-loop pattern for the same reason).
  per_version <- lapply(resolved_versions, function(rv) {
    d <- long_df[long_df$version == rv$label, c("score_spec", "partial_scheme", "weighted_AUC")]
    names(d)[names(d) == "weighted_AUC"] <- paste0("AUC__", rv$label)
    d
  })
  wide <- per_version[[1]]
  for (i in seq(2, length(per_version))) {
    wide <- merge(wide, per_version[[i]], by = c("score_spec", "partial_scheme"), all = TRUE)
  }

  baseline_col <- paste0("AUC__", baseline_label)
  if (!(baseline_col %in% names(wide))) {
    stop(sprintf("baseline_version's label ('%s') not found among compared versions' labels.", baseline_label))
  }
  version_cols <- grep("^AUC__", names(wide), value = TRUE)
  for (col in setdiff(version_cols, baseline_col)) {
    delta_col <- sub("^AUC__", "delta_vs_baseline__", col)
    wide[[delta_col]] <- wide[[col]] - wide[[baseline_col]]
  }
  wide <- wide[order(wide$score_spec, wide$partial_scheme), ]

  # Output columns (auc_comparison_wide.csv), one row per (score_spec,
  # partial_scheme):
  #   score_spec / partial_scheme      - which score definition and
  #                                     Partial-DS handling scheme
  #   AUC__<version label> (one per        - that version's weighted AUC for
  #   CONFIG$versions entry)                 this spec/scheme
  #   delta_vs_baseline__<version label>       - AUC__<label> minus the
  #   (one per non-baseline version)             baseline version's AUC for
  #                                              the same row
  out_path <- file.path(output_dir, "auc_comparison_wide.csv")
  utils::write.csv(wide, out_path, row.names = FALSE)
  log_msg(sprintf("Wrote AUC comparison (%d rows) to %s", nrow(wide), out_path))
  list(long = long_df, wide = wide)
}

# ==============================================================================
# Step 2: threshold-sweep summary comparison
# ==============================================================================
load_threshold_sweep <- function(rv) {
  if (!file.exists(rv$threshold_sweep_path)) {
    stop(sprintf("[%s] threshold_sweep_all not found: %s", rv$label, rv$threshold_sweep_path))
  }
  df <- utils::read.csv(rv$threshold_sweep_path, stringsAsFactors = FALSE)
  df$version <- rv$label
  df
}

# Collapses one (score_spec, partial_scheme, version) group's full sweep to
# the F1-peak and recall-floor numbers - see this file's header comment for
# why both are tracked rather than just the AUC.
summarize_one_group <- function(g) {
  g <- g[order(g$threshold), ]

  f1_row <- g[which.max(g$weighted_F1), ]

  max_tpr <- max(g$weighted_TPR, na.rm = TRUE)
  full_recall_rows <- g[abs(g$weighted_TPR - max_tpr) < 1e-9, ]
  recall_floor_threshold <- max(full_recall_rows$threshold)

  data.frame(
    f1_peak_threshold  = f1_row$threshold,
    f1_peak_value      = f1_row$weighted_F1,
    f1_peak_precision  = f1_row$weighted_precision,
    f1_peak_recall     = f1_row$weighted_recall,
    f1_peak_n_TP_raw   = f1_row$n_TP_raw,
    f1_peak_n_FP_raw   = f1_row$n_FP_raw,
    max_weighted_TPR   = max_tpr,
    recall_floor_threshold = recall_floor_threshold,
    stringsAsFactors = FALSE
  )
}

run_sweep_comparison <- function(resolved_versions, baseline_label, output_dir) {
  log_msg("--- Step 2: threshold-sweep summary comparison ---")
  sweep_df <- do.call(rbind, lapply(resolved_versions, load_threshold_sweep))

  groups <- split(sweep_df, list(sweep_df$score_spec, sweep_df$partial_scheme, sweep_df$version), drop = TRUE)
  summary_rows <- lapply(groups, function(g) {
    s <- summarize_one_group(g)
    s$score_spec     <- g$score_spec[1]
    s$partial_scheme <- g$partial_scheme[1]
    s$version        <- g$version[1]
    s
  })
  summary_long <- do.call(rbind, summary_rows)
  rownames(summary_long) <- NULL
  summary_long <- summary_long[order(summary_long$score_spec, summary_long$partial_scheme, summary_long$version), ]

  # Output columns (sweep_summary_by_version.csv), one row per (score_spec,
  # partial_scheme, version):
  #   score_spec / partial_scheme / version  - which combination this row
  #                                           summarizes
  #   f1_peak_threshold / f1_peak_value /      - the threshold where weighted
  #   f1_peak_precision / f1_peak_recall /       F1 peaks, and the
  #   f1_peak_n_TP_raw / f1_peak_n_FP_raw          precision/recall/raw
  #                                                counts at that threshold
  #   max_weighted_TPR                               - the highest weighted
  #                                                   TPR reached anywhere in
  #                                                   the sweep
  #   recall_floor_threshold                           - the highest
  #                                                     threshold that still
  #                                                     achieves max_weighted_TPR
  #                                                     (the "worth-reviewing"
  #                                                     lower bound - see this
  #                                                     script's header)
  long_out_path <- file.path(output_dir, "sweep_summary_by_version.csv")
  utils::write.csv(summary_long, long_out_path, row.names = FALSE)
  log_msg(sprintf("Wrote sweep summary (%d rows) to %s", nrow(summary_long), long_out_path))

  # Wide comparison, one column per version, for the two numbers most
  # likely to explain an AUC delta (f1_peak_value, recall_floor_threshold),
  # plus f1_peak_threshold for context. Same merge-loop approach as
  # run_auc_comparison(), for the same reason.
  key_metrics <- c("f1_peak_value", "f1_peak_threshold", "recall_floor_threshold")
  wide_out <- NULL
  for (metric in key_metrics) {
    per_version <- lapply(resolved_versions, function(rv) {
      d <- summary_long[summary_long$version == rv$label, c("score_spec", "partial_scheme", metric)]
      names(d)[names(d) == metric] <- paste0(metric, "__", rv$label)
      d
    })
    metric_wide <- per_version[[1]]
    for (i in seq(2, length(per_version))) {
      metric_wide <- merge(metric_wide, per_version[[i]], by = c("score_spec", "partial_scheme"), all = TRUE)
    }
    wide_out <- if (is.null(wide_out)) metric_wide else merge(wide_out, metric_wide, by = c("score_spec", "partial_scheme"), all = TRUE)
  }
  wide_out <- wide_out[order(wide_out$score_spec, wide_out$partial_scheme), ]

  # Output columns (sweep_key_metrics_wide.csv), one row per (score_spec,
  # partial_scheme):
  #   score_spec / partial_scheme                     - which combination
  #   f1_peak_value__<version label> /                    - one column per
  #   f1_peak_threshold__<version label> /                  metric x version,
  #   recall_floor_threshold__<version label>                 pulled from
  #   (one trio per CONFIG$versions entry)                      sweep_summary_by_version.csv
  #                                                              for
  #                                                              side-by-side
  #                                                              comparison
  wide_out_path <- file.path(output_dir, "sweep_key_metrics_wide.csv")
  utils::write.csv(wide_out, wide_out_path, row.names = FALSE)
  log_msg(sprintf("Wrote sweep key-metrics comparison (%d rows) to %s", nrow(wide_out), wide_out_path))
  list(long = summary_long, wide = wide_out)
}

# ==============================================================================
# Optional plot: grouped bars, one cluster per score_spec, one bar per
# version, one PNG per partial_scheme. Base R graphics only, same style as
# triangulate_item_coverage.R's comparison plot.
# ==============================================================================
generate_auc_comparison_plot <- function(long_df, scheme, output_dir) {
  d <- long_df[long_df$partial_scheme == scheme, ]
  specs    <- sort(unique(d$score_spec))
  versions <- unique(d$version)   # preserves CONFIG$versions order
  mat <- matrix(NA_real_, nrow = length(versions), ncol = length(specs), dimnames = list(versions, specs))
  for (i in seq_len(nrow(d))) mat[d$version[i], d$score_spec[i]] <- d$weighted_AUC[i]

  path <- file.path(output_dir, sprintf("auc_comparison_%s.png", scheme))
  grDevices::png(path, width = max(900, 120 * length(specs)), height = 700, res = 130)
  op <- graphics::par(mar = c(7, 4, 3, 2))
  palette <- c("#1b6ca8", "#27ae60", "#c0392b", "#8e44ad", "#e67e22", "#16a085", "#d35400")
  graphics::barplot(
    mat, beside = TRUE, las = 2, cex.names = 0.8,
    col = palette[seq_len(nrow(mat))],
    ylim = c(0, 1), ylab = "Weighted AUC",
    main = sprintf("AUC by version - partial_scheme = %s", scheme)
  )
  graphics::legend("topright", legend = versions, fill = palette[seq_len(nrow(mat))], bty = "n", cex = 0.8)
  graphics::par(op)
  grDevices::dev.off()
  log_msg(sprintf("Wrote plot: %s", path))
}

# ==============================================================================
# Main
# ==============================================================================
main_compare_versions <- function(config) {
  .log_con <<- setup_log_file(config)
  on.exit(if (!is.null(.log_con)) close(.log_con), add = TRUE)
  log_msg(sprintf("=== compare_master_template_versions.R run started: %s ===", Sys.time()))

  if (length(config$versions) < 2) stop("CONFIG$versions needs at least 2 entries to compare anything.")

  resolved_versions <- lapply(config$versions, resolve_version, versions_root = config$versions_root)
  labels <- vapply(resolved_versions, function(rv) rv$label, character(1))
  if (any(duplicated(labels))) {
    stop(sprintf(
      "Duplicate version label(s): %s - labels must be unique (defaults to version_name; set CONFIG$versions[[i]]$label to disambiguate).",
      paste(unique(labels[duplicated(labels)]), collapse = ", ")
    ))
  }
  log_msg(sprintf("Comparing %d version(s): %s", length(resolved_versions), paste(labels, collapse = ", ")))

  version_names <- vapply(resolved_versions, function(rv) rv$version_name, character(1))
  baseline_idx <- which(version_names == config$baseline_version)
  if (length(baseline_idx) == 0) {
    stop(sprintf("baseline_version ('%s') not found among CONFIG$versions.", config$baseline_version))
  }
  baseline_label <- resolved_versions[[baseline_idx[1]]]$label

  dir.create(config$output_dir, recursive = TRUE, showWarnings = FALSE)

  auc_result   <- NULL
  sweep_result <- NULL

  if (isTRUE(config$run_auc_comparison)) {
    auc_result <- run_auc_comparison(resolved_versions, baseline_label, config$output_dir)
    if (isTRUE(config$make_plots)) {
      for (scheme in unique(auc_result$long$partial_scheme)) {
        tryCatch(
          generate_auc_comparison_plot(auc_result$long, scheme, config$output_dir),
          error = function(e) log_warn(sprintf("Could not save AUC comparison plot for scheme '%s' (%s) - continuing without it.", scheme, conditionMessage(e)))
        )
      }
    }
  }

  if (isTRUE(config$run_sweep_comparison)) {
    sweep_result <- run_sweep_comparison(resolved_versions, baseline_label, config$output_dir)
  }

  # --- Not yet built, deliberately: a Step 3 that would join each version's
  # per-vacancy scored output on vacancy_number and compute per-vacancy
  # score deltas (the "which specific vacancies moved, and by how much"
  # view). Natural extension if the Step 1/2 comparison flags something
  # worth explaining at the per-vacancy level; not built now since it
  # wasn't needed for this round of comparisons.

  log_msg("Done.")
  invisible(list(auc = auc_result, sweep = sweep_result))
}

if (sys.nframe() == 0) {
  main_compare_versions(CONFIG)
}
