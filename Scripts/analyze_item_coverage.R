#!/usr/bin/env Rscript
# ==============================================================================
# analyze_item_coverage.R  (companion analysis to similarity_scoring.R)
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
#   Aggregates similarity_item_matches.csv's "master_coverage" rows ACROSS
#   vacancies to answer: which master-template criteria show up reliably in
#   real job ads, and which rarely/never do? This is a diagnostic on the
#   MASTER TEMPLATE's empirical validity against a corpus of real postings -
#   not a per-vacancy score (similarity_scores.csv already gives you that).
#
#   For each master item, across every vacancy scored, "master_coverage"
#   records the single best-matching job bullet and its similarity (see
#   best_matches()/classify_job_items() in similarity_scoring.R - the
#   candidate job bullets are already restricted to CORE-classified content,
#   GENERAL-pool exclusions already applied upstream, nothing extra to do
#   here). This script just groups those rows by master item and summarizes
#   the distribution of best-match scores that item got across vacancies:
#   consistently low -> a candidate for "does this belong in the template?
#   or is it just phrased in a way that doesn't match how ads word it?";
#   consistently high -> a criterion that's reliably present.
#
# Data scope (what this repository does and does not include):
#   This repository contains CODE ONLY. It does not include job-ad PDFs, the
#   master template, or any scored output - see this repository's README
#   for where the master template is deposited. To run this script yourself,
#   first produce a similarity_item_matches.csv by running
#   extract_job_criteria.R then similarity_scoring.R (with "item" included
#   in its granularity_modes) against your own corpus and master template.
#
# Relationship to similarity_scoring.R:
#   Pure consumer of its output file - never recomputes embeddings or
#   touches Ollama. Only meaningful once at least one similarity_scoring.R
#   run with "item" in granularity_modes has produced similarity_item_matches.csv.
#
# validation_mode works exactly like it does in similarity_scoring.R: TRUE
# switches every path below to its Validation_Output/ counterpart, so
# analyzing the validation corpus never touches the main study's files and
# vice versa. This is the ONLY thing that needs to change to point this
# script at the main study data later - same toggle, same meaning.
#
# Usage:
#   Rscript analyze_item_coverage.R
#   Or source() this file and call main_analyze_item_coverage(CONFIG)
#   interactively (e.g. from RStudio) - tweak CONFIG first if you like,
#   nothing here requires being run as a batch job.
#
# Requirements: base R only, no new packages beyond what similarity_scoring.R
# already requires to produce this script's input file.
# ==============================================================================

CONFIG <- list(
  validation_mode = TRUE,   # default TRUE for now since that's the corpus that exists to analyze - flip to FALSE for the main study once it has item-level output

  item_matches_path            = file.path("..", "Output", "similarity_item_matches.csv"),
  validation_item_matches_path = file.path("..", "Validation_Output", "validation_similarity_item_matches.csv"),

  summary_output_path            = file.path("..", "Output", "item_coverage_summary.csv"),
  validation_summary_output_path = file.path("..", "Validation_Output", "validation_item_coverage_summary.csv"),

  # Similarity thresholds to report a "match rate" (proportion of vacancies
  # where this master item's best match scored >= threshold) at -
  # deliberately reported at SEVERAL thresholds as separate columns rather
  # than picking one, since threshold-setting is exactly what you're still
  # doing. Add/remove values freely; every value here becomes its own
  # match_rate_at_X.X column plus the "lowest scorer" flag in the log uses
  # the smallest one as its bar (see near the bottom of main_analyze_item_coverage()).
  match_rate_thresholds = c(0.3, 0.4, 0.5, 0.6, 0.7),

  # If the loaded file spans more than one embedding_model (e.g. you
  # switched models between runs and some vacancies haven't been rescored
  # yet), mixing their similarity scores into one average would blend two
  # different, non-comparable scales. When TRUE (default), this is detected,
  # logged clearly, and the analysis auto-restricts to whichever model has
  # the most rows. Set analyze_specific_model to a model name to force a
  # particular one instead of the automatic most-common choice.
  warn_on_mixed_models = TRUE,
  analyze_specific_model = NULL,

  # --- Optional: restrict analysis to a specific subset of vacancies --------
  # Leave both NULL for the original behavior (every vacancy in the loaded
  # file). Two ways to specify a subset - vacancy_filter_ids takes priority
  # if both are set:
  #   - vacancy_filter_ids: a plain R vector, if you already have the list
  #     in hand (e.g. c("EXAMPLE1", "EXAMPLE2", ...)).
  #   - vacancy_filter_path + vacancy_filter_column: read the list from a
  #     column in an external CSV instead - e.g. point this at your own
  #     sampling crosswalk to restrict to a labelled subset of vacancies, or
  #     at a small CSV of just one label group, to compare which master
  #     items discriminate between groups - run this script once per
  #     subset, with a different output_suffix each time, and compare the
  #     resulting summaries.
  # If a filter is active, set output_suffix too - otherwise this run
  # silently overwrites the unfiltered summary/plot output (a loud warning
  # fires in main_analyze_item_coverage() if you forget).
  vacancy_filter_ids     = NULL,
  vacancy_filter_path     = NULL,
  vacancy_filter_column     = "vacancy_number",

  # Appended to summary_output_path/plot_output_path (before the extension)
  # so a filtered run doesn't overwrite the unfiltered corpus-wide summary,
  # or a previous filtered run's output. E.g. "labelgroup_a" ->
  # item_coverage_summary_labelgroup_a.csv. NULL = no suffix.
  output_suffix = NULL,

  # A SECOND, independent suffix axis from output_suffix above - which
  # MASTER TEMPLATE VERSION produced this file (e.g. "v2_variant"), rather
  # than which label group it was filtered to. Applied AFTER output_suffix
  # (see apply_version_suffix()), so both can be active on the same file at
  # once - e.g. item_coverage_summary_labelgroup_a_v2_variant.csv. Set
  # automatically by run_master_template_version.R to its own
  # CONFIG_VERSION$version_suffix when this script is run through that
  # wrapper; NULL (no suffix) for a plain standalone run.
  version_suffix = NULL,

  # Simple horizontal bar chart (base R graphics only, no extra packages)
  # of mean similarity per master item, sorted worst-to-best. Set to NULL
  # to skip.
  plot_output_path            = file.path("..", "Output", "item_coverage_plot.png"),
  validation_plot_output_path = file.path("..", "Validation_Output", "validation_item_coverage_plot.png"),

  log_to_file = TRUE,
  log_dir     = file.path("..", "Output", "logs"),
  validation_log_dir = file.path("..", "Validation_Output", "logs")
)

# ==============================================================================
# Logging (same tiny harness as similarity_scoring.R, duplicated to keep
# this script standalone)
# ==============================================================================
.log_con <- NULL
log_msg <- function(text) {
  message(text)
  if (!is.null(.log_con)) cat(text, "\n", file = .log_con, sep = "")
}
log_warn <- function(text) log_msg(paste("WARNING:", text))

setup_log_file <- function(config) {
  if (!isTRUE(config$log_to_file)) return(NULL)
  if (!dir.exists(config$log_dir)) dir.create(config$log_dir, recursive = TRUE)
  log_path <- file.path(config$log_dir, sprintf("item_coverage_run_log_%s.txt", format(Sys.time(), "%Y%m%d_%H%M%S")))
  con <- file(log_path, open = "at")
  message(sprintf("Logging this run to: %s", log_path))
  con
}

# ------------------------------------------------------------------------------
# Same path-swap pattern as similarity_scoring.R's resolve_paths_for_mode_similarity_scoring() -
# see the CONFIG$validation_mode comment above for the rationale. Also
# applies output_suffix and version_suffix (see CONFIG comments) to the two
# output paths, after the validation_mode swap so the concerns compose
# correctly - output_suffix (which label group) first, version_suffix
# (which master template version) last, so a file with both active reads
# e.g. item_coverage_summary_labelgroup_a_v2_variant.csv.
# ------------------------------------------------------------------------------
resolve_paths_for_mode_item_coverage <- function(config) {
  if (isTRUE(config$validation_mode)) {
    config$item_matches_path  <- config$validation_item_matches_path
    config$summary_output_path <- config$validation_summary_output_path
    config$plot_output_path      <- config$validation_plot_output_path
    config$log_dir                 <- config$validation_log_dir
  }
  config$summary_output_path <- apply_output_suffix(config$summary_output_path, config$output_suffix)
  config$plot_output_path     <- apply_output_suffix(config$plot_output_path, config$output_suffix)
  config$summary_output_path <- apply_version_suffix(config$summary_output_path, config$version_suffix)
  config$plot_output_path     <- apply_version_suffix(config$plot_output_path, config$version_suffix)
  config
}

apply_output_suffix <- function(path, suffix) {
  if (is.null(path) || is.null(suffix) || !nzchar(suffix)) return(path)
  ext  <- tools::file_ext(path)
  base <- tools::file_path_sans_ext(path)
  paste0(base, "_", suffix, if (nzchar(ext)) paste0(".", ext) else "")
}

# Same append-before-extension behavior as apply_output_suffix() above, kept
# as its own identically-implemented function (not a call-through to that
# one) rather than reusing that name - output_suffix and version_suffix are
# two genuinely different axes (which LABEL GROUP vs. which MASTER TEMPLATE
# VERSION this file belongs to) that can both be active on the same file at
# once, and giving them distinct names keeps each call site self-documenting
# about which axis it's suffixing. Duplicated verbatim (not sourced) across
# every script in the run_master_template_version.R chain that needs it.
# This is deliberate: each script stays standalone and runnable on its own,
# and duplicating an IDENTICAL implementation under the same name across
# scripts is safe (only a DIFFERING implementation under a shared name would
# risk a silent collision if two scripts were ever sourced together).
apply_version_suffix <- function(path, suffix) {
  if (is.null(path) || is.null(suffix) || !nzchar(suffix)) return(path)
  ext  <- tools::file_ext(path)
  base <- tools::file_path_sans_ext(path)
  paste0(base, "_", suffix, if (nzchar(ext)) paste0(".", ext) else "")
}

# ------------------------------------------------------------------------------
# Resolves CONFIG's vacancy-filter options (see CONFIG comment) into a plain
# character vector of vacancy_number(s), or NULL if no filter is configured.
# vacancy_filter_ids takes priority over vacancy_filter_path if both are set.
# ------------------------------------------------------------------------------
resolve_vacancy_filter <- function(config) {
  if (!is.null(config$vacancy_filter_ids)) {
    return(unique(as.character(config$vacancy_filter_ids)))
  }
  if (!is.null(config$vacancy_filter_path)) {
    if (!file.exists(config$vacancy_filter_path)) {
      stop(sprintf("vacancy_filter_path set but file not found: %s", config$vacancy_filter_path))
    }
    filt <- utils::read.csv(config$vacancy_filter_path, stringsAsFactors = FALSE, colClasses = "character", na.strings = character(0))
    if (!(config$vacancy_filter_column %in% names(filt))) {
      stop(sprintf(
        "vacancy_filter_column '%s' not found in %s. Columns present: %s",
        config$vacancy_filter_column, config$vacancy_filter_path, paste(names(filt), collapse = ", ")
      ))
    }
    ids <- unique(as.character(filt[[config$vacancy_filter_column]]))
    ids <- ids[nzchar(trimws(ids))]
    return(ids)
  }
  NULL
}

# ------------------------------------------------------------------------------
# Summary stats for ONE master item across every vacancy that was compared
# against it. `scores` is already-numeric (NA for vacancies where this
# master item had nothing eligible to compare against, per master_coverage's
# own NA handling in best_matches()) - an NA here means "couldn't be
# compared", never "compared and found zero similarity".
# ------------------------------------------------------------------------------
summarize_master_item <- function(section, item_id, item_text, scores, n_vacancies_total, thresholds) {
  valid <- scores[!is.na(scores)]
  n_compared <- length(valid)

  # Output columns (item_coverage_summary.csv), one row per master item:
  #   section / master_item_id / master_item_text  - which master criterion
  #   n_vacancies_total       - vacancies in scope for this run (all of them,
  #                            not just this item's comparable subset)
  #   n_vacancies_compared      - how many of those had something eligible
  #                              to compare this item against
  #   n_vacancies_no_content      - the complement of n_vacancies_compared
  #   mean_similarity / median_similarity /  - distribution of this item's
  #   min_similarity / max_similarity /        best-match similarity across
  #   sd_similarity                              the n_vacancies_compared
  #                                              vacancies; all NA if
  #                                              n_vacancies_compared is 0
  #   match_rate_at_X.X (one column per         - proportion of
  #   CONFIG$match_rate_thresholds entry)         n_vacancies_compared
  #                                                scoring >= that threshold
  out <- data.frame(
    section                 = section,
    master_item_id           = item_id,
    master_item_text          = item_text,
    n_vacancies_total          = n_vacancies_total,
    n_vacancies_compared         = n_compared,
    n_vacancies_no_content         = sum(is.na(scores)),
    mean_similarity                  = if (n_compared > 0) mean(valid) else NA_real_,
    median_similarity                 = if (n_compared > 0) stats::median(valid) else NA_real_,
    min_similarity                     = if (n_compared > 0) min(valid) else NA_real_,
    max_similarity                      = if (n_compared > 0) max(valid) else NA_real_,
    sd_similarity                        = if (n_compared > 1) stats::sd(valid) else NA_real_,
    stringsAsFactors = FALSE
  )
  for (t in thresholds) {
    out[[sprintf("match_rate_at_%.1f", t)]] <- if (n_compared > 0) mean(valid >= t) else NA_real_
  }
  out
}

# ==============================================================================
# Main
# ==============================================================================
main_analyze_item_coverage <- function(config) {
  config <- resolve_paths_for_mode_item_coverage(config)
  .log_con <<- setup_log_file(config)
  on.exit(if (!is.null(.log_con)) close(.log_con), add = TRUE)

  log_msg(sprintf("=== analyze_item_coverage.R run started: %s ===", Sys.time()))
  if (isTRUE(config$validation_mode)) {
    log_msg("*** VALIDATION MODE ACTIVE *** - analyzing the validation corpus's item matches.")
  }

  if (!file.exists(config$item_matches_path)) {
    stop(sprintf(
      "No item-match file found at '%s'. Run similarity_scoring.R with 'item' in granularity_modes first.",
      config$item_matches_path
    ))
  }

  df <- utils::read.csv(config$item_matches_path, stringsAsFactors = FALSE, colClasses = "character", na.strings = character(0))
  log_msg(sprintf("Loaded %d row(s) from %s (%d vacancies).", nrow(df), config$item_matches_path, length(unique(df$vacancy_number))))

  # --- Optional vacancy filter --------------------------------------------------
  vacancy_ids <- resolve_vacancy_filter(config)
  if (!is.null(vacancy_ids)) {
    all_vacancies_before <- unique(df$vacancy_number)
    df <- df[df$vacancy_number %in% vacancy_ids, , drop = FALSE]
    found <- unique(df$vacancy_number)
    missing <- setdiff(vacancy_ids, all_vacancies_before)
    log_msg(sprintf(
      "Vacancy filter active: %d of %d requested vacancy_number(s) found in the loaded file (%d row(s) remain).",
      length(found), length(vacancy_ids), nrow(df)
    ))
    if (length(missing) > 0) {
      log_warn(sprintf(
        "%d requested vacancy_number(s) not present in the loaded file: %s",
        length(missing), paste(utils::head(missing, 20), collapse = ", ")
      ))
    }
    if (nrow(df) == 0) stop("Vacancy filter matched zero rows - check vacancy_filter_path/column and that validation_mode points at the right file.")
    if (is.null(config$output_suffix)) {
      log_warn("A vacancy filter is active but CONFIG$output_suffix is NULL - this run will overwrite the unfiltered summary/plot output. Set output_suffix to avoid this.")
    }
  }

  # --- Model consistency check --------------------------------------------------
  model_counts <- sort(table(df$embedding_model), decreasing = TRUE)
  if (length(model_counts) > 1 && isTRUE(config$warn_on_mixed_models)) {
    log_warn(sprintf(
      "Multiple embedding models present in this file: %s. Mixing similarity scores across models blends non-comparable scales.",
      paste(sprintf("%s (%d rows)", names(model_counts), as.integer(model_counts)), collapse = ", ")
    ))
  }
  target_model <- if (!is.null(config$analyze_specific_model)) config$analyze_specific_model else names(model_counts)[1]
  if (length(model_counts) > 1 || !is.null(config$analyze_specific_model)) {
    n_before <- nrow(df)
    df <- df[df$embedding_model == target_model, , drop = FALSE]
    log_msg(sprintf(
      "Restricting analysis to embedding_model = '%s' (%d of %d rows). Override via CONFIG$analyze_specific_model.",
      target_model, nrow(df), n_before
    ))
  }

  # --- Filter to master_coverage direction --------------------------------------
  # The direction whose anchor item is a MASTER criterion - the one relevant
  # to "which master items get matched" (job_relevance/general_match rows
  # are anchored on JOB bullets instead, a different question).
  mc <- df[df$direction == "master_coverage", , drop = FALSE]
  if (nrow(mc) == 0) {
    stop("No master_coverage rows found in this file - was similarity_scoring.R run with 'item' in granularity_modes?")
  }
  mc$similarity_score <- suppressWarnings(as.numeric(mc$similarity_score))

  n_vacancies_total <- length(unique(df$vacancy_number))
  log_msg(sprintf("Aggregating master_coverage rows across %d vacancies.", n_vacancies_total))

  # --- One summary row per (section, master_item_id) ----------------------------
  keys <- unique(mc[, c("section", "anchor_item_id", "anchor_item_text")])
  keys <- keys[order(keys$section, keys$anchor_item_id), ]

  summary_rows <- lapply(seq_len(nrow(keys)), function(i) {
    item_rows <- mc[mc$section == keys$section[i] & mc$anchor_item_id == keys$anchor_item_id[i], , drop = FALSE]
    summarize_master_item(
      keys$section[i], keys$anchor_item_id[i], keys$anchor_item_text[i],
      item_rows$similarity_score, n_vacancies_total, config$match_rate_thresholds
    )
  })
  summary_df <- do.call(rbind, summary_rows)

  # Worst-covered first within each section - the most actionable ordering
  # for spotting items that might not belong in the template, or that need
  # rewording to match how real ads phrase things. NA (never comparable at
  # all) sorts first - even more worth a look than "compared but scored low".
  summary_df <- summary_df[order(summary_df$section, summary_df$mean_similarity, na.last = FALSE), ]

  dir.create(dirname(config$summary_output_path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(summary_df, config$summary_output_path, row.names = FALSE)
  log_msg(sprintf("Wrote %d-row summary to %s", nrow(summary_df), config$summary_output_path))

  low_bar <- min(config$match_rate_thresholds)
  never_compared <- summary_df[summary_df$n_vacancies_compared == 0, ]
  if (nrow(never_compared) > 0) {
    log_msg(sprintf(
      "Master item(s) with NOTHING to compare against in any vacancy (n_vacancies_compared = 0): %s",
      paste(never_compared$master_item_id, collapse = ", ")
    ))
  }
  low_scorers <- summary_df[!is.na(summary_df$mean_similarity) & summary_df$mean_similarity < low_bar, ]
  if (nrow(low_scorers) > 0) {
    log_msg(sprintf(
      "Master item(s) with mean similarity below %.1f across the vacancies compared: %s",
      low_bar, paste(low_scorers$master_item_id, collapse = ", ")
    ))
  }

  # --- Optional plot ---------------------------------------------------------
  if (!is.null(config$plot_output_path)) {
    tryCatch({
      dir.create(dirname(config$plot_output_path), recursive = TRUE, showWarnings = FALSE)
      plot_df <- summary_df[!is.na(summary_df$mean_similarity), ]
      if (nrow(plot_df) == 0) {
        log_warn("No items with a computable mean_similarity - skipping plot.")
      } else {
        ord <- order(plot_df$mean_similarity)
        grDevices::png(config$plot_output_path, width = 1000, height = max(400, 22 * nrow(plot_df)))
        op <- graphics::par(mar = c(4, 20, 3, 2))
        graphics::barplot(
          plot_df$mean_similarity[ord],
          names.arg = paste0(plot_df$master_item_id[ord], "  ", substr(plot_df$master_item_text[ord], 1, 50)),
          horiz = TRUE, las = 1, cex.names = 0.65,
          xlab = "Mean best-match similarity across vacancies",
          xlim = c(0, 1),
          main = sprintf(
            "Master-item coverage (%s, %d vacancies)",
            if (isTRUE(config$validation_mode)) "validation set" else "main study", n_vacancies_total
          ),
          col = ifelse(plot_df$section[ord] == "responsibilities", "#4C72B0", "#DD8452")
        )
        graphics::legend("bottomright", legend = c("responsibilities", "skills"), fill = c("#4C72B0", "#DD8452"), bty = "n")
        graphics::par(op)
        grDevices::dev.off()
        log_msg(sprintf("Saved coverage plot to %s", config$plot_output_path))
      }
    }, error = function(e) log_warn(sprintf("Could not save plot (%s) - continuing without it.", conditionMessage(e))))
  }

  log_msg("Done.")
  invisible(summary_df)
}

if (sys.nframe() == 0) {
  main_analyze_item_coverage(CONFIG)
}
