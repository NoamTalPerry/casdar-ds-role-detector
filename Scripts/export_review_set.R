#!/usr/bin/env Rscript
# =============================================================================
# export_review_set.R
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
#   This repository contains CODE ONLY. It does not include any scores file
#   or review set. To run this script yourself, first score your own corpus
#   with similarity_scoring.R ("section" in its granularity_modes).
#
# PURPOSE
#   Builds the manual-review set from a full-corpus similarity-scores file:
#   the union of a STRICT tier and a LENIENT tier, with vacancies that have no
#   skills score (NA) routed by their section score only. Written so the set
#   can be regenerated if the thresholds change.
#
# DEFAULT THRESHOLDS (the values used in this project)
#   strict tier  : score_overall at granularity "section" >= 0.7168
#                  (the recall floor on the DS-labelled validation positives)
#   lenient tier : score_skills  at granularity "section" >= 0.7141
#                  (the F1 peak for DS + Partial DS vs No-DS)
#   Both were derived with run_threshold_analysis.R for one master-template
#   version and one label set. They are specific to that template and those
#   labels: if either changes, re-derive the thresholds before regenerating
#   the set, and set your own values in CONFIG_REVIEW.
#
# NA RULE
#   A vacancy with NA score_skills cannot enter the lenient tier. It is
#   included only if it passes the strict tier. Recall note: NA-skills
#   vacancies below the strict threshold are therefore not reviewed; the
#   count of NA-skills vacancies is printed so this can be checked.
#
# USAGE
#   Run with the scripts folder as the working directory, edit CONFIG_REVIEW
#   if needed, then:
#     source("export_review_set.R"); main_export_review_set()
#   or: Rscript export_review_set.R
#   Base R only. Writes one CSV and prints the tier counts for checking.
#
# INPUT
#   A similarity-scores CSV in long format (as written by similarity_scoring.R)
#   with columns: vacancy_number, position_category, position_title,
#   granularity ("document"/"item"/"section"), score_overall, score_skills.
#
# OUTPUT (CONFIG_REVIEW$output_path)
#   vacancy_number, position_category, position_title, section_score,
#   section_skills_score ("NA" if missing), tier ("strict" / "lenient_only"),
#   strict_ge_<t>, lenient_ge_<t> flags, skills_NA_routed_by_section.
#   Sorted strict first, then by descending section score.
#
# CAVEATS
#   - Thresholds derived from a small labelled sample are estimates; a recall
#     floor in particular is set by the single lowest-scoring positive.
# =============================================================================

CONFIG_REVIEW <- list(
  corpus_scores_path = file.path("..", "Output", "similarity_scores.csv"),
  output_path        = file.path("..", "Output", "review_set.csv"),
  granularity        = "section",
  strict_col         = "score_overall",
  strict_threshold   = 0.7168,
  lenient_col        = "score_skills",
  lenient_threshold  = 0.7141,
  
  # Optional sanity check: expected tier counts, e.g.
  # c(strict = 97, lenient = 139, union = 155). A WARNING is printed for any
  # count that differs (a wrong input file or changed thresholds). NULL = off.
  expected_counts    = NULL
)

main_export_review_set <- function(config = CONFIG_REVIEW) {
  if (!file.exists(config$corpus_scores_path)) {
    stop("Scores file not found: ", config$corpus_scores_path,
         " (run from the scripts folder, or edit CONFIG_REVIEW).", call. = FALSE)
  }
  d <- read.csv(config$corpus_scores_path, stringsAsFactors = FALSE,
                check.names = FALSE, colClasses = c(vacancy_number = "character"))
  need <- c("vacancy_number", "position_category", "position_title",
            "granularity", config$strict_col, config$lenient_col)
  miss <- setdiff(need, names(d))
  if (length(miss)) stop("Missing columns in scores file: ", paste(miss, collapse = ", "), call. = FALSE)
  
  d <- d[d$granularity == config$granularity, , drop = FALSE]
  if (anyDuplicated(d$vacancy_number)) {
    stop("Duplicate vacancy_number at granularity '", config$granularity, "'.", call. = FALSE)
  }
  
  s_strict  <- suppressWarnings(as.numeric(d[[config$strict_col]]))
  s_lenient <- suppressWarnings(as.numeric(d[[config$lenient_col]]))
  
  in_strict  <- !is.na(s_strict)  & s_strict  >= config$strict_threshold
  in_lenient <- !is.na(s_lenient) & s_lenient >= config$lenient_threshold
  keep <- in_strict | in_lenient
  
  out <- data.frame(
    vacancy_number       = d$vacancy_number[keep],
    position_category    = d$position_category[keep],
    position_title       = d$position_title[keep],
    section_score        = s_strict[keep],
    section_skills_score = ifelse(is.na(s_lenient[keep]), "NA", s_lenient[keep]),
    tier                 = ifelse(in_strict[keep], "strict", "lenient_only"),
    stringsAsFactors = FALSE
  )
  out[[paste0("strict_ge_", config$strict_threshold)]]   <- as.integer(in_strict[keep])
  out[[paste0("lenient_ge_", config$lenient_threshold)]] <- as.integer(in_lenient[keep])
  out$skills_NA_routed_by_section <- as.integer(is.na(s_lenient[keep]))
  
  out <- out[order(out$tier != "strict", -out$section_score), , drop = FALSE]
  
  counts <- c(strict = sum(in_strict), lenient = sum(in_lenient), union = sum(keep))
  message("Vacancies at granularity '", config$granularity, "': ", nrow(d))
  message("Counts  strict=", counts[["strict"]], "  lenient=", counts[["lenient"]],
          "  union=", counts[["union"]],
          "  lenient-only=", sum(keep & !in_strict),
          "  NA skills=", sum(is.na(s_lenient)),
          "  NA-skills in set=", sum(is.na(s_lenient) & keep))
  if (!is.null(config$expected_counts)) {
    for (nm in intersect(names(config$expected_counts), names(counts))) {
      if (counts[[nm]] != config$expected_counts[[nm]]) {
        message("WARNING: ", nm, " count is ", counts[[nm]], ", expected ", config$expected_counts[[nm]],
                " - check the scores file and thresholds.")
      }
    }
  }
  
  dir.create(dirname(config$output_path), recursive = TRUE, showWarnings = FALSE)
  write.csv(out, config$output_path, row.names = FALSE)
  message("Wrote ", nrow(out), " rows to ", config$output_path)
  invisible(out)
}

if (sys.nframe() == 0) {
  main_export_review_set(CONFIG_REVIEW)
}