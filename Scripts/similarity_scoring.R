#!/usr/bin/env Rscript
# ==============================================================================
# similarity_scoring.R  ("Script 2")
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
#   Take the merged per-vacancy criteria text produced by
#   extract_job_criteria.R ("Script 1") and score how similar each vacancy's
#   RESPONSIBILITIES / SKILLS content is to a master template of data-steward
#   criteria, using a LOCAL embedding model (via Ollama) - not the Claude API.
#   Results are written to Output/similarity_scores.csv.
#
# Data scope (what this repository does and does not include):
#   This repository contains CODE ONLY. It does not include job-ad PDFs, the
#   merged per-vacancy text files Script 1 produces, HR extract files, the
#   master template of data-steward criteria, or any scored output. The
#   current master template (v5) is deposited separately as a research
#   instrument - see this repository's README for where to find it. To run
#   this script yourself, supply your own master_template.txt (see
#   parse_master_template() below for the required format) and your own
#   merged job-ad text files (produced by extract_job_criteria.R, Script 1).
#
# Relationship to Script 1:
#   This script is a pure CONSUMER of Script 1's output (vacancy_status.csv +
#   TXTs_merged/*.txt) and never touches PDFs, TXTs/, or the extraction
#   pipeline. See extract_job_criteria.R's write_merged_file() for the exact
#   format this script parses against.
#
# Design principle carried over from Script 1:
#   bias toward RECALL over precision throughout. Concretely here, that means:
#     - never silently truncate long text sent to the embedding model without
#       flagging it (see embed_context_warn_chars below)
#     - a section with genuinely nothing to compare produces NA, not 0 - a
#       missing/uncomparable score should never look identical to "compared
#       and found no similarity"
#     - no fuzzy/aggressive filtering added without flagging the tradeoff
#       first (there isn't any of that in this script - matching is plain
#       cosine similarity throughout, nothing heuristic)
#
# Usage:
#   Rscript similarity_scoring.R
#   Or edit the CONFIG block below and run interactively.
#
# Requirements:
#   install.packages(c("httr2", "jsonlite", "yaml", "digest"))
#   Ollama (a local embedding-model server, separate from R):
#     1. Install from https://ollama.com (one-time, not an R package)
#     2. Start it (runs automatically after install, or `ollama serve`)
#     3. Pull the embedding model once: `ollama pull nomic-embed-text`
#   This script assumes Ollama is reachable at http://localhost:11434 (the
#   default). If Ollama isn't running, every embedding call will fail with a
#   clear log message telling you so - nothing will silently produce zeros.
#
# External config file (see CONFIG$external_config_path below):
#   A few knobs you're likely to want to flip without touching this file are
#   also readable from a small YAML file alongside this script (default:
#   similarity_scoring_config.yml). See load_external_config() for exactly
#   which keys it recognizes. Not required to exist - if absent, the CONFIG
#   defaults below apply and this is logged, not silent.
# ==============================================================================

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(digest)
})

# ------------------------------------------------------------------------------
# CONFIG - edit these defaults if you prefer not to use the external YAML file
# ------------------------------------------------------------------------------
CONFIG <- list(
  # --- Validation mode ------------------------------------------------------------
  # Set TRUE to score the validation corpus (curated external job ads, outside the main corpus,
  # used for threshold-setting/sanity-checking) instead of the main study
  # corpus. When TRUE, EVERY input/output path below is automatically
  # swapped for its validation_* counterpart (see resolve_paths_for_mode_similarity_scoring())
  # - main-study Output/ files are never touched by a validation run, and
  # vice versa. Scoring BEHAVIOUR (granularity_modes, master template,
  # filters, GENERAL-pool settings, etc.) is deliberately NOT duplicated
  # per-mode - validation is only meaningful if it exercises the exact same
  # configuration that scores the real study data, not a separately-tuned
  # copy of it. Toggle this (including via the external YAML config) rather
  # than maintaining a second script.
  validation_mode = FALSE,
  
  # --- Script 1 input paths (the input contract produced by extract_job_criteria.R) -
  status_table_path = file.path("..", "Output", "vacancy_status.csv"),
  merged_dir         = file.path("..", "TXTs_merged"),
  
  # --- Validation corpus input paths (used only when validation_mode = TRUE) ----
  validation_status_table_path = file.path("..", "Validation_Output", "validation_status.csv"),
  validation_merged_dir         = file.path("..", "Validation_TXTs_merged"),
  
  # --- Master template --------------------------------------------------------
  # Plain-text, itemized, three sections - see parse_master_template() for the
  # exact expected format and MASTER_TEMPLATE_FORMAT_EXAMPLE below for a
  # ready-to-copy stub. Lives under Data/ alongside the other reference files
  # Script 1 already reads from there (ras_fp_files.csv etc.). Deliberately
  # NOT duplicated for validation_mode - see the validation_mode comment above.
  master_template_path = file.path("..", "Data", "master_template.txt"),
  
  # --- Output ------------------------------------------------------------------
  # A SEPARATE file from vacancy_status.csv, not new columns on it - this
  # script is expected to be re-run repeatedly while granularity modes/models
  # are compared empirically, and vacancy_status.csv is the master index for
  # the whole pipeline, not a scratchpad for one experiment's output.
  # One row per (vacancy_number, granularity) pair - see upsert_similarity_scores().
  scores_path = file.path("..", "Output", "similarity_scores.csv"),
  validation_scores_path = file.path("..", "Validation_Output", "validation_similarity_scores.csv"),
  
  # Per-bullet audit detail, item-level mode only - one row per matched item,
  # BOTH directions (job_relevance and master_coverage - see best_matches()).
  # Not columns on scores_path since the master's ~20-30 bullets would make
  # for a fragile, ever-changing column count; long format instead. Written
  # only when "item" is in granularity_modes and this stays TRUE.
  item_matches_path = file.path("..", "Output", "similarity_item_matches.csv"),
  validation_item_matches_path = file.path("..", "Validation_Output", "validation_similarity_item_matches.csv"),
  write_item_match_detail = TRUE,
  
  # Appended to scores_path/item_matches_path (before the extension) via
  # apply_version_suffix() - e.g. "v2_adjacent_role" ->
  # similarity_scores_v2_adjacent_role.csv. Same mechanism/semantics as
  # every other script in the run_master_template_version.R chain (see
  # analyze_item_coverage.R's version_suffix comment for the full
  # rationale). Set automatically by run_master_template_version.R to its
  # own CONFIG_VERSION$version_suffix; NULL (no suffix) for a plain
  # standalone run - this is what lets that wrapper hand this script a
  # plain base filename and let the suffixing happen here, the same way it
  # happens in every other script in the chain, rather than the wrapper
  # building the suffixed filename itself.
  #
  # Deliberately NOT applied to cache_path - the embedding cache is
  # shared/unversioned across template versions by design (most text is
  # unchanged between versions, so reuse avoids redundant embedding - see
  # run_master_template_version.R's header comment).
  version_suffix = NULL,
  
  # --- Embedding model (local, via Ollama) -------------------------------------
  # nomic-embed-text: 137M params, 8192-token context, English-focused, runs
  # fine on CPU. Chosen over bge-m3 (heavier, multilingual - wasted capacity
  # for English-only ads) and all-minilm (only 256-token context - would
  # silently truncate a merged multi-posting document at document/section
  # granularity, a direct recall violation for this project).
  ollama_model      = "nomic-embed-text",
  ollama_api_url    = "http://localhost:11434/api/embed",
  ollama_tags_url   = "http://localhost:11434/api/tags",   # used once per run to look up the model's content digest - see get_ollama_model_digest()
  ollama_batch_size = 32,     # texts per HTTP request when embedding a fresh (uncached) batch
  
  # Soft truncation-risk check: an approximate characters-per-token ratio
  # (~4 for English) used only to WARN, never to actually truncate anything
  # ourselves - Ollama does its own truncation silently if a text exceeds the
  # model's context window, and this project's recall bias means that risk
  # must be surfaced, not hidden. Threshold set conservatively below
  # nomic-embed-text's real 8192-token limit to leave margin for the
  # 4-chars/token estimate being wrong. Set to NULL to disable the check.
  embed_context_warn_chars = 24000,   # ~6000 tokens at the 4 chars/token estimate
  
  # --- Granularity modes --------------------------------------------------------
  # All three scoring approaches are implemented (see compute_score_document(),
  # compute_score_section(), compute_score_item()) - this just controls which
  # ones actually RUN and get written to scores_path. Also overridable via the
  # external YAML config file without editing this script - see
  # load_external_config(). Valid values: "document", "section", "item".
  granularity_modes = c("section", "document", "item"),
  
  # --- Row filtering (which vacancy_status.csv rows get scored) ---------------
  # Skip "failed" rows, process "complete"/"partial", and additionally skip
  # any row with nothing extracted in either section regardless of status
  # label (n_responsibilities == 0 AND n_skills == 0).
  scoreable_statuses = c("complete", "partial"),
  
  # flagged_for_review == TRUE means Script 1 found low repost-overlap across
  # a group's postings (worth a human check on GROUPING) - not a comment on
  # extraction quality. Skipped by default out of caution; also settable
  # from the external YAML config file without editing this script.
  process_flagged_for_review = FALSE,
  
  # For small-scale test runs before scoring the full corpus: if non-empty,
  # restricts this run to ONLY these vacancy_numbers - a pure ADDITIONAL
  # restriction on top of the filters above, not a bypass of them (e.g.
  # listing a flagged/failed vacancy_number here still gets it filtered out
  # unless process_flagged_for_review/status also allow it through). Leave
  # empty (character(0)) for a normal full run.
  only_vacancy_numbers = character(0),   # e.g. c("EXAMPLE1", "EXAMPLE2", "EXAMPLE3")
  
  # If the master template has an optional GENERAL section (see
  # parse_master_template()), should job RESPONSIBILITY bullets also be
  # checked against it, or just skills? Default FALSE (skills only), so no
  # responsibility bullet is ever excluded from scoring by the GENERAL pool.
  # Trade-off if set TRUE: several items in a typical GENERAL synthesis read
  # as duty-framed rather than skill-framed ("general line-management...
  # responsibility", "represent the organisation at external events") and
  # job ads don't consistently sort generic content into the same section
  # the master's synthesis used, so a skills-only check can leave generic
  # responsibility bullets counted towards score_responsibilities.
  apply_general_pool_to_responsibilities = FALSE,
  
  # If the master template has an optional ADJACENT_ROLE section (same
  # format as GENERAL, see parse_master_template()), should it be used at
  # all this run? Default FALSE - the whole feature is inert unless
  # explicitly turned on, so a template that happens to have this section
  # present costs nothing extra (no embeddings computed, no classification
  # pass run) until you deliberately flip this for an ablation comparison.
  # When TRUE: works exactly like GENERAL, but as a SECOND, independent
  # classification stage applied to whatever survives the GENERAL stage
  # first - a job item classified ADJACENT is excluded from
  # score_responsibilities/score_skills (and therefore document/section
  # pooling too, same propagation as GENERAL) and from being an eligible
  # master_coverage match, exactly the same way a GENERAL-classified item
  # already is. Kept as a genuinely separate pool/stage from GENERAL (not
  # merged into it) specifically so its effect can be measured in
  # isolation - "general" would have been the wrong label for this
  # content, since it captures role-adjacent duties (e.g. data engineer/
  # scientist/PM-adjacent work) rather than generic/soft-skill content.
  use_adjacent_pool = FALSE,
  
  # Same trade-off and same default as apply_general_pool_to_responsibilities
  # above: skills are always checked against
  # ADJACENT_ROLE when use_adjacent_pool is TRUE; responsibilities only if
  # this is also TRUE. Has no effect when use_adjacent_pool is FALSE.
  apply_adjacent_pool_to_responsibilities = FALSE,
  
  # --- Re-run behaviour ---------------------------------------------------------
  # If TRUE, a vacancy_number+granularity combination already present in
  # scores_path is skipped (no re-embedding). Set FALSE, or use
  # force_rescore_vacancy_numbers below for a targeted subset, to overwrite.
  skip_existing_scores = TRUE,
  force_rescore_vacancy_numbers = character(0),   # e.g. c("EXAMPLE1", "EXAMPLE2") - always
  # recomputed even if skip_existing_scores is TRUE, for targeted re-checks
  
  # If TRUE (default), skip_existing_scores also requires the existing row's
  # embedding_model (and, when both sides have one recorded, its
  # embedding_model_digest) to match the CURRENT run's model before treating
  # a row as "already done" - a model swap, or a silent weight update behind
  # the same model name, triggers an automatic rescore of the affected rows
  # rather than silently mixing scores from two different models in one
  # file. Every mismatch that triggers a rescore this way is logged. Set
  # FALSE to go back to matching on vacancy_number+granularity alone.
  require_model_match_for_skip = TRUE,
  
  # --- Crash safety / progress ---------------------------------------------------
  # Every this-many vacancies (fully processed, i.e. all their active
  # granularity modes attempted), write accumulated scores/match-detail rows
  # to disk and save the embedding cache, then clear the in-memory buffers -
  # so a crash partway through a large corpus only loses work back to the
  # last checkpoint, not the entire run. Smaller = safer but more disk I/O
  # (each checkpoint re-reads and rewrites the growing output CSVs, an
  # unavoidable cost of the upsert/dedupe logic); larger = less I/O but more
  # at risk in a crash. A final flush always happens at the end of the run
  # regardless of this setting, so nothing is lost from a clean finish.
  checkpoint_every_n_vacancies = 25,
  
  # Log a running progress line ("N/TOTAL vacancies, elapsed, est.
  # remaining") every this-many vacancies. 1 = every vacancy (the default -
  # each one usually involves real network-bound work, so this isn't overly
  # noisy). Set higher to reduce console/log volume on a very large corpus.
  progress_report_every_n = 1,
  
  # --- Embedding cache -----------------------------------------------------------
  # Local embedding is deterministic and cheap, but this script is typically
  # re-run many times while comparing granularity modes on the same ground-
  # truth set. Caching avoids re-embedding identical text (keyed on
  # model+text, so a model change or template edit is automatically NOT
  # served stale) across those repeated runs.
  cache_path = file.path("..", "Output", "embedding_cache.rds"),
  validation_cache_path = file.path("..", "Validation_Output", "validation_embedding_cache.rds"),
  
  # --- Diagnostics ---------------------------------------------------------------
  # If TRUE, save the exact pooled text sent to the embedding model per
  # vacancy/section/granularity to a debug folder - lets you inspect
  # precisely what was compared, same spirit as Script 1's
  # save_extracted_text. Off by default since it's a lot of small files at
  # scale; turn on when spot-checking a handful of vacancies.
  save_pooled_text     = FALSE,
  pooled_text_dir      = file.path("..", "Output", "similarity_pooled_text"),
  validation_pooled_text_dir = file.path("..", "Validation_Output", "similarity_pooled_text"),
  
  call_embeddings = TRUE,   # if FALSE, parse everything and log what WOULD be
  # sent/compared but skip all Ollama calls - mirrors Script 1's call_llm,
  # useful for validating parsing against ground truth before spending any
  # embedding compute (embedding is local/free, but this is still useful for
  # fast parser iteration without waiting on a live Ollama server).
  
  # --- Logging -------------------------------------------------------------------
  # In their own subfolder, not loose in Output/ - a long-running corpus
  # scoring job produces a lot of these, and nothing downstream (any other
  # script in this pipeline) reads a log file by path, so moving them here
  # is safe with respect to everything else.
  log_to_file = TRUE,
  log_dir     = file.path("..", "Output", "logs"),
  validation_log_dir = file.path("..", "Validation_Output", "logs"),
  
  # --- External config file -------------------------------------------------------
  external_config_path = "similarity_scoring_config.yml"
)

# ==============================================================================
# External YAML config overlay
# ==============================================================================
# Reads a small external file (if present) and overrides ONLY the recognized
# keys below onto CONFIG - everything else stays as set in the block above.
# This is intentionally narrow: it's for the handful of knobs you'll want to
# flip on the fly (which flags/statuses to score, which granularity modes to
# run), not a general escape hatch for every setting - paths, model choice,
# etc. stay in this file, matching Script 1's convention of one CONFIG block
# as the source of truth for anything structural.
#
# Example similarity_scoring_config.yml:
#   process_flagged_for_review: true
#   granularity_modes:
#     - section
#     - document
#     - item
#   skip_existing_scores: false
# ------------------------------------------------------------------------------
load_external_config <- function(config, path) {
  if (!file.exists(path)) {
    message(sprintf(
      "No external config file found at '%s' - using CONFIG defaults from this script.",
      path
    ))
    return(config)
  }
  if (!requireNamespace("yaml", quietly = TRUE)) {
    message(sprintf(
      "WARNING: '%s' exists but the 'yaml' package isn't installed - install.packages('yaml') to use it. Using CONFIG defaults from this script for now.",
      path
    ))
    return(config)
  }
  user_cfg <- yaml::read_yaml(path)
  recognized <- c(
    "process_flagged_for_review", "granularity_modes", "skip_existing_scores",
    "force_rescore_vacancy_numbers", "scoreable_statuses", "call_embeddings",
    "require_model_match_for_skip", "only_vacancy_numbers",
    "apply_general_pool_to_responsibilities",
    "use_adjacent_pool", "apply_adjacent_pool_to_responsibilities",
    "checkpoint_every_n_vacancies", "progress_report_every_n",
    "validation_mode", "master_template_path"
  )
  unknown <- setdiff(names(user_cfg), recognized)
  if (length(unknown) > 0) {
    message(sprintf(
      "WARNING: '%s' has key(s) this script doesn't recognize and will ignore: %s",
      path, paste(unknown, collapse = ", ")
    ))
  }
  applied <- intersect(names(user_cfg), recognized)
  for (key in applied) {
    config[[key]] <- user_cfg[[key]]
  }
  if (length(applied) > 0) {
    message(sprintf("Loaded external config overrides from '%s': %s", path, paste(applied, collapse = ", ")))
  }
  config
}
CONFIG <- load_external_config(CONFIG, CONFIG$external_config_path)

# ==============================================================================
# Logging (identical harness to Script 1, duplicated here to keep this script
# standalone rather than sourcing extract_job_criteria.R)
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
  log_path <- file.path(config$log_dir, sprintf("similarity_run_log_%s.txt", format(Sys.time(), "%Y%m%d_%H%M%S")))
  con <- file(log_path, open = "at")
  message(sprintf("Logging this run to: %s", log_path))
  con
}

# Same append-before-extension behavior duplicated identically across every
# script in the run_master_template_version.R chain (analyze_item_coverage.R,
# run_threshold_analysis.R, triangulate_item_coverage.R). Deliberately kept
# as identical duplicated implementations (rather than one shared function
# sourced by all of them) so each script stays standalone and runnable on
# its own - safe to duplicate a name across scripts this way only because
# every copy is guaranteed identical; a function whose behaviour genuinely
# differs per script instead gets a script-specific name (e.g.
# resolve_paths_for_mode_similarity_scoring() below) to avoid a silent
# collision if two scripts are ever sourced together. NULL/empty suffix is
# a no-op.
apply_version_suffix <- function(path, suffix) {
  if (is.null(path) || is.null(suffix) || !nzchar(suffix)) return(path)
  ext  <- tools::file_ext(path)
  base <- tools::file_path_sans_ext(path)
  paste0(base, "_", suffix, if (nzchar(ext)) paste0(".", ext) else "")
}

# ==============================================================================
# Parsing: master template
# ==============================================================================
# Expected format (plain text, UTF-8, one criterion per line, three required
# section headers plus one OPTIONAL fourth - see below). Each line's bullet
# marker can be a dash, asterisk, or number ("- ", "* ", "1.", "1)", tab or
# space after all of these are fine) - whatever you pasted in from your
# source document; the marker itself is stripped and never reaches the
# embedding model:
#
#   RESPONSIBILITIES
#   ----------------
#   1. <criterion>
#   2. <criterion>
#
#   SKILLS - ESSENTIAL
#   -------------------
#   - <criterion>
#
#   SKILLS - DESIRABLE
#   -------------------
#   - <criterion>
#
#   GENERAL
#   -------
#   - <generic/non-data-steward-specific item>
#
# RESPONSIBILITIES / SKILLS - ESSENTIAL / SKILLS - DESIRABLE are REQUIRED
# (each header alone on its own line, exact text) - this errors loudly
# rather than silently scoring against a partial template if one is
# missing/misspelled, since master-template completeness isn't something
# later parts of this script can sanity-check for you.
#
# GENERAL is OPTIONAL. If present, it's a pool of generic/soft-skill or
# otherwise non-data-steward-specific content (e.g. "excellent written and
# verbal communication skills") synthesized the same way as the other
# sections but kept structurally separate. Its purpose: for each job item
# (skills always; responsibilities too if CONFIG$apply_general_pool_to_
# responsibilities is TRUE), compare it against BOTH the relevant core pool
# and this GENERAL pool, and if GENERAL wins (higher similarity), the item
# is treated as generic content and EXCLUDED from score_responsibilities/
# score_skills entirely - not scored as 0, simply not counted - and
# excluded from the candidate pool for the master_coverage direction too.
# Every item's classification is still recorded in
# similarity_item_matches.csv (see CONFIG$write_item_match_detail) for
# audit regardless of which way it was classified. If GENERAL is absent,
# every item classifies as core and behavior is identical to not having
# this feature at all - it costs nothing when unused.
#
# ADJACENT_ROLE is OPTIONAL, same format, and inert unless CONFIG$use_
# adjacent_pool is TRUE (unlike GENERAL, which applies automatically to
# skills whenever the section is present). Purpose: content that reads as
# data-steward-adjacent on its face but was judged to more often capture
# OTHER specific roles (e.g. data engineer/scientist/PM-adjacent duties)
# rather than being generic filler the way GENERAL content is - kept as a
# genuinely separate pool/stage from GENERAL so its effect can be measured
# in isolation. Applied as a SECOND classification pass, on top of
# whatever survives the GENERAL stage - see CONFIG$use_adjacent_pool and
# classify_job_items() for the mechanics.
#
# Essential and desirable are parsed separately (kept in the return value for
# traceability/diagnostics) but MERGED into one skills pool for actual
# matching - mirrors Script 1's merged job-ad SKILLS section, which does not
# reliably distinguish essential/desirable across all ads either.
# ------------------------------------------------------------------------------
parse_master_template <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf(
      "Master template not found at '%s'. Create it before running - see the header comment in this script (parse_master_template()) for the exact expected format, or copy MASTER_TEMPLATE_FORMAT_EXAMPLE (printed by print_master_template_example()).",
      path
    ))
  }
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  
  find_marker <- function(pattern) which(grepl(pattern, lines, perl = TRUE))
  resp_start <- find_marker("^RESPONSIBILITIES\\s*$")
  ess_start  <- find_marker("^SKILLS\\s*-\\s*ESSENTIAL\\s*$")
  des_start  <- find_marker("^SKILLS\\s*-\\s*DESIRABLE\\s*$")
  gen_start  <- find_marker("^GENERAL\\s*$")   # optional - absence is fine, see comment above
  adj_start  <- find_marker("^ADJACENT_ROLE\\s*$")   # optional, same format/rationale as GENERAL - see CONFIG$use_adjacent_pool
  
  missing <- c(
    if (length(resp_start) == 0) "'RESPONSIBILITIES'",
    if (length(ess_start) == 0) "'SKILLS - ESSENTIAL'",
    if (length(des_start) == 0) "'SKILLS - DESIRABLE'"
  )
  if (length(missing) > 0) {
    stop(sprintf(
      "Master template at '%s' is missing required section header(s): %s. Each must appear alone on its own line, exact text.",
      path, paste(missing, collapse = ", ")
    ))
  }
  
  markers <- sort(c(
    resp_start[1], ess_start[1], des_start[1],
    if (length(gen_start) > 0) gen_start[1] else NULL,
    if (length(adj_start) > 0) adj_start[1] else NULL,
    length(lines) + 1L
  ))
  extract_block <- function(start_marker) {
    block_end <- min(markers[markers > start_marker]) - 1
    if (block_end < start_marker + 2) return(character(0))
    block <- lines[(start_marker + 2):block_end]   # +2 skips the header line itself and its dashed underline
    block <- block[nzchar(trimws(block))]
    block <- block[block != "(none extracted)"]
    block <- trimws(block, which = "left")
    # Master template is hand-authored, so its bullet style is whatever you
    # pasted from - strip a leading "- ", "* ", "1. ", "1)\t" etc. (any of
    # these, common list conventions) so the marker itself never becomes
    # part of the text sent to the embedding model. Script 1's own merged
    # job-ad output always uses "- " (machine-generated, single convention),
    # so that parser (parse_merged_job_text() below) doesn't need this.
    sub("^([-*]|[0-9]+[.)])\\s*", "", block)
  }
  
  responsibilities <- extract_block(resp_start[1])
  skills_essential <- extract_block(ess_start[1])
  skills_desirable <- extract_block(des_start[1])
  general           <- if (length(gen_start) > 0) extract_block(gen_start[1]) else character(0)
  adjacent           <- if (length(adj_start) > 0) extract_block(adj_start[1]) else character(0)
  
  if (length(responsibilities) == 0) {
    log_warn(sprintf("Master template's RESPONSIBILITIES section parsed as empty (%s) - responsibilities scoring will be NA for every vacancy until this is fixed.", path))
  }
  if (length(skills_essential) == 0 && length(skills_desirable) == 0) {
    log_warn(sprintf("Master template's SKILLS sections (essential + desirable) both parsed as empty (%s) - skills scoring will be NA for every vacancy until this is fixed.", path))
  }
  if (length(gen_start) > 0 && length(general) == 0) {
    log_warn(sprintf("Master template has a GENERAL header (%s) but it parsed as empty - no items will ever be excluded as generic this run.", path))
  }
  if (length(adj_start) > 0 && length(adjacent) == 0) {
    log_warn(sprintf("Master template has an ADJACENT_ROLE header (%s) but it parsed as empty - no items will ever be excluded via that pool this run, even if CONFIG$use_adjacent_pool is TRUE.", path))
  }
  
  list(
    responsibilities = responsibilities,
    skills_essential = skills_essential,
    skills_desirable = skills_desirable,
    skills           = c(skills_essential, skills_desirable),   # merged pool used for actual matching
    general           = general,   # optional generic/non-DS-specific pool - character(0) if the template has no GENERAL section
    adjacent           = adjacent   # optional cross-role-overlap pool - character(0) if the template has no ADJACENT_ROLE section; see CONFIG$use_adjacent_pool
  )
}

print_master_template_example <- function() {
  cat(MASTER_TEMPLATE_FORMAT_EXAMPLE, sep = "\n")
}

MASTER_TEMPLATE_FORMAT_EXAMPLE <- c(
  "RESPONSIBILITIES",
  "----------------",
  "- Develop and maintain data management plans for research projects",
  "- Advise researchers on FAIR data principles and best practice",
  "",
  "SKILLS - ESSENTIAL",
  "-------------------",
  "- Experience working with research data in an academic or research setting",
  "",
  "SKILLS - DESIRABLE",
  "-------------------",
  "- Familiarity with electronic lab notebooks or data repositories",
  "",
  "GENERAL",
  "-------",
  "- Excellent written and verbal communication skills",
  "",
  "ADJACENT_ROLE",
  "-------------",
  "- Build and maintain APIs, automation scripts, or data pipelines"
)

# ==============================================================================
# Parsing: Script 1's merged per-vacancy .txt output
# ==============================================================================
# Mirrors extract_job_criteria.R's own parse_criteria_txt() marker-search
# logic (locate the "RESPONSIBILITIES" and "^SKILLS" lines, take everything
# between/after them) - this is deliberately format-agnostic about whatever
# header lines (Vacancy group / Model / Source postings / Overlap score /
# NOTE) appear ABOVE "RESPONSIBILITIES", since those are never included in
# the body slice to begin with. Verified against the exact merged-file
# format written by extract_job_criteria.R's write_merged_file().
# ------------------------------------------------------------------------------
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

# ------------------------------------------------------------------------------
# Helper: the merged-file path for a vacancy_status.csv row. Deliberately
# just string-pastes vacancy_number straight into the pattern with NO
# special-casing for numeric-vs-singleton rows - the merged filename
# produced by extract_job_criteria.R's write_merged_file() is LITERALLY
# "vacancy_<vacancy_number>_MERGED.txt" in every case (vacancy_number
# already contains the right string either way), so re-deriving anything
# from the filename here would risk silently diverging from how Script 1
# actually names the file.
# ------------------------------------------------------------------------------
merged_file_path <- function(vacancy_number, config) {
  file.path(config$merged_dir, paste0("vacancy_", vacancy_number, "_MERGED.txt"))
}

# ------------------------------------------------------------------------------
# Model digest (content hash) lookup - NOT the same guarantee as
# CONFIG$ollama_model. The model NAME ("nomic-embed-text") is a label Ollama
# lets you re-pull at any time; if the model's publisher ever pushes updated
# weights under that same name, the label stays identical while the actual
# function computed changes underneath it. The digest (a content hash of the
# model blob, from `ollama list`/`/api/tags`) changes whenever the weights
# actually change, so recording it alongside embedding_model in every output
# row lets a later run PROVE whether two batches of scores really came from
# the same model, rather than just trusting a name that could have silently
# drifted.
#
# Parsing is split from the HTTP call (extract_model_digest()) so the
# parsing logic itself can be unit tested against a fabricated response body
# without needing a live Ollama server.
# ------------------------------------------------------------------------------
extract_model_digest <- function(tags_body, model_name) {
  models <- tags_body$models
  if (is.null(models) || length(models) == 0) return(NA_character_)
  names_vec  <- vapply(models, function(m) m$name  %||% NA_character_, character(1))
  model_vec  <- vapply(models, function(m) m$model %||% NA_character_, character(1))
  digest_vec <- vapply(models, function(m) m$digest %||% NA_character_, character(1))
  # Ollama tags are usually "name:tag" (e.g. "nomic-embed-text:latest") even
  # when CONFIG$ollama_model is just "nomic-embed-text" - match on exact name
  # first, then an exact "model" field match, then a "name:" prefix.
  match_idx <- which(
    names_vec == model_name |
      model_vec == model_name |
      startsWith(names_vec, paste0(model_name, ":"))
  )
  if (length(match_idx) == 0) return(NA_character_)
  digest_vec[match_idx[1]]
}

get_ollama_model_digest <- function(config) {
  resp <- tryCatch(request(config$ollama_tags_url) |> req_perform(), error = function(e) e)
  if (inherits(resp, "error")) {
    log_warn(paste(
      "Could not reach Ollama's /api/tags to record the model digest:", conditionMessage(resp),
      "- embedding_model_digest will be blank in output this run (scores themselves are unaffected)."
    ))
    return(NA_character_)
  }
  if (resp_status(resp) >= 400) {
    log_warn(sprintf(
      "Ollama /api/tags returned HTTP %d - embedding_model_digest will be blank in output this run.",
      resp_status(resp)
    ))
    return(NA_character_)
  }
  body <- resp_body_json(resp, simplifyVector = FALSE)
  digest <- extract_model_digest(body, config$ollama_model)
  if (is.na(digest)) {
    log_warn(sprintf(
      "Could not find model '%s' in Ollama's /api/tags listing - is it pulled (`ollama pull %s`)? embedding_model_digest will be blank this run.",
      config$ollama_model, config$ollama_model
    ))
  }
  digest
}

# ==============================================================================
# Ollama embedding calls
# ==============================================================================
# Uses Ollama's /api/embed endpoint, which accepts a BATCH of inputs in one
# call (input: array of strings) - important for item-level granularity,
# where a single vacancy can have dozens of short bullets to embed.
#
# `input = as.list(texts)` (not the bare character vector) is deliberate: a
# length-1 character vector would otherwise get auto-unboxed by
# jsonlite/httr2 into a bare JSON string instead of a 1-element JSON array,
# which Ollama's batch endpoint does not expect - as.list() forces array
# serialization regardless of length, avoiding a footgun on the (common)
# single-item case.
# ------------------------------------------------------------------------------
call_ollama_embed <- function(texts, config, max_tries = 3) {
  if (length(texts) == 0) return(list())
  
  req <- request(config$ollama_api_url) |>
    req_body_json(list(model = config$ollama_model, input = as.list(texts)))
  
  resp <- NULL
  for (attempt in seq_len(max_tries)) {
    resp <- tryCatch(req_perform(req), error = function(e) e)
    if (!inherits(resp, "error")) {
      status <- resp_status(resp)
      if (status < 500) break
    }
    if (attempt < max_tries) Sys.sleep(2 ^ attempt)
  }
  
  if (inherits(resp, "error")) {
    log_warn(paste(
      "Embedding call to Ollama failed:", conditionMessage(resp),
      "- is Ollama running? Start it (`ollama serve`, or the desktop app) and confirm the model is pulled: `ollama pull", config$ollama_model, "`"
    ))
    return(NULL)
  }
  
  status <- resp_status(resp)
  if (status >= 400) {
    err_body <- tryCatch(resp_body_json(resp), error = function(e) NULL)
    log_warn(sprintf(
      "Ollama returned HTTP %d: %s",
      status, (err_body$error %||% "unknown error")
    ))
    return(NULL)
  }
  
  body <- resp_body_json(resp, simplifyVector = FALSE)
  if (is.null(body$embeddings)) {
    log_warn("Ollama response had no 'embeddings' field - unexpected response shape, check the Ollama version/API.")
    return(NULL)
  }
  lapply(body$embeddings, function(e) as.numeric(unlist(e)))
}

# ------------------------------------------------------------------------------
# Cached, batched wrapper around call_ollama_embed(). Cache is a plain R
# environment keyed on a SHA-256 hash of "<model>||<text>" (via
# digest::digest()). The text is hashed rather than used directly as the
# environment variable name because R limits variable names to 10000
# bytes, which a pooled document/section text (a whole master template, or
# a merged job ad unioned across several repostings) routinely exceeds.
# Keying on model name (as part of the hashed input) means switching
# CONFIG$ollama_model automatically invalidates stale entries rather than
# silently reusing them.
#
# Returns a list the SAME LENGTH AND ORDER as `texts`, with NULL for any text
# that failed to embed (caller must handle NULL, not assume every call
# succeeded) - a failed embed becomes an NA score downstream, never a
# silently-wrong 0.
# ------------------------------------------------------------------------------
get_embeddings <- function(texts, config, cache_env, warn_label = NULL) {
  if (length(texts) == 0) return(list())
  
  if (!is.null(config$embed_context_warn_chars)) {
    long_idx <- which(nchar(texts) > config$embed_context_warn_chars)
    for (i in long_idx) {
      log_warn(sprintf(
        "%stext passed to the embedding model is %d characters (~%d tokens est.) - may exceed %s's context window and be silently truncated by Ollama. Consider a finer granularity_mode for this case.",
        if (!is.null(warn_label)) paste0(warn_label, ": ") else "",
        nchar(texts[i]), round(nchar(texts[i]) / 4), config$ollama_model
      ))
    }
  }
  
  cache_key_for <- function(t) digest::digest(paste0(config$ollama_model, "||", t), algo = "sha256")
  cache_keys <- vapply(texts, cache_key_for, character(1), USE.NAMES = FALSE)
  is_cached  <- vapply(cache_keys, exists, logical(1), envir = cache_env)
  to_fetch_idx <- which(!is_cached)
  
  if (length(to_fetch_idx) > 0 && isTRUE(config$call_embeddings)) {
    to_fetch <- texts[to_fetch_idx]
    chunks <- split(to_fetch, ceiling(seq_along(to_fetch) / config$ollama_batch_size))
    for (chunk in chunks) {
      embeddings <- call_ollama_embed(chunk, config)
      if (is.null(embeddings) || length(embeddings) != length(chunk)) {
        log_warn(sprintf(
          "Embedding batch failed or returned a mismatched length (%d requested, %d returned) - these %d text(s) will score as NA this run.",
          length(chunk), length(if (is.null(embeddings)) 0 else embeddings), length(chunk)
        ))
        next
      }
      for (i in seq_along(chunk)) {
        assign(cache_key_for(chunk[i]), embeddings[[i]], envir = cache_env)
      }
    }
  } else if (length(to_fetch_idx) > 0 && !isTRUE(config$call_embeddings)) {
    log_msg(sprintf("  [call_embeddings = FALSE] would embed %d text(s) here.", length(to_fetch_idx)))
  }
  
  lapply(cache_keys, function(k) if (exists(k, envir = cache_env)) get(k, envir = cache_env) else NULL)
}

# ------------------------------------------------------------------------------
# Embedding cache persistence (RDS, single file - simplest option for a
# corpus this size; avoids thousands of tiny per-item files on disk).
# ------------------------------------------------------------------------------
load_embedding_cache <- function(path) {
  env <- new.env(parent = emptyenv())
  if (file.exists(path)) {
    cached <- tryCatch(readRDS(path), error = function(e) {
      log_warn(sprintf("Could not read embedding cache at '%s' (%s) - starting a fresh cache.", path, conditionMessage(e)))
      NULL
    })
    if (!is.null(cached)) {
      list2env(cached, envir = env)
      log_msg(sprintf("Loaded embedding cache: %d entries from '%s'.", length(cached), path))
    }
  }
  env
}

save_embedding_cache <- function(cache_env, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(as.list(cache_env), path)
  log_msg(sprintf("Saved embedding cache: %d entries to '%s'.", length(ls(cache_env)), path))
}

# ==============================================================================
# Similarity computation
# ==============================================================================
cosine_sim <- function(a, b) {
  if (is.null(a) || is.null(b)) return(NA_real_)
  denom <- sqrt(sum(a^2)) * sqrt(sum(b^2))
  if (denom == 0 || is.na(denom)) return(NA_real_)
  sum(a * b) / denom
}

pool_text <- function(items) paste(items, collapse = "\n")

# ------------------------------------------------------------------------------
# Stable, arbitrary item identifiers for the item-level match-detail file -
# e.g. item_ids("M", "R", 3) -> c("M.R01","M.R02","M.R03"). Zero-padded to a
# minimum of `width` digits (default 2) so a spreadsheet's default
# lexicographic sort ("M.R1", "M.R10", "M.R2", ...) doesn't scramble the
# order - plain "M.R1"..."M.R9" would sort fine alone, but the moment a list
# reaches double digits, "M.R10" sorts before "M.R2" as plain text. Widens
# automatically beyond `width` if a given list actually has more than 99
# items, so this never silently breaks for an unusually long list - it just
# stops being padded to exactly 2 digits for that one list.
#
# "M." ids are stable across the whole run (the master template doesn't
# change per vacancy); "J." ids are only unique WITHIN a given
# vacancy_number (each vacancy has its own J.R01, J.S01, ...) - always pair
# a J. id with its vacancy_number column in similarity_item_matches.csv
# when referencing a specific bullet.
# ------------------------------------------------------------------------------
item_ids <- function(prefix, section_code, n, width = 2) {
  if (n == 0) return(character(0))
  width <- max(width, nchar(as.character(n)))
  paste0(prefix, ".", section_code, sprintf(paste0("%0", width, "d"), seq_len(n)))
}

# ------------------------------------------------------------------------------
# For each item on the ANCHOR side, find its single BEST match on the POOL
# side (max cosine similarity) and return one row per anchor item recording
# both texts, both ids, and the similarity - the shared building block for
# BOTH directions of item-level comparison:
#   - best_matches(job items -> master pool)   = "job-relevance" (precision):
#     for each of THIS AD's bullets, how well is it explained by something in
#     the master template? This is the PRIMARY item-level score (see
#     compute_score_item()) - it doesn't get diluted by the master's breadth,
#     since we're averaging over the (much smaller) job side.
#   - best_matches(master items -> job pool)   = "master-coverage" (recall):
#     for each of the ~20-30 MASTER criteria, is it found somewhere in this
#     ad? Useful as a corpus-level diagnostic (which master criteria are
#     rarely/never matched by any real ad) and per-vacancy "checklist" view,
#     but NOT used as the primary score - a single job is never expected to
#     cover a template aggregated across ~30 postings, so this number is
#     naturally low for every vacancy and doesn't discriminate well on its
#     own.
#
# A row is still returned (with NA match/similarity) for an anchor item that
# failed to embed, or when the pool side is empty - a missing comparison
# must stay visible, never silently disappear from the detail file.
# ------------------------------------------------------------------------------
best_matches <- function(anchor_ids, anchor_texts, anchor_embeddings,
                         pool_ids, pool_texts, pool_embeddings) {
  n <- length(anchor_embeddings)
  if (n == 0) {
    return(data.frame(
      anchor_item_id = character(0), anchor_item_text = character(0),
      matched_item_id = character(0), matched_item_text = character(0),
      similarity_score = numeric(0), stringsAsFactors = FALSE
    ))
  }
  matched_id   <- character(n)
  matched_text <- character(n)
  sim          <- numeric(n)
  for (i in seq_len(n)) {
    a <- anchor_embeddings[[i]]
    pool_sims <- if (is.null(a) || length(pool_embeddings) == 0) {
      rep(NA_real_, length(pool_embeddings))
    } else {
      vapply(pool_embeddings, function(p) cosine_sim(a, p), numeric(1))
    }
    if (length(pool_sims) == 0 || all(is.na(pool_sims))) {
      matched_id[i]   <- NA_character_
      matched_text[i] <- NA_character_
      sim[i]          <- NA_real_
    } else {
      best_idx        <- which.max(pool_sims)
      matched_id[i]   <- pool_ids[best_idx]
      matched_text[i] <- pool_texts[best_idx]
      sim[i]          <- pool_sims[best_idx]
    }
  }
  data.frame(
    anchor_item_id = anchor_ids, anchor_item_text = anchor_texts,
    matched_item_id = matched_id, matched_item_text = matched_text,
    similarity_score = sim, stringsAsFactors = FALSE
  )
}

# Mean of a best_matches() data.frame's similarity_score column, NA-safe.
# Returns NA (not 0) rather than treating "nothing to compare" the same as
# "compared and found zero similarity" - same reasoning as elsewhere in this
# script's NA handling.
mean_similarity <- function(matches_df) {
  if (nrow(matches_df) == 0) return(NA_real_)
  vals <- matches_df$similarity_score
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) return(NA_real_)
  mean(vals)
}

# ------------------------------------------------------------------------------
# Classifies each job item (one section at a time - responsibilities or
# skills) as CORE (data-steward-specific) or GENERAL (generic/non-DS content
# better matched by the master template's optional GENERAL pool - see
# parse_master_template()), by comparing each item against BOTH pools and
# keeping whichever pool gives the higher similarity.
#
# This runs the SAME regardless of which granularity_modes are active - the
# filtered (CORE-only) item list this returns feeds section/document
# pooling too, not just item-level scoring, so a job bullet that's really
# "excellent written and verbal communication skills" doesn't inflate a
# document-level score just because it's baked into one pooled string. This
# is the one place a GENERAL pool adds cost to document/section modes:
# classifying requires per-item embeddings even when only document/section
# granularity was requested.
#
# check_general lets the caller disable the GENERAL comparison entirely for
# a given call (used for responsibilities when CONFIG$apply_general_pool_to_
# responsibilities is FALSE) without needing a separate code path - when
# FALSE, or when the master has no GENERAL section at all
# (master_general_ids has length 0), every item classifies CORE and the
# keep_* lists equal the original inputs unchanged - a pure identity
# passthrough, so this costs nothing when the feature isn't in play.
# ------------------------------------------------------------------------------
classify_job_items <- function(ids, texts, section_label,
                               master_core_ids, master_core_texts, master_core_embeddings,
                               master_general_ids, master_general_texts, master_general_embeddings,
                               check_general, vacancy_number, config, cache_env) {
  empty_matches <- function() data.frame(
    anchor_item_id = character(0), anchor_item_text = character(0),
    matched_item_id = character(0), matched_item_text = character(0),
    similarity_score = numeric(0), stringsAsFactors = FALSE
  )
  if (length(texts) == 0) {
    return(list(
      keep_ids = character(0), keep_texts = character(0), is_general = logical(0),
      core_matches = empty_matches(), general_matches = empty_matches(), checked_general = FALSE
    ))
  }
  
  embeddings <- get_embeddings(texts, config, cache_env,
                               warn_label = sprintf("vacancy %s (item/%s)", vacancy_number, section_label))
  core_matches <- best_matches(ids, texts, embeddings, master_core_ids, master_core_texts, master_core_embeddings)
  
  do_general <- isTRUE(check_general) && length(master_general_ids) > 0
  if (do_general) {
    general_matches <- best_matches(ids, texts, embeddings, master_general_ids, master_general_texts, master_general_embeddings)
    is_general <- !is.na(general_matches$similarity_score) &
      (is.na(core_matches$similarity_score) | general_matches$similarity_score > core_matches$similarity_score)
  } else {
    general_matches <- data.frame(
      anchor_item_id = ids, anchor_item_text = texts,
      matched_item_id = NA_character_, matched_item_text = NA_character_,
      similarity_score = NA_real_, stringsAsFactors = FALSE
    )
    is_general <- rep(FALSE, length(texts))
  }
  
  list(
    keep_ids = ids[!is_general], keep_texts = texts[!is_general], is_general = is_general,
    core_matches = core_matches, general_matches = general_matches, checked_general = do_general
  )
}

mean_section_scores <- function(score_responsibilities, score_skills) {
  vals <- c(score_responsibilities, score_skills)
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) return(NA_real_)
  mean(vals)
}

# ------------------------------------------------------------------------------
# Optionally save the exact pooled text sent to the embedding model, for
# spot-checking - same purpose as Script 1's save_extracted_text.
# ------------------------------------------------------------------------------
maybe_save_pooled_text <- function(vacancy_number, granularity, label, text, config) {
  if (!isTRUE(config$save_pooled_text)) return(invisible(NULL))
  dir.create(config$pooled_text_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(config$pooled_text_dir, sprintf("vacancy_%s_%s_%s.txt", vacancy_number, granularity, label))
  writeLines(text, out_path, useBytes = TRUE)
}

# ==============================================================================
# Per-vacancy scoring, one function per granularity mode
# ==============================================================================

# --- Document-level: one pooled string for the whole ad vs. one pooled
#     string for the whole master template. Coarsest signal, cheapest, and
#     the one most exposed to the context-window truncation risk flagged in
#     CONFIG$embed_context_warn_chars - a merged UNION file across several
#     postings can get long. -------------------------------------------------
compute_score_document <- function(job_parsed, master_pooled_all, vacancy_number, config, cache_env) {
  job_text <- pool_text(c(job_parsed$responsibilities, job_parsed$skills))
  maybe_save_pooled_text(vacancy_number, "document", "job", job_text, config)
  
  if (!nzchar(trimws(job_text))) {
    return(list(score_overall = NA_real_))
  }
  
  embeddings <- get_embeddings(c(job_text, master_pooled_all), config, cache_env,
                               warn_label = sprintf("vacancy %s (document)", vacancy_number))
  list(score_overall = cosine_sim(embeddings[[1]], embeddings[[2]]))
}

# --- Section-level: pooled responsibilities vs. pooled master
#     responsibilities, pooled (merged essential+desirable) skills vs. pooled
#     master skills, scored independently. ------------------------------------
compute_score_section <- function(job_parsed, master_pooled_resp, master_pooled_skills,
                                  vacancy_number, config, cache_env) {
  job_resp_text   <- pool_text(job_parsed$responsibilities)
  job_skills_text <- pool_text(job_parsed$skills)
  maybe_save_pooled_text(vacancy_number, "section", "job_responsibilities", job_resp_text, config)
  maybe_save_pooled_text(vacancy_number, "section", "job_skills", job_skills_text, config)
  
  score_resp <- NA_real_
  if (length(job_parsed$responsibilities) > 0) {
    e <- get_embeddings(c(job_resp_text, master_pooled_resp), config, cache_env,
                        warn_label = sprintf("vacancy %s (section/responsibilities)", vacancy_number))
    score_resp <- cosine_sim(e[[1]], e[[2]])
  }
  
  score_skills <- NA_real_
  if (length(job_parsed$skills) > 0) {
    e <- get_embeddings(c(job_skills_text, master_pooled_skills), config, cache_env,
                        warn_label = sprintf("vacancy %s (section/skills)", vacancy_number))
    score_skills <- cosine_sim(e[[1]], e[[2]])
  }
  
  list(
    score_responsibilities = score_resp,
    score_skills           = score_skills,
    score_overall           = mean_section_scores(score_resp, score_skills)
  )
}

# --- Item-level: each job-ad bullet vs. each master criterion. resp_class/
#     skl_class are precomputed by classify_job_items() in main_similarity_scoring() (shared
#     across ALL granularity modes for this vacancy, not just item-level),
#     and already contain: the CORE-pool best match per job item
#     (job_relevance direction - PRIMARY, feeds score_responsibilities/
#     score_skills), the GENERAL-pool best match per job item (diagnostic
#     only), and which items were classified GENERAL (excluded from
#     scoring).
#
#     resp_adj_class/skl_adj_class (both optional, NULL unless CONFIG$
#     use_adjacent_pool is TRUE) are the SECOND-stage classification -
#     ADJACENT_ROLE vs. CORE, run only on whatever survived the GENERAL
#     stage. Kept as separate objects from resp_class/skl_class (not
#     merged) specifically so their match data can be labelled correctly
#     ("adjacent_match", not mistakenly "general_match") in the output -
#     see the main-loop comment above this function's call site.
#
#     This function adds the master_coverage direction (restricted to
#     items that survived BOTH stages - a GENERAL- or ADJACENT_ROLE-
#     classified bullet is never eligible to "cover" a master criterion
#     either) and assembles/aggregates everything.
#
#     Master item embeddings/ids are computed ONCE for the whole run (passed
#     in already-embedded) since the master template doesn't change per
#     vacancy. Re-fetching the CORE-classified subset's embeddings below
#     hits the cache (already embedded during classification) - no extra
#     network cost. -------------------------------------------------------
compute_score_item <- function(resp_class, skl_class, master,
                               master_resp_ids, master_skills_ids,
                               master_resp_embeddings, master_skills_embeddings,
                               vacancy_number, config, cache_env,
                               resp_adj_class = NULL, skl_adj_class = NULL) {
  # "Final" survivors after both stages - resp_adj_class/skl_adj_class,
  # when present, already only ever contain stage-1 survivors as their
  # input (see main_similarity_scoring()), so their own keep_ids/keep_texts/core_matches/
  # is_general already correctly represent "survived GENERAL AND
  # ADJACENT_ROLE" with no further combination needed for these fields.
  resp_final <- if (!is.null(resp_adj_class)) resp_adj_class else resp_class
  skl_final  <- if (!is.null(skl_adj_class))  skl_adj_class  else skl_class
  
  resp_keep_embeddings <- get_embeddings(resp_final$keep_texts, config, cache_env,
                                         warn_label = sprintf("vacancy %s (item/responsibilities, master_coverage)", vacancy_number))
  skl_keep_embeddings  <- get_embeddings(skl_final$keep_texts, config, cache_env,
                                         warn_label = sprintf("vacancy %s (item/skills, master_coverage)", vacancy_number))
  
  # SECONDARY (diagnostic only): master-coverage - for each master criterion,
  # best match among items that survived every active classification stage.
  mc_resp <- best_matches(master_resp_ids, master$responsibilities, master_resp_embeddings,
                          resp_final$keep_ids, resp_final$keep_texts, resp_keep_embeddings)
  mc_skl  <- best_matches(master_skills_ids, master$skills, master_skills_embeddings,
                          skl_final$keep_ids, skl_final$keep_texts, skl_keep_embeddings)
  
  # PRIMARY: job-relevance, averaged over items surviving every active
  # stage - GENERAL- and/or ADJACENT_ROLE-classified items are excluded
  # from the mean entirely, not scored as 0.
  score_resp <- if (nrow(resp_final$core_matches) == 0) NA_real_ else mean_similarity(resp_final$core_matches[!resp_final$is_general, , drop = FALSE])
  score_skl  <- if (nrow(skl_final$core_matches) == 0) NA_real_ else mean_similarity(skl_final$core_matches[!skl_final$is_general, , drop = FALSE])
  
  # Combined exclusion mask, aligned to the ORIGINAL full item set (stage
  # 1's ids/texts) - TRUE if excluded at stage 1, or if it survived stage 1
  # but was then excluded at stage 2. adj_cls$is_general is aligned by
  # POSITION with stage1$keep_ids (adj_cls was called with exactly those
  # ids/texts, in that order, as its input - see main_similarity_scoring()), so overlaying by
  # position onto the stage-1 survivor positions is valid.
  combined_exclusion_mask <- function(stage1, adj) {
    mask <- stage1$is_general
    if (!is.null(adj)) {
      survivor_positions <- which(!stage1$is_general)
      mask[survivor_positions] <- mask[survivor_positions] | adj$is_general
    }
    mask
  }
  
  section_rows <- function(section, cls, adj_cls) {
    if (nrow(cls$core_matches) == 0) return(NULL)
    rows <- list(cbind(section = section, direction = "job_relevance", cls$core_matches,
                       excluded_from_score = combined_exclusion_mask(cls, adj_cls), stringsAsFactors = FALSE))
    if (isTRUE(cls$checked_general)) {
      rows[[length(rows) + 1]] <- cbind(section = section, direction = "general_match", cls$general_matches,
                                        excluded_from_score = cls$is_general, stringsAsFactors = FALSE)
    }
    rows
  }
  # Separate from section_rows() above (not folded in) so "general_match"
  # and "adjacent_match" stay unambiguous in similarity_item_matches.csv's
  # direction column - each always means comparison against that ONE named
  # pool, never a mix of the two.
  adjacent_rows <- function(section, adj_cls) {
    if (is.null(adj_cls) || !isTRUE(adj_cls$checked_general) || nrow(adj_cls$general_matches) == 0) return(NULL)
    list(cbind(section = section, direction = "adjacent_match", adj_cls$general_matches,
               excluded_from_score = adj_cls$is_general, stringsAsFactors = FALSE))
  }
  coverage_rows <- function(section, mc_df) {
    if (nrow(mc_df) == 0) return(NULL)
    list(cbind(section = section, direction = "master_coverage", mc_df,
               excluded_from_score = FALSE, stringsAsFactors = FALSE))
  }
  
  match_parts <- c(
    section_rows("responsibilities", resp_class, resp_adj_class),
    section_rows("skills", skl_class, skl_adj_class),
    adjacent_rows("responsibilities", resp_adj_class),
    adjacent_rows("skills", skl_adj_class),
    coverage_rows("responsibilities", mc_resp),
    coverage_rows("skills", mc_skl)
  )
  match_parts <- Filter(Negate(is.null), match_parts)
  match_detail <- if (length(match_parts) > 0) do.call(rbind, match_parts) else data.frame()
  
  list(
    score_responsibilities = score_resp,
    score_skills           = score_skl,
    score_overall           = mean_section_scores(score_resp, score_skl),
    match_detail            = match_detail
  )
}

# ==============================================================================
# Status table filtering
# ==============================================================================
read_status_table <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("vacancy_status.csv not found at '%s' - run extract_job_criteria.R first.", path))
  }
  utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character", na.strings = character(0))
}

filter_scoreable_rows <- function(status_df, config) {
  n_before <- nrow(status_df)
  
  keep_status <- status_df$status %in% config$scoreable_statuses
  skipped_status <- sum(!keep_status)
  
  n_resp <- suppressWarnings(as.integer(trimws(status_df$n_responsibilities)))
  n_skl  <- suppressWarnings(as.integer(trimws(status_df$n_skills)))
  has_content <- !(is.na(n_resp) & is.na(n_skl)) & (coalesce0(n_resp) > 0 | coalesce0(n_skl) > 0)
  skipped_empty <- sum(keep_status & !has_content)
  
  flagged <- toupper(trimws(status_df$flagged_for_review %||% "")) == "TRUE"
  keep_flagged <- flagged & !isTRUE(config$process_flagged_for_review)
  skipped_flagged <- sum(keep_status & has_content & keep_flagged)
  
  keep <- keep_status & has_content & !keep_flagged
  out <- status_df[keep, , drop = FALSE]
  
  log_msg(sprintf(
    "Row filtering: %d total | %d skipped (status not in %s) | %d skipped (nothing extracted) | %d skipped (flagged_for_review, process_flagged_for_review = FALSE) | %d remaining to score.",
    n_before, skipped_status, paste(config$scoreable_statuses, collapse = "/"),
    skipped_empty, skipped_flagged, nrow(out)
  ))
  
  if (length(config$only_vacancy_numbers) > 0) {
    not_found <- setdiff(config$only_vacancy_numbers, out$vacancy_number)
    if (length(not_found) > 0) {
      log_msg(sprintf(
        "CONFIG$only_vacancy_numbers requested %s - not eligible after the filters above (wrong status, nothing extracted, or flagged_for_review) and will be skipped this run.",
        paste(not_found, collapse = ", ")
      ))
    }
    out <- out[out$vacancy_number %in% config$only_vacancy_numbers, , drop = FALSE]
    log_msg(sprintf(
      "CONFIG$only_vacancy_numbers restriction active (small-scale test run) - narrowed to %d row(s).",
      nrow(out)
    ))
  }
  
  out
}

coalesce0 <- function(x) ifelse(is.na(x), 0L, x)

# ------------------------------------------------------------------------------
# Decide whether (vacancy_number, granularity) already has a usable score in
# existing_scores. When config$require_model_match_for_skip is TRUE
# (default), a row only counts as "already done" if it was scored with the
# SAME model name and, when both sides have a digest recorded, the SAME
# digest - a name-only match with no digest available on either side is
# still treated as a match (there's nothing to disprove, and forcing a full
# rescore the first time this check ships against an existing scores file
# would be a surprising one-off cost). Any name or digest mismatch that
# forces a rescore is logged, so an automatic full-corpus rescore triggered
# by a silent model update never happens invisibly.
# ------------------------------------------------------------------------------
already_scored <- function(existing_scores, vn, granularity, config, model_digest) {
  if (is.null(existing_scores)) return(FALSE)
  match_rows <- which(existing_scores$vacancy_number == vn & existing_scores$granularity == granularity)
  if (length(match_rows) == 0) return(FALSE)
  if (!isTRUE(config$require_model_match_for_skip)) return(TRUE)
  
  existing_model_col  <- if ("embedding_model" %in% names(existing_scores)) existing_scores$embedding_model else rep(NA_character_, nrow(existing_scores))
  existing_digest_col <- if ("embedding_model_digest" %in% names(existing_scores)) existing_scores$embedding_model_digest else rep(NA_character_, nrow(existing_scores))
  
  for (idx in match_rows) {
    ex_model  <- existing_model_col[idx]
    ex_digest <- existing_digest_col[idx]
    if (!nzchar(ex_model %||% "")) ex_model <- NA_character_
    if (!nzchar(ex_digest %||% "")) ex_digest <- NA_character_
    
    if (!is.na(ex_model) && !identical(ex_model, config$ollama_model)) {
      log_msg(sprintf(
        "Model mismatch for vacancy_number '%s' [%s]: existing score used '%s', current run uses '%s' - forcing a rescore (set CONFIG$require_model_match_for_skip = FALSE to disable this check).",
        vn, granularity, ex_model, config$ollama_model
      ))
      next
    }
    if (!is.na(ex_digest) && !is.na(model_digest) && !identical(ex_digest, model_digest)) {
      log_msg(sprintf(
        "Model digest mismatch for vacancy_number '%s' [%s]: same model name ('%s') but existing digest (%s) differs from the current run's (%s) - weights changed under the same name - forcing a rescore (set CONFIG$require_model_match_for_skip = FALSE to disable this check).",
        vn, granularity, config$ollama_model, ex_digest, model_digest
      ))
      next
    }
    return(TRUE)   # name matches (or unrecorded), digest matches or unverifiable on one/both sides
  }
  FALSE
}

# ==============================================================================
# Similarity-scores output (upsert, keyed on vacancy_number + granularity -
# same schema-evolution-safe pattern as Script 1's upsert_status_table())
# ==============================================================================
upsert_similarity_scores <- function(path, new_rows) {
  if (nrow(new_rows) == 0) return(invisible(NULL))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  
  new_rows$.key <- paste(new_rows$vacancy_number, new_rows$granularity, sep = "::")
  
  if (file.exists(path)) {
    existing <- utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character", na.strings = character(0))
    existing$.key <- paste(existing$vacancy_number, existing$granularity, sep = "::")
    existing <- existing[!(existing$.key %in% new_rows$.key), , drop = FALSE]
    
    missing_in_existing <- setdiff(names(new_rows), names(existing))
    for (col in missing_in_existing) existing[[col]] <- rep("", nrow(existing))   # rep(), not a bare "" - errors when existing has 0 rows (e.g. every row for this key just got superseded)
    missing_in_new <- setdiff(names(existing), names(new_rows))
    for (col in missing_in_new) new_rows[[col]] <- rep("", nrow(new_rows))
    
    all_cols <- union(names(existing), names(new_rows))
    combined <- rbind(existing[all_cols], new_rows[all_cols])
  } else {
    combined <- new_rows
  }
  
  combined <- combined[order(combined$vacancy_number, combined$granularity), , drop = FALSE]
  combined$.key <- NULL
  utils::write.csv(combined, path, row.names = FALSE)
}

fmt_score <- function(x) ifelse(is.na(x), "NA", sprintf("%.4f", x))

format_duration <- function(seconds) {
  if (is.na(seconds) || is.null(seconds) || seconds < 0) return("unknown")
  seconds <- round(seconds)
  h <- seconds %/% 3600
  m <- (seconds %% 3600) %/% 60
  s <- seconds %% 60
  if (h > 0) sprintf("%dh %dm", h, m)
  else if (m > 0) sprintf("%dm %ds", m, s)
  else sprintf("%ds", s)
}

# ------------------------------------------------------------------------------
# Writes accumulated results/item_match_results to disk (via the upsert
# functions) and saves the embedding cache, then returns EMPTY buffers for
# the caller to keep accumulating into. Called periodically during the main
# loop (CONFIG$checkpoint_every_n_vacancies) and once more at the end of a
# run - a crash between checkpoints only loses work back to the last one,
# not the whole run. Safe/cheap to call with empty buffers (a no-op write).
# ------------------------------------------------------------------------------
flush_progress <- function(config, results, item_match_results, cache_env) {
  if (length(results) > 0) {
    new_rows <- do.call(rbind, results)
    upsert_similarity_scores(config$scores_path, new_rows)
    log_msg(sprintf("Checkpoint: wrote/updated %d row(s) to %s", nrow(new_rows), config$scores_path))
  }
  if (length(item_match_results) > 0) {
    new_match_rows <- do.call(rbind, item_match_results)
    upsert_item_matches(config$item_matches_path, new_match_rows)
    log_msg(sprintf("Checkpoint: wrote/updated %d item-match row(s) to %s", nrow(new_match_rows), config$item_matches_path))
  }
  save_embedding_cache(cache_env, config$cache_path)
  list(results = list(), item_match_results = list())
}

# ==============================================================================
# Item-match detail output (upsert, keyed on vacancy_number ONLY - a rescore
# of a vacancy replaces ALL of its match rows across both sections and both
# directions, since this file always reflects the most recent item-level run
# for that vacancy, not an accumulating history). Same schema-evolution-safe
# pattern as the other upsert function above.
# ==============================================================================
upsert_item_matches <- function(path, new_rows) {
  if (nrow(new_rows) == 0) return(invisible(NULL))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  
  touched_vacancies <- unique(new_rows$vacancy_number)
  
  if (file.exists(path)) {
    existing <- utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character", na.strings = character(0))
    existing <- existing[!(existing$vacancy_number %in% touched_vacancies), , drop = FALSE]
    
    missing_in_existing <- setdiff(names(new_rows), names(existing))
    for (col in missing_in_existing) existing[[col]] <- rep("", nrow(existing))   # rep(), not a bare "" - errors when existing has 0 rows (e.g. every row for this vacancy just got superseded)
    missing_in_new <- setdiff(names(existing), names(new_rows))
    for (col in missing_in_new) new_rows[[col]] <- rep("", nrow(new_rows))
    
    all_cols <- union(names(existing), names(new_rows))
    combined <- rbind(existing[all_cols], new_rows[all_cols])
  } else {
    combined <- new_rows
  }
  
  combined <- combined[order(combined$vacancy_number, combined$section, combined$direction, combined$anchor_item_id), , drop = FALSE]
  utils::write.csv(combined, path, row.names = FALSE)
}

# ------------------------------------------------------------------------------
# If config$validation_mode is TRUE, swaps every input/output path onto its
# validation_* counterpart (see the validation_mode comment in CONFIG for
# the full rationale). All downstream code stays completely unaware of
# which mode is active - it just reads config$status_table_path,
# config$merged_dir etc. as always - so this is the ONLY place validation
# vs. main-study path selection happens. master_template_path is
# deliberately left untouched - validation must score against the same
# template the main study uses, or it isn't validating anything.
#
# Also applies CONFIG$version_suffix (see that field's comment) to
# scores_path/item_matches_path - AFTER the validation_mode swap, so it
# suffixes whichever of the two paths just got selected, and ALWAYS (not
# only inside the validation_mode branch), so a plain main-study run gets
# suffixed too. Deliberately not applied to cache_path/pooled_text_dir/
# log_dir - see CONFIG$version_suffix's comment for why the cache in
# particular stays unversioned.
# ------------------------------------------------------------------------------
resolve_paths_for_mode_similarity_scoring <- function(config) {
  if (isTRUE(config$validation_mode)) {
    config$status_table_path <- config$validation_status_table_path
    config$merged_dir         <- config$validation_merged_dir
    config$scores_path         <- config$validation_scores_path
    config$item_matches_path    <- config$validation_item_matches_path
    config$cache_path            <- config$validation_cache_path
    config$pooled_text_dir         <- config$validation_pooled_text_dir
    config$log_dir                   <- config$validation_log_dir
  }
  config$scores_path       <- apply_version_suffix(config$scores_path, config$version_suffix)
  config$item_matches_path  <- apply_version_suffix(config$item_matches_path, config$version_suffix)
  config
}

# ==============================================================================
# Main
# ==============================================================================
main_similarity_scoring <- function(config) {
  config <- resolve_paths_for_mode_similarity_scoring(config)
  
  .log_con <<- setup_log_file(config)
  on.exit(if (!is.null(.log_con)) close(.log_con), add = TRUE)
  
  log_msg(sprintf("=== similarity_scoring.R run started: %s ===", Sys.time()))
  if (isTRUE(config$validation_mode)) {
    log_msg(sprintf(
      "*** VALIDATION MODE ACTIVE *** - reading %s / %s, writing under %s. Main-study Output/ files are not touched this run.",
      config$status_table_path, config$merged_dir, dirname(config$scores_path)
    ))
  }
  log_msg(sprintf("Granularity modes active this run: %s", paste(config$granularity_modes, collapse = ", ")))
  invalid_modes <- setdiff(config$granularity_modes, c("document", "section", "item"))
  if (length(invalid_modes) > 0) {
    stop(sprintf("CONFIG$granularity_modes contains unrecognized value(s): %s. Valid values: 'document', 'section', 'item'.", paste(invalid_modes, collapse = ", ")))
  }
  
  master <- parse_master_template(config$master_template_path)
  log_msg(sprintf(
    "Master template parsed: %d responsibilities, %d essential + %d desirable skills (%d merged).",
    length(master$responsibilities), length(master$skills_essential),
    length(master$skills_desirable), length(master$skills)
  ))
  
  cache_env <- load_embedding_cache(config$cache_path)
  
  model_digest <- NA_character_
  if (isTRUE(config$call_embeddings)) {
    model_digest <- get_ollama_model_digest(config)
    log_msg(sprintf(
      "Embedding model: %s (digest: %s)",
      config$ollama_model, if (is.na(model_digest)) "unavailable - see warning above" else model_digest
    ))
  } else {
    log_msg("[call_embeddings = FALSE] skipping model digest lookup (no Ollama calls this run).")
  }
  
  # Master-side embeddings needed once per run, computed only for active modes.
  master_pooled_all <- pool_text(c(master$responsibilities, master$skills))
  master_pooled_resp <- pool_text(master$responsibilities)
  master_pooled_skills <- pool_text(master$skills)
  
  master_resp_item_embeddings    <- list()
  master_skills_item_embeddings  <- list()
  master_general_item_embeddings <- list()
  master_adjacent_item_embeddings <- list()
  master_resp_ids    <- character(0)
  master_skills_ids  <- character(0)
  master_general_ids <- character(0)
  master_adjacent_ids <- character(0)
  
  # Whether the ADJACENT_ROLE pool is actually in play this run - gated on
  # BOTH the toggle AND the section having content, so a template that
  # happens to have an ADJACENT_ROLE section but use_adjacent_pool = FALSE
  # never pays for it (no embeddings computed at all - see CONFIG$use_
  # adjacent_pool for the full rationale).
  adjacent_pool_active <- isTRUE(config$use_adjacent_pool) && length(master$adjacent) > 0
  
  # Per-item master embeddings are needed not just for item-level scoring,
  # but whenever a GENERAL pool exists or the ADJACENT_ROLE pool is active
  # this run - classify_job_items() (called per vacancy below) needs them
  # to classify CORE vs GENERAL/ADJACENT_ROLE for every granularity mode,
  # not just "item". If none of these conditions hold, this block - and
  # the classification step in the main loop below - is skipped entirely,
  # so a template with no GENERAL section and no active ADJACENT_ROLE pool
  # costs exactly what this script cost before either feature existed.
  needs_item_embeddings <- ("item" %in% config$granularity_modes) || (length(master$general) > 0) || adjacent_pool_active
  
  if (needs_item_embeddings) {
    log_msg("Embedding master-template items (item-level scoring and/or GENERAL/ADJACENT_ROLE-pool classification) ...")
    master_resp_item_embeddings    <- get_embeddings(master$responsibilities, config, cache_env, warn_label = "master template (item/responsibilities)")
    master_skills_item_embeddings  <- get_embeddings(master$skills, config, cache_env, warn_label = "master template (item/skills)")
    master_resp_ids   <- item_ids("M", "R", length(master$responsibilities))
    master_skills_ids <- item_ids("M", "S", length(master$skills))
    if (length(master$general) > 0) {
      master_general_item_embeddings <- get_embeddings(master$general, config, cache_env, warn_label = "master template (item/general)")
      master_general_ids <- item_ids("M", "G", length(master$general))
      log_msg(sprintf(
        "GENERAL pool active: %d generic/non-DS item(s) - applied to skills always, responsibilities %s.",
        length(master$general), if (isTRUE(config$apply_general_pool_to_responsibilities)) "also" else "NOT (CONFIG$apply_general_pool_to_responsibilities = FALSE)"
      ))
    }
    if (adjacent_pool_active) {
      master_adjacent_item_embeddings <- get_embeddings(master$adjacent, config, cache_env, warn_label = "master template (item/adjacent)")
      master_adjacent_ids <- item_ids("M", "A", length(master$adjacent))
      log_msg(sprintf(
        "ADJACENT_ROLE pool active: %d item(s) - applied to skills always, responsibilities %s.",
        length(master$adjacent), if (isTRUE(config$apply_adjacent_pool_to_responsibilities)) "also" else "NOT (CONFIG$apply_adjacent_pool_to_responsibilities = FALSE)"
      ))
    } else if (length(master$adjacent) > 0) {
      log_msg(sprintf(
        "Master template has %d ADJACENT_ROLE item(s) but CONFIG$use_adjacent_pool is FALSE - pool ignored entirely this run, zero cost.",
        length(master$adjacent)
      ))
    }
  }
  
  status_df <- read_status_table(config$status_table_path)
  to_score <- filter_scoreable_rows(status_df, config)
  
  existing_scores <- NULL
  if (isTRUE(config$skip_existing_scores) && file.exists(config$scores_path)) {
    existing_scores <- utils::read.csv(config$scores_path, stringsAsFactors = FALSE, colClasses = "character", na.strings = character(0))
  }
  
  results <- list()
  item_match_results <- list()
  n_scored <- 0
  n_skipped_existing <- 0
  n_missing_file <- 0
  n_failed <- 0
  vacancies_since_checkpoint <- 0
  run_start_time <- Sys.time()
  
  # Called at the end of EVERY outer-loop iteration - including early exits
  # (missing file, classification failure) - so progress reporting and
  # checkpoint counting never get silently skipped just because a
  # particular vacancy failed early. Uses <<- since it mutates state in
  # main_similarity_scoring()'s enclosing scope (results/item_match_results/vacancies_since_checkpoint).
  end_of_vacancy_iteration <- function(i) {
    if (i %% config$progress_report_every_n == 0 || i == nrow(to_score)) {
      elapsed <- as.numeric(difftime(Sys.time(), run_start_time, units = "secs"))
      rate <- elapsed / i
      remaining <- rate * (nrow(to_score) - i)
      log_msg(sprintf(
        "Progress: %d/%d vacancies (%.1f%%) | %d scored | %d skipped (already scored) | %d missing file | %d failed | elapsed %s | est. remaining %s",
        i, nrow(to_score), 100 * i / nrow(to_score), n_scored, n_skipped_existing, n_missing_file, n_failed,
        format_duration(elapsed), format_duration(remaining)
      ))
    }
    vacancies_since_checkpoint <<- vacancies_since_checkpoint + 1
    if (vacancies_since_checkpoint >= config$checkpoint_every_n_vacancies) {
      flushed <- flush_progress(config, results, item_match_results, cache_env)
      results <<- flushed$results
      item_match_results <<- flushed$item_match_results
      vacancies_since_checkpoint <<- 0
    }
  }
  
  for (i in seq_len(nrow(to_score))) {
    row <- to_score[i, ]
    vn <- row$vacancy_number
    force_this <- vn %in% config$force_rescore_vacancy_numbers
    
    merged_path <- merged_file_path(vn, config)
    if (!file.exists(merged_path)) {
      log_warn(sprintf("vacancy_number '%s' is marked scoreable in vacancy_status.csv but its merged file is missing: %s - skipping.", vn, merged_path))
      n_missing_file <- n_missing_file + 1
      end_of_vacancy_iteration(i)
      next
    }
    job_parsed <- tryCatch(parse_merged_job_text(merged_path), error = function(e) {
      log_warn(sprintf("vacancy_number '%s': failed to parse merged file %s (%s) - skipping this vacancy this run (will be retried next run).", vn, merged_path, conditionMessage(e)))
      NULL
    })
    if (is.null(job_parsed)) {
      n_failed <- n_failed + 1
      end_of_vacancy_iteration(i)
      next
    }
    
    # Classification (GENERAL-pool filtering, then optionally a SECOND,
    # independent ADJACENT_ROLE-pool pass on top of whatever survives) -
    # shared across ALL granularity modes for this vacancy, computed once
    # regardless of how many modes are active this run. When
    # needs_item_embeddings is FALSE (no GENERAL section, no active
    # ADJACENT_ROLE pool, and item mode not requested), this is skipped
    # entirely and filtered_job_parsed is just job_parsed unchanged - zero
    # added cost for runs not using either feature.
    #
    # Stage 1 (GENERAL) and stage 2 (ADJACENT_ROLE) results are kept as
    # SEPARATE objects (resp_class/skl_class vs. resp_adj_class/
    # skl_adj_class) rather than one overwriting the other - reusing
    # classify_job_items()'s "general_matches"/"checked_general" fields for
    # the ADJACENT_ROLE comparison would mislabel adjacent-pool match data
    # as GENERAL-pool match data in similarity_item_matches.csv, corrupting
    # the direction column's meaning. compute_score_item() takes both and
    # emits a correctly-labeled third "adjacent_match" direction.
    if (needs_item_embeddings) {
      classification_ok <- TRUE
      classification_result <- tryCatch({
        resp_class <- classify_job_items(
          item_ids("J", "R", length(job_parsed$responsibilities)), job_parsed$responsibilities, "responsibilities",
          master_resp_ids, master$responsibilities, master_resp_item_embeddings,
          master_general_ids, master$general, master_general_item_embeddings,
          check_general = isTRUE(config$apply_general_pool_to_responsibilities),
          vn, config, cache_env
        )
        skl_class <- classify_job_items(
          item_ids("J", "S", length(job_parsed$skills)), job_parsed$skills, "skills",
          master_skills_ids, master$skills, master_skills_item_embeddings,
          master_general_ids, master$general, master_general_item_embeddings,
          check_general = TRUE,
          vn, config, cache_env
        )
        
        # Stage 2: ADJACENT_ROLE, applied only to stage-1 survivors (their
        # own ids/texts become this call's "job items" input) - so a bullet
        # already excluded as GENERAL never gets a second, redundant look
        # against ADJACENT_ROLE too, and the two pools' effects stay
        # cleanly separable (an item is excluded because of GENERAL, or
        # because of ADJACENT_ROLE, never ambiguously "both").
        if (adjacent_pool_active) {
          resp_adj_class <- classify_job_items(
            resp_class$keep_ids, resp_class$keep_texts, "responsibilities",
            master_resp_ids, master$responsibilities, master_resp_item_embeddings,
            master_adjacent_ids, master$adjacent, master_adjacent_item_embeddings,
            check_general = isTRUE(config$apply_adjacent_pool_to_responsibilities),
            vn, config, cache_env
          )
          skl_adj_class <- classify_job_items(
            skl_class$keep_ids, skl_class$keep_texts, "skills",
            master_skills_ids, master$skills, master_skills_item_embeddings,
            master_adjacent_ids, master$adjacent, master_adjacent_item_embeddings,
            check_general = TRUE,
            vn, config, cache_env
          )
        } else {
          resp_adj_class <- NULL
          skl_adj_class  <- NULL
        }
        
        final_resp_texts <- if (adjacent_pool_active) resp_adj_class$keep_texts else resp_class$keep_texts
        final_skl_texts  <- if (adjacent_pool_active) skl_adj_class$keep_texts  else skl_class$keep_texts
        
        list(resp_class = resp_class, skl_class = skl_class,
             resp_adj_class = resp_adj_class, skl_adj_class = skl_adj_class,
             filtered_job_parsed = list(responsibilities = final_resp_texts, skills = final_skl_texts))
      }, error = function(e) {
        log_warn(sprintf("vacancy_number '%s': classification step failed (%s) - skipping this vacancy entirely this run (will be retried on the next run).", vn, conditionMessage(e)))
        NULL
      })
      if (is.null(classification_result)) {
        n_failed <- n_failed + 1
        end_of_vacancy_iteration(i)
        next
      }
      resp_class     <- classification_result$resp_class
      skl_class      <- classification_result$skl_class
      resp_adj_class <- classification_result$resp_adj_class
      skl_adj_class  <- classification_result$skl_adj_class
      filtered_job_parsed <- classification_result$filtered_job_parsed
    } else {
      resp_class <- NULL
      skl_class  <- NULL
      resp_adj_class <- NULL
      skl_adj_class  <- NULL
      filtered_job_parsed <- job_parsed
    }
    
    for (granularity in config$granularity_modes) {
      already_done <- already_scored(existing_scores, vn, granularity, config, model_digest)
      if (already_done && !force_this) {
        n_skipped_existing <- n_skipped_existing + 1
        next
      }
      
      log_msg(sprintf("Scoring vacancy_number '%s' [%s] ...", vn, granularity))
      
      scores <- tryCatch({
        switch(
          granularity,
          "document" = compute_score_document(filtered_job_parsed, master_pooled_all, vn, config, cache_env),
          "section"  = compute_score_section(filtered_job_parsed, master_pooled_resp, master_pooled_skills, vn, config, cache_env),
          "item"     = compute_score_item(resp_class, skl_class, master,
                                          master_resp_ids, master_skills_ids,
                                          master_resp_item_embeddings, master_skills_item_embeddings,
                                          vn, config, cache_env,
                                          resp_adj_class = resp_adj_class, skl_adj_class = skl_adj_class)
        )
      }, error = function(e) {
        log_warn(sprintf("vacancy_number '%s' [%s]: scoring failed (%s) - skipped this run (not recorded as already-scored, so it will be retried next run).", vn, granularity, conditionMessage(e)))
        NULL
      })
      if (is.null(scores)) {
        n_failed <- n_failed + 1
        next
      }
      
      if (granularity == "item" && isTRUE(config$write_item_match_detail) &&
          !is.null(scores$match_detail) && nrow(scores$match_detail) > 0) {
        detail <- scores$match_detail
        # Output columns (similarity_item_matches.csv):
        #   vacancy_number         - which job ad this row belongs to
        #   section                 - "responsibilities" or "skills"
        #   direction                 - "job_relevance" (this job bullet's best
        #                              match in the master's CORE pool - the
        #                              primary score), "general_match" (its best
        #                              match in the GENERAL pool, diagnostic only),
        #                              "adjacent_match" (its best match in the
        #                              ADJACENT_ROLE pool, diagnostic only), or
        #                              "master_coverage" (a MASTER criterion's best
        #                              match among surviving job bullets - recall
        #                              diagnostic, not the primary score)
        #   anchor_item_id / anchor_item_text     - the item this row is anchored
        #                                          on (a job bullet, or a master
        #                                          criterion for master_coverage
        #                                          rows)
        #   matched_item_id / matched_item_text     - its single best match on the
        #                                            opposite side; NA if nothing
        #                                            was comparable
        #   similarity_score                          - cosine similarity of that
        #                                              best match; NA, never 0, if
        #                                              nothing was comparable
        #   excluded_from_score                         - TRUE if this job bullet
        #                                                was classified GENERAL or
        #                                                ADJACENT_ROLE and therefore
        #                                                excluded from
        #                                                score_responsibilities/
        #                                                score_skills
        #   embedding_model / embedding_model_digest      - which model (and
        #                                                  content hash) produced
        #                                                  this row's embeddings
        #   last_run_timestamp                              - when this row was
        #                                                     last (re)written
        item_match_results[[length(item_match_results) + 1]] <- data.frame(
          vacancy_number    = vn,
          section            = detail$section,
          direction           = detail$direction,
          anchor_item_id      = detail$anchor_item_id,
          anchor_item_text     = detail$anchor_item_text,
          matched_item_id       = detail$matched_item_id,
          matched_item_text      = detail$matched_item_text,
          similarity_score         = fmt_score(detail$similarity_score),
          excluded_from_score       = detail$excluded_from_score,
          embedding_model            = config$ollama_model,
          embedding_model_digest      = model_digest %||% NA_character_,
          last_run_timestamp          = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          stringsAsFactors = FALSE
        )
      }
      
      # Output columns (similarity_scores.csv):
      #   vacancy_number / position_category / position_title  - which job ad
      #   granularity            - "document" / "section" / "item"
      #   embedding_model / embedding_model_digest  - which model (and content
      #                                              hash) produced this row
      #   score_responsibilities / score_skills       - NA at "document"
      #                                                granularity (only
      #                                                score_overall is computed
      #                                                there); a genuinely
      #                                                incomparable section is
      #                                                NA, never 0
      #   score_overall                                 - the primary score for
      #                                                  this row's granularity
      #   n_job_responsibility_items / n_job_skill_items  - bullet counts on the
      #                                                    job-ad side
      #   n_master_responsibility_items /                   - bullet counts on
      #   n_master_skill_items                                the master-template
      #                                                        side (constant
      #                                                        across all rows
      #                                                        from one run)
      #   last_run_timestamp                                    - when this row
      #                                                          was last
      #                                                          (re)written
      results[[length(results) + 1]] <- data.frame(
        vacancy_number                = vn,
        position_category               = row$position_category %||% NA_character_,
        position_title                   = row$position_title %||% NA_character_,
        granularity                       = granularity,
        embedding_model                   = config$ollama_model,
        embedding_model_digest             = model_digest %||% NA_character_,
        score_responsibilities            = fmt_score(scores$score_responsibilities %||% NA_real_),
        score_skills                       = fmt_score(scores$score_skills %||% NA_real_),
        score_overall                      = fmt_score(scores$score_overall %||% NA_real_),
        n_job_responsibility_items        = length(job_parsed$responsibilities),
        n_job_skill_items                  = length(job_parsed$skills),
        n_master_responsibility_items     = length(master$responsibilities),
        n_master_skill_items               = length(master$skills),
        last_run_timestamp                 = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        stringsAsFactors = FALSE
      )
      n_scored <- n_scored + 1
    }
    
    end_of_vacancy_iteration(i)
  }
  
  log_msg(sprintf(
    "Scoring complete: %d (vacancy, granularity) rows scored, %d skipped (already scored), %d skipped (missing merged file), %d failed.",
    n_scored, n_skipped_existing, n_missing_file, n_failed
  ))
  
  flush_progress(config, results, item_match_results, cache_env)
  log_msg("Done.")
}

if (sys.nframe() == 0) {
  main_similarity_scoring(CONFIG)
}