#!/usr/bin/env Rscript
# =============================================================================
# report_review_labels.R
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
#   This repository contains CODE ONLY. It does not include any review set,
#   labels or HR data.
#
# Purpose:
#   Aggregates-only report on the manual labels (DS / Partial DS / No-DS) of a
#   review set built by export_review_set.R, after blind review.
#
# Input (CONFIG_REPORT$merged_path): one row per reviewed vacancy, with a
#   review_id and a label, plus (optional, used when present) confidence,
#   flagged, skipped, a note/justification, and the columns export_review_set.R
#   writes (vacancy_number, tier, position_category, position_title,
#   section_score, section_skills_score) - i.e. the reviewer's labels joined
#   back to the review set through the blind-review crosswalk. Column names
#   are matched against the candidate lists in CONFIG_REPORT.
#
# Reports
#   1. Label shares overall (DS / PDS / NO), confidence, flagged, skipped.
#   2. Label shares by tier (strict vs lenient_only) and by position category.
#   3. Whether the scores separate DS / Partial DS from No-DS: rank AUC and a
#      threshold sweep, overall and within each tier.
#   4. Label shares by reporting unit (optional; needs an HR-linked table with
#      a vacancy-number column and a reporting-unit column).
#   5. Flagged items and low-confidence labels (local only: notes, vacancy numbers).
#
# Standalone, base R only. Entry point main_report_review_labels(); config
# CONFIG_REPORT; every helper rpl_-prefixed.
#
# Privacy: aggregate files get small-cell suppression (min_cell); the unit table
# shows small counts as "<=N" (unit_censor_upto). Anything with vacancy numbers, notes or
# titles carries _LOCAL_ONLY in its name. The console log is NOT suppressed.
# Bias: recall over precision. No row is dropped; unmatched rows are reported.
#
# CAVEAT built into the output: the review set contains only vacancies ABOVE
# the thresholds, so (a) precision is the real-corpus precision of the tiers,
# (b) "recall" and AUC here are range-restricted and are NOT the validation
# metrics. They only describe ordering inside the set.
# =============================================================================

CONFIG_REPORT <- list(
  # the merged labels + crosswalk file (one row per vacancy, from the join step)
  merged_path   = file.path("..", "Output", "review_labels_joined_LOCAL_ONLY.csv"),
  # optional: HR-linked table (vacancy number + reporting unit), for the
  # reporting-unit breakdown. NA = skip.
  hr_linked_path = NA,
  output_dir    = file.path("..", "Output", "review_labels_report"),
  
  # column names (first match wins; the script stops with a clear message if a
  # required one is missing)
  col_id        = c("review_id"),
  col_label     = c("label"),
  col_conf      = c("confidence"),
  col_flag      = c("flagged"),
  col_skip      = c("skipped"),
  col_note      = c("justification", "note"),
  col_vacancy   = c("vacancy_number"),
  col_tier      = c("tier"),
  col_category  = c("position_category", "category"),
  col_title     = c("position_title", "title"),
  col_score_overall = c("section_score", "score_overall"),
  col_score_skills  = c("section_skills_score", "score_skills"),
  hr_vacancy_col = c("vacancy_number", "Vacancy Number", "vacancy"),
  hr_unit_col    = c("Reporting Unit", "Structure Group 1: Reporting Unit",
                     "Structure Group 1: Reporting Unit (latest)", "reporting_unit"),
  
  positive_scheme = "collapse",   # "collapse": DS + PDS = positive; "ds_only": DS alone
  min_cell        = 5,            # set to the ethics value
  unit_censor_upto = 5,           # unit table: counts <= this shown as "<=5"
  low_conf_upto   = 3             # confidence <= this is listed as low confidence
)

rpl_log  <- function(...) cat(format(Sys.time(), "%H:%M:%S"), "|", ..., "\n", sep = " ")
rpl_warn <- function(...) cat(format(Sys.time(), "%H:%M:%S"), "| WARNING:", ..., "\n", sep = " ")

rpl_pick <- function(df, candidates, what, required = TRUE) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit)) return(hit[1])
  if (required) stop(sprintf("Column for %s not found. Tried: %s. Present: %s",
                             what, paste(candidates, collapse = ", "),
                             paste(names(df), collapse = ", ")), call. = FALSE)
  NA_character_
}

rpl_read <- function(path) {
  if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
  d <- read.csv(path, encoding = "UTF-8", stringsAsFactors = FALSE,
                check.names = FALSE, na.strings = c("", "NA"))
  names(d)[1] <- sub(intToUtf8(0xFEFF), "", names(d)[1], fixed = TRUE)
  rpl_log("read", nrow(d), "rows from", path)
  d
}

# DS / PDS / NO whatever the spelling ("Partial DS", "No-DS", "NO", ...)
rpl_norm_label <- function(x) {
  k <- toupper(gsub("[^A-Za-z]", "", as.character(x)))
  out <- ifelse(k %in% c("DS", "DATASTEWARD"), "DS",
                ifelse(k %in% c("PDS", "PARTIALDS", "PARTIAL"), "PDS",
                       ifelse(k %in% c("NO", "NODS", "N"), "NO", NA_character_)))
  out
}
rpl_bool <- function(x) toupper(trimws(as.character(x))) %in% c("TRUE", "T", "1", "YES")

# rank (Mann-Whitney) AUC with ties; NA if either class is empty
rpl_auc <- function(score, pos) {
  ok <- !is.na(score) & !is.na(pos); score <- score[ok]; pos <- pos[ok]
  n1 <- sum(pos); n0 <- sum(!pos)
  if (n1 == 0 || n0 == 0) return(NA_real_)
  r <- rank(score)
  (sum(r[pos]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

# threshold sweep: every observed score as a >= cutoff
rpl_sweep <- function(score, pos) {
  ok <- !is.na(score) & !is.na(pos); score <- score[ok]; pos <- pos[ok]
  cuts <- sort(unique(score))
  do.call(rbind, lapply(cuts, function(cu) {
    sel <- score >= cu
    tp <- sum(sel & pos); fp <- sum(sel & !pos); fn <- sum(!sel & pos)
    prec <- if (tp + fp) tp / (tp + fp) else NA
    rec  <- if (tp + fn) tp / (tp + fn) else NA
    f1   <- if (!is.na(prec) && !is.na(rec) && (prec + rec) > 0) 2 * prec * rec / (prec + rec) else NA
    data.frame(cutoff = cu, n_kept = tp + fp, tp = tp, fp = fp,
               precision = prec, recall_within_set = rec, f1 = f1)
  }))
}

# label-by-group table with small-cell suppression for sharing
rpl_label_table <- function(df, group_col, min_cell) {
  lv <- c("DS", "PDS", "NO")
  g <- ifelse(is.na(df[[group_col]]), "(missing)", as.character(df[[group_col]]))
  rows <- lapply(sort(unique(g)), function(k) {
    s <- df$label_n[g == k]
    c(group = k, n = length(s), DS = sum(s == "DS", na.rm = TRUE),
      PDS = sum(s == "PDS", na.rm = TRUE), NO = sum(s == "NO", na.rm = TRUE))
  })
  t <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  for (v in c("n", lv)) t[[v]] <- as.integer(t[[v]])
  t$share_positive <- round((t$DS + t$PDS) / t$n, 3)
  t$share_ds       <- round(t$DS / t$n, 3)
  t
}

rpl_suppress <- function(t, min_cell) {
  lv <- c("DS", "PDS", "NO"); out <- t
  for (i in seq_len(nrow(t))) {
    vals <- t[i, lv]; hide <- vals > 0 & vals < min_cell
    if (sum(hide) == 1) {                      # complementary suppression
      rest <- which(!hide & vals > 0)
      if (length(rest)) hide[rest[which.min(unlist(vals[rest]))]] <- TRUE
    }
    for (j in which(hide)) out[i, lv[j]] <- NA
    if (any(hide) || t$n[i] < min_cell) {      # totals/shares would let cells be recovered
      out$n[i] <- if (t$n[i] < min_cell) NA else t$n[i]
      out$share_positive[i] <- NA; out$share_ds[i] <- NA
    }
  }
  out$note <- ifelse(is.na(out$DS) | is.na(out$PDS) | is.na(out$NO) | is.na(out$n),
                     paste0("cells below ", min_cell, " suppressed"), "")
  out
}

main_report_review_labels <- function(cfg = CONFIG_REPORT) {
  dir.create(cfg$output_dir, showWarnings = FALSE, recursive = TRUE)
  d <- rpl_read(cfg$merged_path)
  
  id_c  <- rpl_pick(d, cfg$col_id, "review id")
  lab_c <- rpl_pick(d, cfg$col_label, "label")
  cf_c  <- rpl_pick(d, cfg$col_conf, "confidence", FALSE)
  fl_c  <- rpl_pick(d, cfg$col_flag, "flagged", FALSE)
  sk_c  <- rpl_pick(d, cfg$col_skip, "skipped", FALSE)
  nt_c  <- rpl_pick(d, cfg$col_note, "note", FALSE)
  vn_c  <- rpl_pick(d, cfg$col_vacancy, "vacancy number", FALSE)
  ti_c  <- rpl_pick(d, cfg$col_tier, "tier", FALSE)
  ca_c  <- rpl_pick(d, cfg$col_category, "category", FALSE)
  tt_c  <- rpl_pick(d, cfg$col_title, "title", FALSE)
  so_c  <- rpl_pick(d, cfg$col_score_overall, "section score", FALSE)
  ss_c  <- rpl_pick(d, cfg$col_score_skills, "section_skills score", FALSE)
  
  stopifnot(!anyDuplicated(d[[id_c]]))
  d$label_n <- rpl_norm_label(d[[lab_c]])
  bad <- is.na(d$label_n)
  if (any(bad)) rpl_warn(sum(bad), "rows with an unrecognised or empty label (kept, counted as unlabelled): ",
                         paste(unique(d[[lab_c]][bad]), collapse = ", "))
  d$flagged_b <- if (!is.na(fl_c)) rpl_bool(d[[fl_c]]) else FALSE
  d$skipped_b <- if (!is.na(sk_c)) rpl_bool(d[[sk_c]]) else FALSE
  d$conf_n    <- if (!is.na(cf_c)) suppressWarnings(as.numeric(d[[cf_c]])) else NA_real_
  d$pos <- if (cfg$positive_scheme == "ds_only") d$label_n == "DS" else d$label_n %in% c("DS", "PDS")
  d$pos[is.na(d$label_n)] <- NA
  if (!is.na(ti_c))  d$tier_g <- d[[ti_c]]
  if (!is.na(so_c))  d$s_overall <- suppressWarnings(as.numeric(d[[so_c]]))
  if (!is.na(ss_c))  d$s_skills  <- suppressWarnings(as.numeric(d[[ss_c]]))
  
  n <- nrow(d); np <- sum(d$pos, na.rm = TRUE)
  rpl_log(sprintf("== 1. OVERALL (n = %d) ==", n))
  print(table(Label = d$label_n, useNA = "ifany"))
  rpl_log(sprintf("positive scheme '%s': %d positive (%.1f%%) = precision of the whole review set",
                  cfg$positive_scheme, np, 100 * np / n))
  if (!is.na(cf_c)) { rpl_log("confidence by label:"); print(table(Label = d$label_n, Confidence = d$conf_n, useNA = "ifany")) }
  rpl_log("flagged:", sum(d$flagged_b), "| skipped:", sum(d$skipped_b))
  
  # ---- 2. by tier and category ---------------------------------------------
  agg <- list()
  if (!is.na(ti_c)) {
    rpl_log("== 2a. BY TIER ==")
    t <- rpl_label_table(d, "tier_g", cfg$min_cell); print(t)
    agg$tier <- cbind(table = "by_tier", rpl_suppress(t, cfg$min_cell))
  } else rpl_warn("no tier column: tier breakdown skipped")
  if (!is.na(ca_c)) {
    rpl_log("== 2b. BY POSITION CATEGORY ==")
    d$cat_g <- d[[ca_c]]
    t <- rpl_label_table(d, "cat_g", cfg$min_cell); print(t)
    agg$cat <- cbind(table = "by_category", rpl_suppress(t, cfg$min_cell))
  }
  
  # ---- 3. separation ---------------------------------------------------------
  sep <- NULL
  rpl_log("== 3. DO THE SCORES SEPARATE POSITIVES FROM NO-DS? (range-restricted: set is above threshold) ==")
  score_cols <- c(section = "s_overall", section_skills = "s_skills")
  score_cols <- score_cols[score_cols %in% names(d)]
  strata <- c(list(all = rep(TRUE, n)),
              if (!is.na(ti_c)) lapply(split(seq_len(n), d$tier_g), function(ix) seq_len(n) %in% ix))
  for (sc in names(score_cols)) for (st in names(strata)) {
    m <- strata[[st]] & !is.na(d$pos)
    a <- rpl_auc(d[[score_cols[[sc]]]][m], d$pos[m])
    rpl_log(sprintf("  AUC %-14s stratum %-12s n=%3d positives=%3d  AUC=%s", sc, st, sum(m), sum(d$pos[m]),
                    ifelse(is.na(a), "NA", sprintf("%.3f", a))))
    sep <- rbind(sep, data.frame(score = sc, stratum = st, n = sum(m), n_pos = sum(d$pos[m]),
                                 auc = round(a, 4), stringsAsFactors = FALSE))
    if (st == "all") {
      sw <- rpl_sweep(d[[score_cols[[sc]]]][m], d$pos[m])
      if (!is.null(sw) && nrow(sw)) {
        best <- sw[which.max(replace(sw$f1, is.na(sw$f1), -1)), ]
        rpl_log(sprintf("  best F1 within set for %s: cutoff %.4f keeps %d, precision %.3f, recall-within-set %.3f, F1 %.3f",
                        sc, best$cutoff, best$n_kept, best$precision, best$recall_within_set, best$f1))
        write.csv(sw, file.path(cfg$output_dir, sprintf("threshold_sweep_%s.csv", sc)),
                  row.names = FALSE, fileEncoding = "UTF-8")
      }
    }
  }
  if (!is.null(sep)) write.csv(sep, file.path(cfg$output_dir, "separation_auc.csv"),
                               row.names = FALSE, fileEncoding = "UTF-8")
  if (!length(score_cols)) rpl_warn("no score columns found: separation skipped")
  rpl_log("  Reading guide: AUC near 0.5 inside the set means raising the threshold would NOT",
          "buy precision; a raised cutoff only helps if the sweep shows precision rising while",
          "positives are kept. Do not compare these AUCs with the validation AUCs.")
  
  # ---- 4. by reporting unit ---------------------------------------------------
  if (!is.na(cfg$hr_linked_path) && file.exists(cfg$hr_linked_path) && !is.na(vn_c)) {
    h <- rpl_read(cfg$hr_linked_path)
    hv <- rpl_pick(h, cfg$hr_vacancy_col, "HR vacancy number", FALSE)
    hu <- rpl_pick(h, cfg$hr_unit_col, "HR reporting unit", FALSE)
    if (!is.na(hv) && !is.na(hu)) {
      h <- h[!is.na(h[[hu]]) & !is.na(h[[hv]]), ]
      u <- tapply(as.character(h[[hu]]), trimws(as.character(h[[hv]])), function(x) tail(x, 1))  # latest row
      d$unit_g <- unname(u[trimws(as.character(d[[vn_c]]))])
      d$unit_g[is.na(d$unit_g)] <- "(no HR link)"
      rpl_log("== 4. BY REPORTING UNIT (latest occupancy; censored at <=", cfg$unit_censor_upto, ") ==")
      t <- rpl_label_table(d, "unit_g", cfg$min_cell)
      t <- t[order(-t$n), ]
      cen <- t$n <= cfg$unit_censor_upto
      if (sum(cen) == 1) cen[which(!cen)[which.min(t$n[!cen])]] <- TRUE   # complementary
      shown <- t; shown$n <- as.character(shown$n)
      for (v in c("DS", "PDS", "NO")) shown[[v]] <- as.character(shown[[v]])
      shown[cen, c("n", "DS", "PDS", "NO")] <- paste0("<=", cfg$unit_censor_upto)
      shown$share_positive[cen] <- NA; shown$share_ds[cen] <- NA
      print(shown)
      write.csv(shown, file.path(cfg$output_dir, "labels_by_unit_censored.csv"),
                row.names = FALSE, fileEncoding = "UTF-8")
      write.csv(t, file.path(cfg$output_dir, "labels_by_unit_LOCAL_ONLY.csv"),
                row.names = FALSE, fileEncoding = "UTF-8")
    } else rpl_warn("HR linked file has no recognisable vacancy / unit column: unit table skipped")
  } else rpl_log("== 4. unit table skipped (no HR file, or no vacancy number in the merged file) ==")
  
  # ---- 5. flagged / low-confidence (LOCAL ONLY) -------------------------------
  keep <- c(id_c, vn_c, ti_c, tt_c, ca_c, "label_n", cf_c, nt_c, so_c, ss_c)
  keep <- keep[!is.na(keep)]
  fl <- d[d$flagged_b, keep, drop = FALSE]
  lc <- d[!is.na(d$conf_n) & d$conf_n <= cfg$low_conf_upto, keep, drop = FALSE]
  rpl_log(sprintf("== 5. FLAGGED %d | LOW CONFIDENCE (<= %d) %d | with a note %d ==", nrow(fl), cfg$low_conf_upto,
                  nrow(lc), if (!is.na(nt_c)) sum(!is.na(d[[nt_c]])) else 0))
  if (!is.na(ti_c)) { rpl_log("flagged by tier x label:"); print(table(Tier = d$tier_g[d$flagged_b], Label = d$label_n[d$flagged_b])) }
  write.csv(fl, file.path(cfg$output_dir, "flagged_items_LOCAL_ONLY.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(lc, file.path(cfg$output_dir, "low_confidence_items_LOCAL_ONLY.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  
  # ---- aggregate (shareable, suppressed) --------------------------------------
  if (length(agg)) {
    a <- do.call(rbind, agg)
    write.csv(a, file.path(cfg$output_dir, "labels_aggregate_suppressed.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  }
  rpl_log("done. Files in", cfg$output_dir,
          "| the console log above is NOT suppressed; share only the suppressed / censored CSVs.")
  invisible(list(data = d, separation = sep))
}

if (sys.nframe() == 0) {
  main_report_review_labels(CONFIG_REPORT)
}