# CaSDaR: Data Steward Job-Ad Analysis Pipeline

This repository contains the analysis code developed for the CaSDaR sub-project *"Evidencing the impact of data stewardship roles using administrative and scientometrics data"*, an R pipeline that identifies and measures data-steward job responsibilities and skills in job-advertisement text using embedding-based similarity scoring against a master template of data-steward criteria.

> **Status:** the master template is deposited in Apollo, the University of Cambridge's institutional repository ([doi:10.17863/CAM.133662](https://doi.org/10.17863/CAM.133662); see [Master template](#master-template-data-deposit) below). A citable DOI for this code will be added once a first release is minted (see [Citation](#citation)).

## About the project

[CaSDaR](https://casdar.ac.uk/) (Careers and Skills for Data-driven Research) is a UKRI-funded Network+ (Grant No. UKRI739). This repository holds the code for one CaSDaR-funded sub-project, based at the University of Cambridge:

- **Principal Applicant:** Noam Tal-Perry, University of Cambridge ([ORCID](https://orcid.org/0000-0003-2521-9546))
- **Co-Applicants:** Clair Castle ([ORCID](https://orcid.org/0000-0002-6897-582X)), Kim Clugston ([ORCID](https://orcid.org/0009-0000-5106-4555)), Becky Ioppolo ([ORCID](https://orcid.org/0000-0003-1301-7947)), Steven Wooding ([ORCID](https://orcid.org/0000-0002-8036-1054))— all University of Cambridge

## Acknowledgements

This work was funded by the CaSDaR (Careers and Skills for Data-driven Research) Network+, UKRI-funded (Grant No. UKRI739). The CaSDaR Network+ is led by Principal Investigator Dr Samantha Pearman-Kanza ([ORCID](https://orcid.org/0000-0002-4831-9489)), with Co-Investigators Professor James Baker ([ORCID](https://orcid.org/0000-0002-2682-6922)), Professor Simon Coles, Professor Simon Hettrick, and Ms Isobel Stark, and Network+ Coordinator Dr Louise Saul ([ORCID](https://orcid.org/0009-0000-6422-0342)).

## What this repository contains

This repository contains the pipeline's **code** (`Scripts/`) and the **earlier master template versions** tested during the template's development (`templates/`). It does not include:
- job advertisement PDFs or extracted text,
- the underlying HR/recruitment data used to enrich extracted records,
- any scored output (similarity scores, coverage summaries, threshold-analysis results), or
- the current master template of data-steward criteria, which is deposited separately (see [Master template](#master-template-data-deposit) below).

The purpose of this repository is to promote transparency and reusability of this project. 
Every script's own header comment describes exactly what inputs it expects and what it does and doesn't touch — check the script before running it. Each output is described within the script.

## Pipeline overview

The fourteen scripts in this repository form two connected pipelines plus a small set of comparison/versioning tools:

**Main extraction → scoring → coverage pipeline**
1. `extract_job_criteria.R` — extracts responsibilities/skills criteria from job-ad PDFs via the Claude API, groups reposted vacancies, and optionally enriches records with HR status data.
2. `run_validation_batch.R` — runs `extract_job_criteria.R` against a separate, external validation corpus (job ads outside the main study corpus), fully isolated from the main pipeline's inputs/outputs.
3. `diagnose_failed_vacancies.R` — explains why vacancy groups are marked `failed` or `partial` (scanned PDFs needing OCR, garbled font encoding, truncated or unparseable responses, API errors, postings skipped by the category filter), using the run logs, the files on disk and an optional probe of the PDFs. Read-only by default.
4. `similarity_scoring.R` — scores each vacancy's extracted text against a master template of data-steward criteria using local embeddings (via [Ollama](https://ollama.com)), at document/section/item granularity.
5. `analyze_item_coverage.R` — aggregates item-level match data across vacancies to show which master-template criteria are reliably matched in real job ads.
6. `run_item_coverage_by_label.R` — wraps `analyze_item_coverage.R` to run it once per labelled group (e.g. confirmed data-steward roles vs. non-data-steward roles), deriving each group's vacancy list from a labelled sample.
7. `triangulate_item_coverage.R` — joins multiple `analyze_item_coverage.R` outputs together to compare coverage across groups and estimate a per-item discrimination gap.

**Validation sampling → reliability → threshold → review-set pipeline**
8. `draw_negative_validation_sample.R` — draws a reproducible, stratified sample of vacancies for manual labelling, oversampling near the empirical decision boundary.
9. `export_reliability_recheck.R` — produces a second, blind review pass over a subset of the drawn sample, for inter-rater or self-reliability checking, and resolves the passes (with an optional adjudication pass) into final labels.
10. `run_threshold_analysis.R` — builds the weighted ROC/precision-recall picture from the labelled sample plus an external positive validation set, to support choosing an operating threshold.
11. `export_review_set.R` — applies the chosen thresholds to the full corpus's scores to build the set of candidate data-steward vacancies for manual review (a strict and a lenient tier).
12. `report_review_labels.R` — once the review set has been labelled, reports the label shares by tier and position category (the real-corpus precision of each tier) and how well the scores order positives within the set, with small-cell suppression on every shareable output.

**Versioning / comparison tools**
13. `run_master_template_version.R` — a general wrapper that scores a candidate master template end-to-end (both corpora, plus the full downstream analysis chain) into one dedicated, versioned output folder, for testing template variants.
14. `compare_master_template_versions.R` — compares two or more completed `run_master_template_version.R` runs side by side (AUC, threshold-sweep summaries, plots).

Each script can also be run standalone; see its own header comment for the exact input/output contract and a fuller design rationale.

### Which parts are designed for reuse

The scripts fall into two groups:

- **General-purpose tools.** `extract_job_criteria.R` (with `run_validation_batch.R`, and `diagnose_failed_vacancies.R` to explain any failed extractions) and `similarity_scoring.R` are written to be reused. Point them at your own job-ad PDFs and your own master template, and they extract criteria and score each ad against it. All project-specific settings live in each script's `CONFIG` block.
- **Project-specific analysis.** The remaining scripts cover sampling, the reliability and adjudication workflow, threshold analysis, building and reporting on the manual review set, item-coverage analysis and template-version comparison. They implement the validation design used in this project: a score-stratified sample from our own corpus, blind manual labelling with a second pass and adjudication, and an external positive set. They are shared for transparency, so the analysis behind our results can be inspected and reproduced. They should run on comparable inputs, but they assume our file layout, label scheme (DS / Partial DS / No-DS) and sampling design, so you may need to adapt them rather than run them as they are.

## Requirements

- **R** (developed against a recent R release; no specific minimum version is enforced by the code)
- R packages: `pdftools`, `httr2`, `jsonlite`, `stringr`, `digest`, `yaml` (used across different scripts — each script's header lists exactly what it needs)
- **[Ollama](https://ollama.com)**, running locally, with the `nomic-embed-text` model pulled (`ollama pull nomic-embed-text`) — used by `similarity_scoring.R` for local embedding
- An **Anthropic API key**, for `extract_job_criteria.R`'s PDF-to-text extraction step (set as an environment variable; see that script's header for the exact variable name and handling)

## Getting started

These scripts expect to live in their own subfolder, with sibling data/output folders one level up (every path in this pipeline is written relative to `..`). A typical layout looks like this once you've supplied your own data:

```
your-project-root/
├── Scripts/                        
│   ├── extract_job_criteria.R
│   ├── run_validation_batch.R
│   ├── similarity_scoring.R
│   └── ...                          (the rest of the .R files)
├── Data/                             <- your master_template.txt, HR export, etc. (not in this repo)
├── PDFs/                              <- your main-corpus job-ad PDFs (not in this repo)
├── Validation_PDFs/                    <- your external validation job-ad PDFs (not in this repo)
├── TXTs/, TXTs_extracted/, TXTs_merged/            <- created by extract_job_criteria.R
├── Validation_TXTs/, ..._extracted/, ..._merged/    <- created by run_validation_batch.R
├── Output/                                           <- created by similarity_scoring.R and downstream scripts
└── Validation_Output/                                 <- created by the validation-side equivalents
```

**Typical run order**, starting from your own PDFs and master template:

1. Place your job-ad PDFs under `PDFs/` (and, separately, any external validation-set PDFs under `Validation_PDFs/`); place your master template at `Data/master_template.txt` (see `similarity_scoring.R`'s `parse_master_template()` for the required format).
2. `Rscript extract_job_criteria.R` — extracts and merges criteria from the main corpus. If some vacancy groups end up `failed` or `partial`, `Rscript diagnose_failed_vacancies.R` explains why before you decide whether to re-run, raise `max_tokens` or OCR the PDFs.
3. `Rscript run_validation_batch.R` — does the same for the external validation corpus, fully isolated from step 2.
4. `Rscript similarity_scoring.R` — score the main corpus (`CONFIG$validation_mode = FALSE`) and, separately, the validation corpus (`CONFIG$validation_mode = TRUE`) against the master template. `run_master_template_version.R` (step 9) can do both of these in one wrapped run if you're testing a template version rather than running the base pipeline.
5. `Rscript draw_negative_validation_sample.R` → label the resulting blind review file → `Rscript export_reliability_recheck.R` → label pass 2 → call `compare_passes()` for an agreement figure. If disagreements go to an adjudicator, have them label a pass-3 file covering the disputed rows, then call `compare_passes()` again with it and `build_final_labels()` to produce one final label per vacancy. `run_threshold_analysis.R` uses these final labels by default; set its `label_source = "pass1"` if you did a single labelling pass.
6. `Rscript run_threshold_analysis.R` — once the labelled sample from step 5 is complete, builds the ROC/precision-recall picture for threshold selection.
7. `Rscript export_review_set.R` — applies the chosen thresholds to the main corpus's scores to build the review set; review it blind (as in step 5), join the labels back to the review set, then `Rscript report_review_labels.R` for the label shares and precision of each tier.
8. `Rscript analyze_item_coverage.R` (or `run_item_coverage_by_label.R` to run it per labelled group) → `Rscript triangulate_item_coverage.R` to compare groups and estimate per-item discrimination.
9. To test a candidate master template version end-to-end: `Rscript run_master_template_version.R`, then `Rscript compare_master_template_versions.R` once you have two or more completed version runs to compare.

Each script's own header comment is the authoritative reference for its exact CONFIG options and output files.

## Master template (data deposit)

The master template of data-steward responsibilities and skills that `similarity_scoring.R` scores job ads against (current version: v5 — 27 responsibilities, 47 skills split into 34 essential and 13 desirable, plus supplementary pools of 27 general and 7 adjacent-role items) is a research instrument in its own right and is deposited separately, not included in this code repository. It is available from the University of Cambridge's institutional repository, [Apollo](https://www.repository.cam.ac.uk/), together with a README describing its structure and how it was developed. If you use the template, please cite it as:

> Tal-Perry, N., Clugston, K., & Castle, C. (2026). *CaSDaR Master Template (v5): A coding scheme of responsibilities and skills for identifying data steward roles in job advertisements* [Data set]. Apollo - University of Cambridge Repository. https://doi.org/10.17863/CAM.133662

The deposited file follows the format `similarity_scoring.R` expects (see `parse_master_template()`), so it can be used directly as `Data/master_template.txt`.

The earlier versions tested during the template's development are in [`templates/`](templates/), named after the `run_master_template_version.R` runs that used them. Its README maps each run to its template file and scoring settings, and describes what changes between versions.

## Development notes

Portions of this codebase — including code structure, documentation, and the cleanup applied for this public release — were drafted with the assistance of Claude Sonnet 5 (Anthropic) between July and August 2026, and extended with the assistance of Claude Sonnet 5.5 (Anthropic) between October and November 2026. All code was reviewed, tested, and verified by the author, who takes full responsibility for its correctness and behaviour.

## License

- **Code** (all `.R` files in this repository): [MIT License](LICENSE)
- **Master template and other research-instrument/data deposits**: released separately under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/)
- **Earlier master template versions** (`templates/`): [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/), as described in that folder's README

## Citation

A citable DOI for this code will be minted via GitHub's integration with [Zenodo](https://zenodo.org) once the repository is public and a first release is tagged. This section will be updated with the DOI and a suggested citation at that point.

## Contact

Dr Noam Tal-Perry — [nt490@cam.ac.uk](mailto:nt490@cam.ac.uk)