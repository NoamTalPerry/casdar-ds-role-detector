# Earlier master template versions

This folder holds the earlier versions of the CaSDaR master template that were tested while the template was being developed, so that the template-version comparison behind our results can be inspected and reproduced. The files are named after the runs of `run_master_template_version.R` that used them.

**The current version is not in this folder.** Master template v5 (used in run `v7_items_update`) is deposited in Apollo, the University of Cambridge's institutional repository, which is its canonical copy:

> Tal-Perry, N., Clugston, K., & Castle, C. (2026). *CaSDaR Master Template (v5): A coding scheme of responsibilities and skills for identifying data steward roles in job advertisements* [Data set]. Apollo - University of Cambridge Repository. https://doi.org/10.17863/CAM.133662

## Which file each run used

Settings are `similarity_scoring.R` `CONFIG` options, set through `run_master_template_version.R`'s `sim_config_overrides`. All of them default to `FALSE`; "none" means every setting was left at its default.

| Run | Template file | Settings |
|---|---|---|
| `v1_base` | `v1_base.txt` | none |
| `v2_gen_resp` | `v1_base.txt` | `apply_general_pool_to_responsibilities = TRUE` |
| `v3_adjrole` | `v3_adjrole.txt` | `use_adjacent_pool = TRUE` |
| `v4_adjrole_resp` | `v3_adjrole.txt` | `use_adjacent_pool = TRUE`, `apply_adjacent_pool_to_responsibilities = TRUE` |
| `v5_no_general` | `v5_no_general.txt` | none |
| `v6_adj_incorporated` | `v6_adj_incorporated.txt` | none |
| `v7_items_update` | master template v5 (Apollo, above) | `use_adjacent_pool = TRUE` |

## What changes between the files

| File | Responsibilities | Essential skills | Desirable skills | GENERAL | ADJACENT_ROLE |
|---|---|---|---|---|---|
| `v1_base.txt` | 27 | 28 | 16 | 27 | - |
| `v3_adjrole.txt` | 27 | 28 | 16 | 27 | 10 |
| `v5_no_general.txt` | 27 | 28 | 16 | - | - |
| `v6_adj_incorporated.txt` | 27 | 38 | 16 | 27 | - |
| master template v5 (Apollo) | 27 | 34 | 13 | 27 | 7 |

- **`v1_base.txt`**: the starting template.
- **`v3_adjrole.txt`**: `v1_base` plus an ADJACENT_ROLE pool of 10 new items that read as data-steward-adjacent but more often describe a different role (e.g. data engineer, project manager), kept separate so their effect can be measured.
- **`v5_no_general.txt`**: `v1_base` without the GENERAL pool, to measure the effect of excluding generic job-ad content from scoring.
- **`v6_adj_incorporated.txt`**: `v1_base` with the 10 items from `v3_adjrole`'s ADJACENT_ROLE pool added to the essential skills, i.e. scored as core criteria.
- **Master template v5 (Apollo)**: 7 of those 10 items are essential skills. The ADJACENT_ROLE pool holds the other 3 plus 4 items moved out of the core lists (one essential skill on communicating technical concepts to non-technical audiences, and three desirable skills: source-code hosting, cloud platforms and wiki platforms).

The format is the one `similarity_scoring.R` expects (see `parse_master_template()`); item numbers are cosmetic. Item IDs differ between versions, so compare items across versions by their text.

## License

The template files in this folder are released under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/), not under the repository's MIT License, which covers the code only. Authors: Noam Tal-Perry, Kim Clugston and Clair Castle (University of Cambridge). If you use a version from this folder, please cite the Apollo deposit above and name the version you used.
