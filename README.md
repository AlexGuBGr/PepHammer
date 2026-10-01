# PepHammer

Search and compare multiple peptides against a comprehensive database of bioactive peptides.
This repository contains the Shiny app only.

[![tests](https://github.com/AlexGuBGr/PepHammer/actions/workflows/tests.yml/badge.svg)](https://github.com/AlexGuBGr/PepHammer/actions/workflows/tests.yml)

## Databases

The app searches the following databases (see `global.R`):

| Selection | Database file |
|---|---|
| Peptipedia | `db/peptipedia_bio_strings.sqlite` |
| Peptipedia\|Tissue | `db/peptipedia_bio_strings.sqlite` |
| MultiPep | `db/pepti_multipep.sqlite` |
| MultiPep\|Tissue | `db/multipep_tissue.sqlite` |
| NeuroPep_v2 | `db/neuropepv2_one.sqlite` |

## Running the app

Install the R packages used by the app (`shiny`, `bslib`, `RSQLite`, `DBI`, `plotly`, `future`, `promises`, `future.callr`, `stringr`, `matrixStats`, `purrr`), then start it from the repository root:

```r
shiny::runApp()
```

## Repository layout

| Path | Contents |
|---|---|
| `global.R`, `ui.R`, `server.R` | Shiny app |
| `R/` | Search, statistics and database helper functions |
| `python/` | Scripts used to build and prepare the databases |
| `db/` | SQLite databases |
| `www/` | Images used by the app |
| `tests/` | Python (pytest) and R (testthat) tests |

## Tests

Python (from the repository root):

```bash
pip install pandas numpy pytest
python -m pytest tests/python -v
```

R (requires `testthat`, `DBI`, `RSQLite`, `stringr`, `matrixStats`, `purrr`):

```bash
Rscript tests/testthat.R
```

The tests use small temporary databases, so they do not need the files in `db/`.
They run automatically on every push and pull request through GitHub Actions
(Python 3.10 and 3.11, R 4.5.2).

## Citation

If you find PepHammer useful, please cite the
[preprint on bioRxiv](https://www.biorxiv.org/content/10.64898/2026.04.13.718252v1).

## License

See [LICENSE](LICENSE).
