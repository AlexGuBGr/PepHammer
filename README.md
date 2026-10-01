# PepHammer
Search and compare multiple peptides against a comprehensive database of bioactive peptides.
This is only the Shiny app.


## Tests

Python (from the repository root):

```
pip install pandas numpy pytest
python -m pytest tests/python
```

R (requires `testthat`, `DBI`, `RSQLite`, `stringr`, `matrixStats`):

```
Rscript tests/testthat.R
```
