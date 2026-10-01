# Locate the project root regardless of where testthat is invoked from
root <- normalizePath(file.path(testthat::test_path(), "..", ".."))

source(file.path(root, "R", "dbfunctions.R"))
source(file.path(root, "R", "functions.R"))

aa_levels <- c('D','Q','S','A','P','R','C','E','F','G','H','I','K','L','M','N','T','V','W','Y','X','B','J','Z','O','U')

# Small throw-away database used by the tests
make_test_db <- function() {
    path <- tempfile(fileext = ".sqlite")
    con <- DBI::dbConnect(RSQLite::SQLite(), path)
    on.exit(DBI::dbDisconnect(con))
    DBI::dbWriteTable(con, "peptipedia", data.frame(
        peptide       = c("AAAA", "ACDE", "GGGG", "AOAA", "AAUA", "AAAAA"),
        Length        = c(4, 4, 4, 4, 4, 5),
        bioactivities = c("Hormone", "Hormone;Toxic", "Toxic", "Hormone", "Hormone", "Toxic"),
        stringsAsFactors = FALSE
    ))
    path
}
