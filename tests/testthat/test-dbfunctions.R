test_that("list_tables returns the tables of the database", {
    db <- make_test_db()
    expect_equal(list_tables(db), "peptipedia")
})

test_that("get_row_count counts rows", {
    db <- make_test_db()
    expect_equal(get_row_count("peptipedia", db)$n, 6)
})

test_that("get_countdb counts rows of a given length", {
    db <- make_test_db()
    expect_equal(get_countdb(db, "peptipedia", 4)[[1]], 5)
})

test_that("get_column_sums sums numeric columns", {
    db <- make_test_db()
    expect_equal(get_column_sums("peptipedia", "Length", db)$Length, 4 * 5 + 5)
})

test_that("load_entire_table returns data, and NULL for a missing table", {
    db <- make_test_db()
    expect_equal(nrow(load_entire_table("peptipedia", db)), 6)
    expect_null(load_entire_table("missing", db))
})

test_that("load_columns_from_table returns only requested columns", {
    db <- make_test_db()
    res <- load_columns_from_table(c("peptide", "Length"), "peptipedia", db)
    expect_equal(colnames(res), c("peptide", "Length"))
    expect_null(suppressMessages(load_columns_from_table("nope", "peptipedia", db)))
})

test_that("load_columns_from_table_where filters by value set", {
    db <- make_test_db()
    res <- load_columns_from_table_where("*", "peptipedia", "peptide", c("AAAA", "GGGG"), db)
    expect_setequal(res$peptide, c("AAAA", "GGGG"))
    res2 <- load_columns_from_table_where("peptide", "peptipedia", "Length", 5, db)
    expect_equal(res2$peptide, "AAAAA")
})

test_that("load_columns_from_table_where_NOT_OU skips peptides with O or U", {
    db <- make_test_db()
    res <- load_columns_from_table_where_NOT_OU("peptide", "peptipedia", 4, db)
    expect_setequal(res$peptide, c("AAAA", "ACDE", "GGGG"))
})

test_that("add_or_replace_table_in_db writes and overwrites a table", {
    db <- make_test_db()
    add_or_replace_table_in_db(data.frame(x = 1:3), "newtab", db)
    expect_true("newtab" %in% list_tables(db))
    add_or_replace_table_in_db(data.frame(x = 1), "newtab", db)
    expect_equal(nrow(load_entire_table("newtab", db)), 1)
})

test_that("append_table appends rows", {
    db <- make_test_db()
    add_or_replace_table_in_db(data.frame(x = 1:3), "newtab", db)
    append_table("newtab", data.frame(x = 4L), db)
    expect_equal(nrow(load_entire_table("newtab", db)), 4)
})

test_that("rename_table and delete_table modify the schema", {
    db <- make_test_db()
    rename_table("peptipedia", "renamed", db)
    expect_equal(list_tables(db), "renamed")
    delete_table("renamed", db)
    expect_length(list_tables(db), 0)
})

test_that("set_busytimeout configures the connection", {
    db <- make_test_db()
    con <- DBI::dbConnect(RSQLite::SQLite(), db)
    on.exit(DBI::dbDisconnect(con))
    set_busytimeout(con, 1234)
    expect_equal(DBI::dbGetQuery(con, "PRAGMA busy_timeout;")[[1]], 1234)
})
