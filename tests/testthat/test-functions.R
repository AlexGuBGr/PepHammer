test_that("grantham matrix is symmetric with zero diagonal", {
    G <- grantham_distance_matrix()
    expect_equal(dim(G), c(20, 20))
    expect_equal(G, t(G))
    expect_true(all(diag(G) == 0))
    expect_equal(rownames(G), colnames(G))
})

test_that("grantham matrix has known published values", {
    G <- grantham_distance_matrix()
    expect_equal(G["S", "R"], 110)
    expect_equal(G["L", "I"], 5)
    expect_equal(G["C", "W"], 215)
})

test_that("extend_grantham adds X, B, Z and J", {
    G <- grantham_distance_matrix()
    E <- extend_grantham(G)
    expect_equal(dim(E), c(24, 24))
    expect_true(all(c("X", "B", "Z", "J") %in% rownames(E)))
    expect_equal(E["X", "X"], mean(G))
    expect_equal(E["X", "A"], mean(G[, "A"]))
    expect_equal(E["B", "D"], G["D", "N"] / 2)
    expect_equal(E["Z", "E"], G["E", "Q"] / 2)
    expect_equal(E["J", "L"], G["I", "L"] / 2)
    expect_equal(E["B", "A"], (G["D", "A"] + G["N", "A"]) / 2)
})

test_that("build_compat_matrix handles ambiguous residues", {
    cm <- build_compat_matrix(aa_levels)
    expect_equal(dim(cm), rep(length(aa_levels), 2))
    ix <- function(a) which(aa_levels == a)
    expect_true(all(diag(cm)))
    expect_true(cm[ix("B"), ix("D")] && cm[ix("D"), ix("B")])
    expect_true(cm[ix("B"), ix("N")])
    expect_true(cm[ix("Z"), ix("E")] && cm[ix("Z"), ix("Q")])
    expect_true(cm[ix("J"), ix("I")] && cm[ix("J"), ix("L")])
    expect_false(cm[ix("A"), ix("D")])
    expect_false(cm[ix("B"), ix("A")])
    # X matches everything
    expect_true(all(cm[ix("X"), ]))
    expect_true(all(cm[, ix("X")]))
})

test_that("encode_matrix and decode_matrix round-trip", {
    mat <- matrix(c("A", "C", "D", "E", "X", "G"), nrow = 2, byrow = TRUE)
    enc <- encode_matrix(NULL, mat, aa_levels)
    expect_equal(enc[1, 1], which(aa_levels == "A"))
    expect_equal(dim(enc), dim(mat))
    expect_equal(decode_matrix(enc, aa_levels), mat)
})

test_that("encode_matrix loads strings from the database", {
    db <- make_test_db()
    mat <- encode_matrix(db, 4, aa_levels, gett = "peptipedia", str_only = TRUE, rmOU = TRUE)
    expect_equal(ncol(mat), 4)
    expect_setequal(apply(mat, 1, paste0, collapse = ""), c("AAAA", "ACDE", "GGGG"))
})

test_that("check_length keeps valid peptides only", {
    x <- c("A", "AC", strrep("A", 150), strrep("A", 151), "XXAAAAAAAA", "XXXAAAAAAA", "AXXXX")
    # length 1 and > 150 dropped; more than 20 % X dropped
    expect_equal(check_length(x, NULL), c("AC", strrep("A", 150), "XXAAAAAAAA"))
})

test_that("exapnd_to_table converts the bioactivity string into indicator columns", {
    df <- data.frame(peptide = c("AA", "CC"),
                     bioactivities = c("Hormone;Toxic", "Toxic"),
                     stringsAsFactors = FALSE)
    res <- exapnd_to_table(df, c("Hormone", "Toxic", "Other"))
    expect_equal(colnames(res), c("peptide", "Hormone", "Toxic", "Other"))
    expect_equal(res$Hormone, c(1, 0))
    expect_equal(res$Toxic, c(1, 1))
    expect_equal(res$Other, c(0, 0))
})

test_that("best_matches_fast finds the exact match", {
    db <- make_test_db()
    cm <- build_compat_matrix(aa_levels)
    res <- best_matches_fast("ACDE", 4, cm, aa_levels, TRUE, db, "peptipedia")
    expect_equal(res$Query[1], "ACDE")
    expect_equal(res$Found[1], "ACDE")
    expect_equal(res$Score[1], 1)
    expect_equal(res$Miss_count[1], 0)
    expect_equal(res$Length[1], 4)
})

test_that("best_matches_fast reports mismatches for an inexact query", {
    db <- make_test_db()
    cm <- build_compat_matrix(aa_levels)
    res <- best_matches_fast("ACDF", 4, cm, aa_levels, TRUE, db, "peptipedia")
    expect_equal(res$Found[1], "ACDE")
    expect_equal(res$Miss_count[1], 1)
    expect_equal(res$Score[1], 0.75)
})

test_that("best_matches_fast returns an empty frame when no peptides of that length exist", {
    db <- make_test_db()
    cm <- build_compat_matrix(aa_levels)
    res <- best_matches_fast("ACD", 3, cm, aa_levels, TRUE, db, "peptipedia")
    expect_equal(nrow(res), 0)
})

test_that("best_matches_fast_grantham picks the lowest-distance peptide", {
    db <- make_test_db()
    grantm <- extend_grantham(grantham_distance_matrix())
    # add_extra() and encode_matrix() read these as globals, as in the app
    assign("grantm", grantm, envir = globalenv())
    assign("aa_levels", aa_levels, envir = globalenv())
    on.exit(rm(list = c("grantm", "aa_levels"), envir = globalenv()))

    res <- best_matches_fast_grantham("ACDE", 4, grantm, TRUE, db, "peptipedia")
    expect_equal(res$Found[1], "ACDE")
    expect_equal(res$Distance[1], 0)
    expect_equal(res$Miss_count_exact[1], 0)
    expect_equal(res$Percent_of_worst[1], 0)
})
