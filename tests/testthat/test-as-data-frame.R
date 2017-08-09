context("as.data.frame")

test_that("as.data.frame converts search results to data frame", {
    oakland_public_df <- as.data.frame(oakland_public)
    expect_is(oakland_public_df, "data.frame")
    expect_equal(nrow(oakland_public_df), length(oakland_public$features))
})

test_that("as.data.frame converts isochrone results to sf", {
    marina_walks_df <- as.data.frame(marina_walks)
    expect_is(marina_walks_df, "data.frame")
    expect_equal(length(unique(marina_walks_df$contour)), length(marina_walks$features))
    num_features <- vapply(marina_walks$features, function(x) length(x$geometry$coordinates), FUN.VALUE = integer(1))
    num_features <- sum(num_features)
    expect_equal(nrow(marina_walks_df), num_features)
})

test_that("as.data.frame errors on empty search results", {
    empty_search <- oakland_public
    empty_search$features <- list()
    expect_error(as.data.frame(empty_search), "no data")
})

