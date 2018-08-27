expect_dict_equal <- function(x1, x2) {
    # test whether two lists are "equal" up to re-ordering
    # like setequal, but also looks at element names
    actual_query <- x1[sort(names(x1))]
    expected_query <- x2[sort(names(x2))]
    expect_equal(actual_query, expected_query)

}

mz_setup <- function() {
    hosts <- c("search", "matrix", "tile")
    gh <- function(x) tryCatch(mz_get_host(x), error = function(e) NULL)
    old_opts <- structure(lapply(hosts, gh),
                          names = hosts)
    suppressWarnings(
        lapply(hosts,
               mz_set_host,
               provider = mz_provider("www.example.com",
                                      path = "not/a/real/path",
                                      key = "api-key"))
    )
    old_opts
}

mz_teardown <- function(old_opts) {
    suppressWarnings(
        Map(mz_set_host,
            which = names(old_opts),
            provider = old_opts)
    )
}
test_that <- function(...) {
    old_opts <- mz_setup()
    testthat::test_that(...)
    mz_teardown(old_opts)
}

expect_tiles_contain_xy <- function(object, x, y) {
    act <- quasi_label(rlang::enquo(object))

    xs <- vapply(object, function(.) .[["x"]], numeric(1))
    ys <- vapply(object, function(.) .[["y"]], numeric(1))

    expect(
        x %in% xs && y %in% ys,
        sprintf("%s does not contain tile (%i, %i)", act$lab, x, y)
    )
    invisible(act$val)
}
