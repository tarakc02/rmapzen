context("usage-statistics")

test_that("Usage statistics are updated when header contains new information", {
    stats <- usage_recorder()

    header <- list(
        date = "Fri, 06 Jan 2017 19:15:25 GMT",
        `x-cache` = "MISS",
        `x-apiaxleproxy-qpd-left` = 100L,
        `x-apiaxleproxy-qps-left` = 5L
    )

    stats("update_search")(header)
    search_miss <- stats("view")()
    expect_length(search_miss, 3L)
    search_stats <- search_miss$search
    expect_equal(search_stats$last_updated, "Fri, 06 Jan 2017 19:15:25 GMT")
    expect_equal(search_stats$remaining_day, 100)
    expect_equal(search_stats$remaining_second, 5)
    expect_true(
        setequal(names(search_miss), c("search", "matrix", "tile"))
    )
    prev <- search_miss

    # no updates if cache hit
    header <- list(
        date = "Fri, 06 Jan 2017 19:15:25 GMT",
        `x-cache` = "HIT",
        `x-apiaxleproxy-qpd-left` = 12,
        `x-apiaxleproxy-qps-left` = 0
    )
    stats("update_search")(header)
    # nothing should have updated
    expect_equal(stats("view")(), prev)

    newheader <- list(
        date = "Fri, 06 Jan 2017 19:15:25 GMT",
        `x-apiaxleproxy-qpd-left` = 100L,
        `x-apiaxleproxy-qps-left` = 5L
    )

    stats("update_matrix")(newheader)
    stats("update_tile")(newheader)
    updated_stats <- stats("view")()
    expect_equal(updated_stats$matrix$last_updated, "Fri, 06 Jan 2017 19:15:25 GMT")
    expect_equal(updated_stats$matrix$remaining_day, 100)
    expect_equal(updated_stats$matrix$remaining_second, 5)

    expect_equal(updated_stats$tile$last_updated, "Fri, 06 Jan 2017 19:15:25 GMT")
    expect_equal(updated_stats$tile$remaining_day, 100)
    expect_equal(updated_stats$tile$remaining_second, 5)
    prev <- updated_stats

    # tiles shouldn't update unless there is a full header of info
    tile_cache_header <- list(
        `x-apiaxleproxy-qpd-left` = 18
    )
    stats("update_tile")(tile_cache_header)
    expect_equal(stats("view")(), prev)

})
