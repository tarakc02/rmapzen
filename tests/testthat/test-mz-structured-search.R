context("structured search")

test_that("Notting Hill structured search url built correctly", {
    url <- build_structured_search_url(
        neighbourhood = "Notting Hill",
        locality = "London",
        api_key = "abc"
    )
    expect_match(url$path, "/structured$")
    expect_identical(url$query$neighbourhood, "Notting Hill")
    expect_identical(url$query$locality, "London")
    expect_identical(url$query$api_key, "abc")
})

test_that("Bangkok structured search URL built correctly", {
    url <- build_structured_search_url(
        locality = "Bangkok",
        country = "Thailand",
        api_key = "abc"
    )
    expect_identical(url$query$locality, "Bangkok")
    expect_identical(url$query$country, "Thailand")
})

test_that("Structured search with postalcode and country works", {
    url <- build_structured_search_url(
        postalcode = "94609",
        country = "USA",
        api_key = "abc",
        size = 1
    )
    expect_dict_equal(
        url$query, list(
            postalcode = "94609",
            country = "USA",
            api_key = "abc",
            size = 1
        )
    )
})

test_that("Structured search allows general search parameters", {
    url <- build_structured_search_url(
        locality = "Berkeley",
        country = "USA",
        size = 1,
        focus.point = c(lat = 1, lon = 2),
        api_key = "abc"
    )
    expect_dict_equal(
        url$query, list(
            api_key = "abc",
            locality = "Berkeley",
            country = "USA",
            size = 1,
            focus.point.lat = 1,
            focus.point.lon = 2
        )
    )
})

test_that("Structure search gives errors when not enough info", {
    expect_error(build_search_url())
    expect_error(build_search_url(postalcode = 12356))
})
