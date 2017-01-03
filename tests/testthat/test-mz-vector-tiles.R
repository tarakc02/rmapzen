context("vector-tiles")

test_that("vector tile urls built correctly", {
    # 16/19293/24641.json
    url <- vector_url(x = 19293, y  = 24641, z = 16,
                   format = "json", api_key = "abc")

    expect_identical(url$path, "mapzen/vector/v1/all/16/19293/24641.json")
    expect_identical(url$query$api_key, "abc")
})
