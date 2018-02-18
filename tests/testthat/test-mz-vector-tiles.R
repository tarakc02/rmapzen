context("vector-tiles")

test_that("vector tile urls built correctly", {
    # 16/19293/24641.json
    url <- vector_url(x = 19293, y  = 24641, z = 16,
                   format = "json", api_key = "abc")

    expect_match(url$path, "/all/16/19293/24641.json")
    expect_identical(url$query$api_key, "abc")
})

test_that("single tiles can be pulled", {
    skip("skipping single-tile test, requires access to external resource")
    single_tile <- mz_vector_tiles(mz_tile_coordinates(19293, 24641, 16))
    expect_is(single_tile, "mapzen_vector_tiles")
    expect_length(single_tile, 9L)
    expect_is(single_tile[[1]], "mapzen_vector_layer")
})

test_that("multiple contiguous tiles can be pulled", {
    skip("skipping multiple contiguous tiles test, requires access to external resource")
    multi_tile <- mz_vector_tiles(mz_tile_coordinates(19293:19294, 24641:24642, 16))
    expect_is(multi_tile, "mapzen_vector_tiles")
    expect_length(multi_tile, 9L)
    expect_is(multi_tile[[1]], "mapzen_vector_layer")
})
