context("as_sf")

test_that("as_sf converts search results to sf", {
    oakland_public_sf <- as_sf(oakland_public)
    expect_is(oakland_public_sf, "sf")
})

test_that("as_sf converts isochrone results to sf", {
    marina_walks_sf <- as_sf(marina_walks)
    expect_is(marina_walks_sf, "sf")

    marina_walks_polygons_sf <- as_sf(marina_walks_polygons)
    expect_is(marina_walks_polygons_sf, "sf")
})

test_that("as_sf converts vector tile layers to sf", {
    roadlines <- as_sf(ca_tiles$roads)
    expect_is(roadlines, "sf")

    earthpolygons <- as_sf(ca_tiles$earth)
    expect_is(earthpolygons, "sf")

    waterpoints <- as_sf(ca_tiles$water)
    expect_is(waterpoints, "sf")
})

test_that("as_sf errors on empty search results", {
    empty_search <- oakland_public
    empty_search$features <- list()
    expect_error(as_sf(empty_search), "no data")
})

test_that("as_sf errors on empty vector tile layers", {
    expect_error(as_sf(ca_tiles$buildings), "empty")
})
