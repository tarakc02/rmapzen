context("mz_bbox")

test_that("mz_bbox gets bounding box for vector tiles", {
    # ca_tiles has all of california, so should include
    # san diego and sacramento
    bbox <- mz_bbox(ca_tiles)
    expect_is(bbox, "data.frame")
    expect_true(setequal(
        names(bbox),
        c("min_lon", "min_lat", "max_lon", "max_lat")
    ))

    sdlon <- -117.1534
    sdlat <- 32.80151
    saclon <- -121.4668
    saclat <- 38.57873

    expect_lt(bbox$min_lon, sdlon)
    expect_gt(bbox$max_lon, sdlon)
    expect_lt(bbox$min_lat, sdlat)
    expect_gt(bbox$max_lat, sdlat)

    expect_lt(bbox$min_lon, saclon)
    expect_gt(bbox$max_lon, saclon)
    expect_lt(bbox$min_lat, saclat)
    expect_gt(bbox$max_lat, saclat)
})

test_that("mz_bbox gets bounding box for isochrones", {
    bbox <- mz_bbox(marina_walks)
    expect_is(bbox, "data.frame")
    expect_true(setequal(
        names(bbox),
        c("min_lon", "min_lat", "max_lon", "max_lat")
    ))
    # should at least contain the marina itself
    marinalon <- -122.3151
    marinalat <- 37.86613
    expect_lt(bbox$min_lon, marinalon)
    expect_gt(bbox$max_lon, marinalon)
    expect_lt(bbox$min_lat, marinalat)
    expect_gt(bbox$max_lat, marinalat)
})

test_that("mz_bbox gets bounding box for search results", {
    bbox <- mz_bbox(oakland_public)
    coords <- mz_coordinates(oakland_public)
    # should contain all the points
    within <- function(lon, lat, bbox) {
        expect_lte(bbox$min_lon, lon)
        expect_gte(bbox$max_lon, lon)
        expect_lte(bbox$min_lat, lat)
        expect_gte(bbox$max_lat, lat)
    }
    Map(function(x, y) within(x, y, bbox), coords$lon, coords$lat)
})

test_that("mz_bbox works for sf and sp objects", {
    oakland_sf <- as_sf(oakland_public)
    oakland_sp <- as_sp(oakland_public)

    expect_is(mz_bbox(oakland_sf), "mz_bbox")
    expect_is(mz_bbox(oakland_sp), "mz_bbox")
})
