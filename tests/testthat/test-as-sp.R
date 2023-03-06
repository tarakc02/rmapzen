context("as_sp")

test_that("as_sp converts search results to SpatialPointsDF", {
    oakland_public_sp <- expect_warning(as_sp(oakland_public))
    expect_is(oakland_public_sp, "SpatialPointsDataFrame")
})

test_that("as_sp converts isochrone results to SpatialLines/PolygonsDF", {
    marina_walks_sp <- expect_warning(as_sp(marina_walks))
    # would by SpatialPolygons if used polygons = TRUE in the mz_isochrone call
    expect_is(marina_walks_sp, "SpatialLinesDataFrame")

    marina_walks_polygons_sp <- expect_warning(as_sp(marina_walks_polygons))
    expect_is(marina_walks_polygons_sp, "SpatialPolygonsDataFrame")
})

test_that("as_sp converts vector tile layers to the appropriate Spatial*DF", {
    roadlines <- expect_warning(as_sp(ca_tiles$roads, geometry_type = "line"))
    expect_is(roadlines, "SpatialLinesDataFrame")

    earthpolygons <- expect_warning(as_sp(ca_tiles$earth, geometry_type = "polygon"))
    expect_is(earthpolygons, "SpatialPolygonsDataFrame")

    waterpoints <- expect_warning(as_sp(ca_tiles$water, geometry_type = "point"))
    expect_is(waterpoints, "SpatialPointsDataFrame")
})

test_that("as_sp errors on empty search results", {
    empty_search <- oakland_public
    empty_search$features <- list()
    expect_error(expect_warning(as_sp(empty_search)), "no data")
})

test_that("as_sp errors on emtpy vector tile layers", {
    expect_error(expect_warning(as_sp(ca_tiles$buildings)), "empty")
})
