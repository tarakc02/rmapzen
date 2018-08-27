context("specifying tile coordinates")

test_that("tile coordinates are generated for contiguous rectangular range", {
    ex1 <- mz_tile_coordinates(1:5, 4, 12)
    # should be one element per x for each y
    expect_length(ex1, 5)

    ex2 <- mz_tile_coordinates(1:5, 1:4, 12)
    expect_length(ex2, 20)

    # tiles should use range(x) and range(y)
    # so x should have 5 columns,
    # y should have 7 rows
    ex3 <- mz_tile_coordinates(c(1, 4, 5), c(7, 1), 10)
    expect_length(ex3, 35)
    xs <- vapply(ex3, function(.).[["x"]], FUN.VALUE = numeric(1))
    ys <- vapply(ex3, function(.).[["y"]], FUN.VALUE = numeric(1))

    # a random tile within the specified range should be in the spec
    tile43 <- intersect(which(xs == 4), which(ys == 3))
    expect_length(tile43, 1L)
    zs <- unique(vapply(ex3, function(.).[["z"]], numeric(1)))
    expect_length(zs, 1L)
})

test_that("error when ambiguous arguments are provided", {
    rect <- mz_rect(
        min_lon = -122.3163,
        min_lat = 37.86393,
        max_lon = -122.3093,
        max_lat = 37.86912
    )

    expect_error(
        as.mz_tile_coordinates(
            rect,
            height = 375, width = 500,
            z = 17),
        "height.+width.+z")

    expect_error(
        as.mz_tile_coordinates(
            rect,
            height = 375),
        "height.+width")

    expect_warning(
        as.mz_tile_coordinates(
            rect,
            width = 500,
            z = 17),
        "width.+ignored")


})

test_that("bounding box is correctly converted to tile coordinates", {
    marina_rect <- mz_rect(
        min_lon = -122.3163,
        min_lat = 37.86393,
        max_lon = -122.3093,
        max_lat = 37.86912
    )

    # at zoom16, this tile should be there:
    # based on http://tools.geofabrik.de/calc/#type=geofabrik_standard&bbox=-122.321657,37.862023,-122.305585,37.870408&grid=1
    expect_tiles_contain_xy(
        as.mz_tile_coordinates(marina_rect),
        x = 10501, y = 25310
    )

    # for zoom 10, this is the tile containing the marina rectangle
    expect_tiles_contain_xy(
        as.mz_tile_coordinates(marina_rect, z = 10),
        x = 164, y = 395
    )
})

test_that("point + zoom is correctly converted to tile coordinates", {
    marinalon <- -122.319080
    marinalat <-   37.873242

    marina <- mz_location(lat = marinalat, lon = marinalon)
    # based on: http://tools.geofabrik.de/calc/#type=geofabrik_standard&bbox=-122.321657,37.862023,-122.305585,37.870408&grid=1
    expect_tiles_contain_xy(
        as.mz_tile_coordinates(marina, z = 15),
        x = 5250,
        y = 12654
    )
})
