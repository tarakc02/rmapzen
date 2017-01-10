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

test_that("bounding box is correctly converted to tile coordinates", {
    rect <- mz_rect(
        min_lon = -122.3163,
        min_lat = 37.86393,
        max_lon = -122.3093,
        max_lat = 37.86912
    )

    marinalon <- -122.3151
    marinalat <- 37.86613
    tile_coords <- as.mz_tile_coordinates(rect)
    z <- tile_coords[[1]]$z

    # at zoom16, this tile should be there:
    # based on http://tools.geofabrik.de/calc/#type=geofabrik_standard&bbox=-122.321657,37.862023,-122.305585,37.870408&grid=1
    x <- 10501
    y <- 25310

    x <- as.integer(x * 2^(z - 16))
    y <- as.integer(y * 2^(z - 16))

    # make sure the coord is in the set:
    xs <- vapply(tile_coords, function(.) .[["x"]], numeric(1))
    ys <- vapply(tile_coords, function(.) .[["y"]], numeric(1))

    expect_true(x %in% xs)
    expect_true(y %in% ys)
})
