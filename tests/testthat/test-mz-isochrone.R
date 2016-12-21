context("mz-isochrone")

test_that("isochrone urls built correctly", {
    test1 <- build_isochrone_url(
        locations = c(lat = 37, lon = -122),
        costing_model = mz_costing$pedestrian(),
        contours = mz_contours(
            times = seq(10, 30, 10),
            colors = c("440154", "21908C", "FDE725")
        ),
        date_time = NULL,
        polygons = NULL,
        denoise = NULL,
        generalize = NULL,
        id = "test1",
        api_key = "abc"
    )

    expect_true(setequal(
        names(test1$query),
        c("api_key", "json", "id")
    ))

    expect_equal(test1$query$id, "test1")
    expect_equal(test1$query$api_key, "abc")
    expect_equal(
        as.character(test1$query$json),
        '{"locations":[{"lon":-122,"lat":37}],"costing":"pedestrian","contours":[{"time":10,"color":"440154"},{"time":20,"color":"21908C"},{"time":30,"color":"FDE725"}]}'
    )

    ##

    test2 <- build_isochrone_url(
        locations = mz_location(lat = 37, lon = -122),
        costing_model = mz_costing$pedestrian(
            mz_costing_options$pedestrian$walkway_factor(.2),
            mz_costing_options$pedestrian$alley_factor(3)
        ),
        contours = mz_contours(
            times = seq(10, 30, 10),
            colors = c("440154", "21908C", "FDE725")
        ),
        date_time = NULL,
        polygons = NULL,
        denoise = NULL,
        generalize = NULL,
        id = "test2",
        api_key = "abc"
    )

    expect_equal(
        as.character(test2$query$json),
        '{"locations":[{"lon":-122,"lat":37}],"costing":"pedestrian","costing_options":{"pedestrian":{"walkway_factor":0.2,"alley_factor":3}},"contours":[{"time":10,"color":"440154"},{"time":20,"color":"21908C"},{"time":30,"color":"FDE725"}]}'
    )

    ##

    test3 <- build_isochrone_url(
        locations = mz_location(lat = 37, lon = -122),
        costing_model = mz_costing$pedestrian(
            mz_costing_options$pedestrian$walking_speed(7.0),
            mz_costing_options$pedestrian$walkway_factor(10)
        ),
        contours = mz_contours(seq(10, 40, 10)),
        date_time = mz_date_time(as.POSIXct("2016-12-11 17:41:51 PST"), "departure"),
        polygons = FALSE,
        denoise = .7,
        generalize = 4773,
        id = "my-id",
        api_key = "mz-key"
    )

    test3 <- jsonlite::fromJSON(test3$query$json,
                                simplifyVector = FALSE)
    expect_equal(
        test3$polygons, FALSE
    )
    expect_equal(
        test3$denoise, .7
    )
    expect_equal(
        test3$generalize, 4773
    )
    expect_equal(
        test3$date_time,
        list(
            type = 1, value = "2016-12-11T17:41"
        )
    )
    ##

    test4 <- build_isochrone_url(
        locations = mz_location(lat = 37, lon = -122),
        costing_model = mz_costing$multimodal(
            transit = list(
                mz_costing_options$transit$use_bus(1.0),
                mz_costing_options$transit$use_rail(.7)
            ),
            pedestrian = list(
                mz_costing_options$pedestrian$alley_factor(3)
            )
        ),
        contours = mz_contours(10, 'ffffff'),
        date_time = NULL,
        polygons = NULL,
        denoise = NULL,
        generalize = NULL,
        id = NULL,
        api_key = "abc"
    )

})
