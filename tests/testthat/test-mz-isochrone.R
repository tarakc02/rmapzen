context("mz-isochrone")

test_that("isochrone urls built correctly", {
    test1 <- build_isochrone_url(
        locations = c(lat = 37, lon = -122),
        costing = "pedestrian",
        costing_options = NULL,
        contours = data.frame(
            time = seq(10, 30, 10),
            color = c("440154", "21908C", "FDE725")
        ),
        date_time = NULL,
        polygon = FALSE,
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
        locations = c(lat = 37, lon = -122),
        costing = "pedestrian",
        costing_options = list(
            pedestrian = data.frame(
                walkway_factor = .2,
                alley_factor = 3
            )
        ),
        contours = data.frame(
            time = seq(10, 30, 10),
            color = c("440154", "21908C", "FDE725")
        ),
        date_time = NULL,
        polygon = FALSE,
        denoise = NULL,
        generalize = NULL,
        id = "test1",
        api_key = "abc"
    )

    expect_equal(
        as.character(test2$query$json),
        '{"locations":[{"lon":-122,"lat":37}],"costing":"pedestrian","costing_options":{"pedestrian":[{"walkway_factor":0.2,"alley_factor":3}]},"contours":[{"time":10,"color":"440154"},{"time":20,"color":"21908C"},{"time":30,"color":"FDE725"}]}'
    )

})


