context("mz-reverse")

test_that("reverse search urls built correctly", {

    pt1 <- build_reverse_url(
        point = c(lat = 48.858268, lon = 2.294471),
        size = 1,
        api_key = "mapzen-xxxx"
    )

    expect_dict_equal(
        pt1$query, list(
            point.lat = 48.858268,
            point.lon = 2.294471,
            size = 1,
            api_key = "mapzen-xxxx"
        )
    )

    pt2 <- build_reverse_url(
        point = c(lat = 48.858268, lon = 2.294471),
        size = 1,
        api_key = "mapzen-xxxx",
        sources = c("osm", "oa")
    )

    expect_dict_equal(
        pt2$query, list(
            point.lat = 48.858268,
            point.lon = 2.294471,
            size = 1,
            api_key = "mapzen-xxxx",
            sources = "osm,oa"
        )
    )

    pt3 <- build_reverse_url(
        point = c(lat = 48.858268, lon = 2.294471),
        size = 1,
        api_key = "mapzen-xxxx",
        layers = "locality"
    )

    expect_dict_equal(
        pt3$query, list(
            point.lat = 48.858268,
            point.lon = 2.294471,
            size = 1,
            api_key = "mapzen-xxxx",
            layers = "locality"
        )
    )

    pt4 <- build_reverse_url(
        point = c(lat = 48.858268, lon = 2.294471),
        size = 1,
        api_key = "mapzen-xxxx",
        sources = c("osm", "oa"),
        layers = c("locality", mz_layers$macroregion)
    )

    expect_dict_equal(
        pt4$query, list(
            point.lat = 48.858268,
            point.lon = 2.294471,
            size = 1,
            api_key = "mapzen-xxxx",
            sources = "osm,oa",
            layers = "locality,macroregion"
        )
    )

    pt5 <- build_reverse_url(
        point = c(lat = 48.858268, lon = 2.294471),
        size = 1,
        api_key = "mapzen-xxxx",
        boundary.country = "LIE"
    )

    expect_dict_equal(
        pt5$query, list(
            point.lat = 48.858268,
            point.lon = 2.294471,
            size = 1,
            api_key = "mapzen-xxxx",
            boundary.country = "LIE"
        )
    )

    tower <- build_reverse_url(
        point = c(
            lat = 51.5081124,
            lon = -.0759493
        ),
        api_key = "mapzen-xxxx",
        layers = mz_layers$address,
        sources = "osm",
        size = 1
    )

    expect_dict_equal(
        tower$query, list(
            point.lat = 51.5081124,
            point.lon = -.0759493,
            api_key = "mapzen-xxxx",
            layers = "address",
            sources = "osm",
            size = 1
        )
    )

})
