context("mz-search")

test_that("search urls built correctly", {
    main_st <- build_search_url("main street, usa", size = 3, api_key = "zzz")
    expect_true(
        setequal(
            main_st$query, list(
                api_key = "zzz",
                text = "main street, usa",
                size = 3)
        )
    )

    expect_identical(main_st$scheme, "https")

    eiffel <- build_search_url("eiffel tower, paris", size = 1,
                               boundary.country = "FR", api_key = "zzz")

    expect_true(
        setequal(
            eiffel$query, list(
                api_key = "zzz",
                text = "eiffel tower, paris",
                size = 1,
                boundary.country = "FR"
            )
        )
    )

    # example from mapzen documentation
    ymca_tx <- build_search_url(text = "YMCA",
        boundary.rect = c(
            min.lat = 25.84,
            min.lon = -106.65,
            max.lat = 36.5,
            max.lon = -93.51),
        api_key = "zzz"
    )

    expect_true(
        setequal(
            ymca_tx$query, list(
                api_key = "zzz",
                text = "YMCA",
                size = 10,
                boundary.rect.min.lat=25.84,
                boundary.rect.min.lon=-106.65,
                boundary.rect.max.lat=36.5,
                boundary.rect.max.lon=-93.51
            )
        )
    )

    ymca_circle <- build_search_url(
        "YMCA",
        size = 3,
        boundary.circle = c(
            lat = 43.818156,
            lon = -79.186484,
            radius = 35
        ),
        api_key = "abc"
    )

    expect_true(setequal(
        ymca_circle$query, list(
            api_key = "abc",
            text = "YMCA",
            size = 3,
            boundary.circle.lat = 43.818156,
            boundary.circle.lon = -79.186484,
            boundary.circle.radius = 35
        )
    ))

    ymca_aus <- build_search_url(
        "YMCA",
        focus.point = c(
            lat = -33.856680,
            lon = 151.215281
        ),
        boundary.country = "AUS",
        api_key = "abc"
    )

    expect_true(setequal(
        ymca_aus$query, list(
            api_key = "abc",
            text = "YMCA",
            size = 10,
            boundary.country = "AUS",
            focus.point.lon = 151.215281,
            focus.point.lat = -33.856680
        )
    ))

    ymca_oa <- build_search_url(
        text = "YMCA",
        source = "oa",
        api_key = "abc"
    )

    expect_true(setequal(
        ymca_oa$query, list(
            api_key = "abc",
            source = "oa",
            size = 10,
            text = "YMCA"
        )
    ))

    ymca_2sources <- build_search_url(
        text = "YMCA",
        sources = c("osm", mz_sources$gn),
        api_key = "abc"
    )

    expect_true(setequal(
        ymca_2sources$query, list(
            api_key = "abc",
            sources = "osm,gn",
            size = 10,
            text = "YMCA"
        )
    ))

    ymca_2layers <- build_search_url(
        text = "YMCA",
        layers = c(mz_layers$venue, mz_layers$address),
        api_key = "abc"
    )

    expect_true(setequal(
        ymca_2layers$query, list(
            api_key = "abc",
            sources = "venue,address",
            size = 10,
            text = "YMCA"
        )
    ))
})
