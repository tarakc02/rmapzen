context("mz-autocomplete")

test_that("autocomplete request urls built correctly", {

    a1 <- build_autocomplete_url(
        "union square",
        focus.point = c(lat = 37.7, lon = -122.4),
        api_key = "mz"
    )

    expect_dict_equal(
        a1$query, list(
            focus.point.lat = 37.7,
            focus.point.lon = -122.4,
            text = "union square",
            api_key = "mz"
        )
    )

    a2 <- build_autocomplete_url(
        "pennsylvania",
        sources = "openaddresses",
        api_key = "m"
    )

    expect_dict_equal(
        a2$query, list(
            text = "pennsylvania",
            sources = "openaddresses",
            api_key = "m"
        )
    )

    a3 <- build_autocomplete_url(
        "starbuck",
        layers = "coarse",
        api_key = "m"
    )

    expect_dict_equal(
        a3$query, list(
            text = "starbuck",
            layers = "coarse",
            api_key = "m"
        )
    )
})
