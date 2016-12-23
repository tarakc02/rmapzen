context("mz-place")

test_that("place search builds urls correctly", {
    tower1 <- build_place_url("openstreetmap:venue:way:5013364", api_key = "x")
    expect_dict_equal(
        tower1$query, list(
            api_key = "x",
            ids = "openstreetmap:venue:way:5013364"
        )
    )

    tower2 <- build_place_url(
        c("openstreetmap:venue:way:5013364", "whosonfirst:borough:421205771"),
        api_key = "abc"
    )

    expect_dict_equal(
        tower2$query, list(
            api_key = "abc",
            ids = "openstreetmap:venue:way:5013364,whosonfirst:borough:421205771"
        )
    )

})
