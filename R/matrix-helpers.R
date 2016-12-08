location <- function(lat, lon) {
    assert_that(
        is.number(lat),
        is.number(lon)
    )

    structure(
        c(lat = lat, lon = lon),
        class = "mz_location"
    )
}

contours <- function(times, colors = NULL) {
    assert_that(is.numeric(times))
    if (is.null(colors)) {
        colors <- c('edf8fb','b2e2e2','66c2a4','238b45')
        colors <- colors[1:length(times)]
    }

    assert_that(
        is.character(colors),
        length(colors) == length(times),
        length(colors) <= 4L
    )

    structure(
        data.frame(
            times = times, colors = colors,
            stringsAsFactors = FALSE
        ),
        class = c("mz_contours", "data.frame")
    )
}
