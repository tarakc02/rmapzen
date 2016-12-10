#' @export
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

#' @export
contours <- function(times, colors = NULL) {
    assert_that(is.numeric(times))
    if (is.null(colors)) {
        colors <- c('feedde','fdbe85','fd8d3c','d94701')
        colors <- colors[1:length(times)]
    }

    assert_that(
        is.character(colors),
        length(colors) == length(times),
        length(colors) <= 4L
    )

    structure(
        data.frame(
            time = times, color = colors,
            stringsAsFactors = FALSE
        ),
        class = c("mz_contours", "data.frame")
    )
}
