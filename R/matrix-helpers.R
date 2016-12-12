#' @export
mz_location <- function(lat, lon) {
    assert_that(
        is.number(lat),
        is.number(lon)
    )

    structure(
        data.frame(lat = lat, lon = lon),
        class = c("mz_location", "data.frame")
    )
}

#' @export
as.mz_location <- function(x, ...) UseMethod("as.mz_location")

#' @export
as.mz_location.default <- function(x, ...) {
    lat <- x[["lat"]]
    lon = x[["lon"]]
    mz_location(lat = lat, lon = lon)
}

#' @export
as.mz_location.mz_geocode_result <- function(x, ...) {
    lat <- x[["geocode_latitude"]]
    lon <- x[["geocode_longitude"]]
    mz_location(lat = lat, lon = lon)
}

#' @export
mz_contours <- function(times, colors = NULL) {
    assert_that(is.numeric(times),
                length(times) <= 4L)

    if (is.null(colors)) {
        times <- sort(times)
        colors <- c('feedde','fdbe85','fd8d3c','d94701')
        colors <- colors[1:length(times)]
    }

    assert_that(
        is.character(colors),
        length(colors) == length(times)
    )

    structure(
        data.frame(
            time = times, color = colors,
            stringsAsFactors = FALSE
        ),
        class = c("mz_contours", "data.frame")
    )
}

mz_date_time <- function(date_time, type = "departure") {
    assert_that(is.time(date_time), is.string(type))
    types <- c("current", "departure", "arrival")
    typenum <- match(type, types) - 1

    if (is.na(typenum)) stop(
        "type '", type, "'",
        " not recognized. type should be one of: ",
        paste0("'", types, "'", collapse = ", ")
    )

    date_time <- format(date_time, "%Y-%m-%dT%H:%M")

    structure(
        data.frame(
            type = typenum,
            value = date_time,
            stringsAsFactors = FALSE
        ),
        class = c("mz_date_time", "data.frame")
    )
}
