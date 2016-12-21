#' Create/extract lat/lon location information
#'
#' \code{mz_location} constructs a new \code{mz_location} object, which can be
#' used with functions such as \code{\link{mz_isochrone}} or
#' \code{\link{mz_reverse_geocode}}. \code{as.mz_location} coerces eligible
#' objects to \code{mz_location}s.
#'
#' @param lat Latitude
#' @param lon Longitude
#' @param x An object that has location information
#' @param ... Not currently used
#'
#' @name mz_location
#' @seealso
#' \code{\link{mz_isochrone}} For using the Mapzen isochrone service
#' \code{\link{mz_contours}}, \code{\link{mz_costing}}, and
#' \code{\link{mz_costing_options}} for other argument constructors
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

#' @rdname mz_location
#' @export
as.mz_location <- function(x, ...) UseMethod("as.mz_location")

#' @rdname mz_location
#' @export
as.mz_location.default <- function(x, ...) {
    lat <- x[["lat"]]
    lon = x[["lon"]]
    mz_location(lat = lat, lon = lon)
}

#' @rdname mz_location
#' @export
as.mz_location.mz_geocode_result <- function(x, ...) {
    lat <- x[["geocode_latitude"]]
    lon <- x[["geocode_longitude"]]
    mz_location(lat = lat, lon = lon)
}

#' Create an mz_contours object
#'
#' Contours are given as inputs to \code{\link{mz_isochrone}}. This function
#' makes it convenient to construct them.
#'
#' @param times Times in minutes for the contour. Up to a maximum of 4 numbers.
#' @param colors Colors for the contours. By default, a palette will be constructed
#' from the Colorbrewer 4-class oranges palette.
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

#' Create mz_date_time objects
#'
#' Mobility services (such as \code{mz_isochrone}) take, optionally, a date_time
#' argument that specifies the date and time along with type (departure/arrival).
#' This function constructs the appropriate objects to use as date_time arguments.
#'
#' @param date_time A POSIXt date-time object
#' @param type "departure" or "arrival"
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
