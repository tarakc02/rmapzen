build_isochrone_url <- function(
    locations,
    costing_model,
    contours,
    date_time,
    polygons,
    denoise,
    generalize,
    id,
    api_key = NULL
) {
    costing <- costing_model$costing
    costing_options <- costing_model$costing_options

    json <- build_isochrone_json(
        locations = locations,
        costing = costing,
        costing_options = costing_options,
        date_time = date_time,
        contours = contours,
        polygons = polygons,
        denoise = denoise,
        generalize = generalize
    )

    matrix_url(
        endpoint = "isochrone",
        json = json,
        id = id,
        api_key = api_key)
}

build_isochrone_json <- function(
    locations,
    costing,
    costing_options,
    date_time,
    contours,
    polygons,
    denoise,
    generalize) {

    # locations should have lon/lat. for now, only one location
    # is supported
    locations <- as.mz_location(locations)

    assert_that(is.null(polygons) || is.flag(polygons))
    assert_that(is.null(denoise) || is.number(denoise))
    assert_that(is.null(generalize) || is.number(generalize))

    locations <- data.frame(
        lon = locations[["lon"]],
        lat = locations[["lat"]]
    )

    res <- list(locations = locations)

    costing_model <- jsonlite::unbox(costing)

    res <- c(res, list(costing = costing_model))

    if (length(costing_options) > 0L)
        res <- c(res, costing_options = list(costing_options))

    if (!is.null(date_time)) {
        assert_that(inherits(date_time, "mz_date_time"))
        res <- c(res, date_time = list(jsonlite::unbox(date_time)))
    }

    res <- c(res, contours = list(contours))

    if (!is.null(polygons))
        res <- c(res, polygons = list(jsonlite::unbox(polygons)))

    if (!is.null(denoise)) {
        assert_that(denoise >= 0, denoise <= 1)
        res <- c(res,
                 denoise = list(jsonlite::unbox(denoise)))
    }

    if (!is.null(generalize))
        res <- c(res, generalize = list(jsonlite::unbox(generalize)))

    jsonlite::toJSON(res)
}


iso_process <- function(response) {
    tryCatch(
        httr::stop_for_status(response),
        http_400 = function(e) {
            txt <- httr::content(response, as = "text")
            lst <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
            stop(e$message, "\n", lst$error, " (", lst$error_code, ")",
                 call. = FALSE)
        }
    )
    header <- httr::headers(response)
    mz_update_usage(header, "matrix")
    txt <- httr::content(response, as = "text")
    lst <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
    structure(lst,
              header = header,
              class = c("mapzen_isochrone_list", "geo_list"))
}

#' @import assertthat
isochrone_get <- function(url) {
    response <- matrix_GET(httr::build_url(url))
    iso_process(response)
}

#' Retrieve isochrones
#'
#' From \url{https://mapzen.com/documentation/mobility/isochrone/api-reference/}:
#' "An isochrone is a line that connects points of equal travel time about a
#' given location, from the Greek roots of 'iso' for equal and 'chrone' for time.
#' The Mapzen Isochrone service computes areas that are reachable within
#' specified time intervals from a location, and returns the reachable regions
#' as contours of polygons or lines that you can display on a map."
#'
#' @param locations An \code{mz_location}, or something that can be coerced to an
#' \code{\link{mz_location}}, as the departure point for the isochrone. This can be the
#' result of \code{\link{mz_geocode}}. Despite the argument name, the isochrone
#' service currently can only accept a single location
#' @param costing_model The costing model, see \code{\link{mz_costing}}
#' @param contours Up to 4 contours, see \code{\link{mz_contours}}
#' @param date_time The local date and time at the location, and whether it is
#' the departure or arrival time. See \code{\link{mz_date_time}}
#' @param polygons Whether to return polygons (TRUE) or linestrings (FALSE, default)
#' @param denoise A value between 0 and 1 (default 1) to remove smaller contours.
#' A value of 1 will only return the largest contour for a given time value. A
#' value of 0.5 drops any contours that are less than half the area of the
#' largest contour.
#' @param generalize Tolerance in meters for the Douglas-Peucker generalization.
#' @param id A descriptive identifier, the response will contain the id as an element.
#' @param api_key Your Mapzen API key. The default is to look for the key within
#' the provider information that was set up with `mz_set_host`.
#'
#' @return A \code{mapzen_isochrone_list}, which can be converted to \code{sf}
#' or \code{sp} using \code{\link{as_sf}} or \code{\link{as_sp}}.
#'
#' @seealso \code{\link{mz_costing}}
#'
#' @examples
#' \dontrun{
#' mz_isochrone(
#'     mz_location(lat = 37.87416, lon = -122.2544),
#'     costing_model = mz_costing$auto(),
#'     contours = mz_contours(c(10, 20, 30))
#' )
#'
#' # departure point can be specified as a geocode result
#' mz_isochrone(
#'     mz_geocode("UC Berkeley"),
#'     costing_model = mz_costing$pedestrian(),
#'     contours = mz_contours(c(10, 20, 30))
#' )
#' }
#'
#' @name mz_isochrone
#' @export
mz_isochrone <- function(
    locations,
    costing_model,
    contours,
    date_time = NULL,
    polygons = NULL,
    denoise = NULL,
    generalize = NULL,
    id = "my-iso",
    api_key = NULL
) {
    costing_options <- costing_model$costing_options
    url <- build_isochrone_url(
        locations = locations,
        costing_model = costing_model,
        contours = contours,
        date_time = date_time,
        polygons = polygons,
        denoise = denoise,
        generalize = generalize,
        id = id,
        api_key = api_key)
    isochrone_get(url)
}

#' @export
print.mapzen_isochrone_list <- function(x, ...) {
    cat("GeoJSON response from Mapzen\n")
    cat("Isochrones: ", length(x$features), sep = "")
}
