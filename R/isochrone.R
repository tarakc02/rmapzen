build_isochrone_url <- function(
    locations,
    costing_model,
    contours,
    date_time,
    polygon,
    denoise,
    generalize,
    id,
    api_key = mz_key()
) {
    costing <- costing_model$costing
    costing_options <- costing_model$costing_options

    json <- build_isochrone_json(
        locations = locations,
        costing = costing,
        costing_options = costing_options,
        date_time = date_time,
        contours = contours,
        polygon = polygon,
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
    polygon,
    denoise,
    generalize) {

    # locations should have lon/lat. for now, only one location
    # is supported
    locations <- as.mz_location(locations)

    assert_that(is.null(polygon) || is.flag(polygon))
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

    if (!is.null(polygon))
        res <- c(res, polygon = list(jsonlite::unbox(polygon)))

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
              class = c("mapzen_isochrone_list", "geo_list"))
}

#' @import assertthat
isochrone_get <- function(url) {
    response <- matrix_GET(httr::build_url(url))
    iso_process(response)
}

#' Retrieve isochrones
#' @name mz_isochrone
#' @export
mz_isochrone <- function(
    locations,
    costing_model,
    contours,
    date_time = NULL,
    polygon = NULL,
    denoise = NULL,
    generalize = NULL,
    id = "my-iso",
    api_key = mz_key()
) {
    costing_options <- costing_model$costing_options
    url <- build_isochrone_url(
        locations = locations,
        costing_model = costing_model,
        contours = contours,
        date_time = date_time,
        polygon = polygon,
        denoise = denoise,
        generalize = generalize,
        id = id,
        api_key = api_key)
    isochrone_get(url)
}

print.mapzen_isochrone_list <- function(x, ...) {
    cat("GeoJSON response from Mapzen\n")
    cat("Isochrones: ", length(x$features), sep = "")
}
