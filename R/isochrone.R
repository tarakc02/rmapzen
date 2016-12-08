build_isochrone_url <- function(
    locations,
    costing,
    costing_options,
    contours,
    date_time,
    polygon,
    denoise,
    generalize,
    id,
    api_key = mz_key()
) {
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
    assert_that(is.numeric(locations))
    assert_that(length(locations) == 2L)
    assert_that(has_components(locations, c("lon", "lat")))

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
        assert_that(is.time(date_time))
        date_time <- format(date_time, "%Y-%m-%dT%H:%M")
        res <- c(res, date_time = list(jsonlite::unbox(date_time)))
    }

    res <- c(res, contours = list(contours))
    jsonlite::toJSON(res)
}


iso_process <- function(response) {
    httr::stop_for_status(response)
    header <- httr::headers(response)
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

mz_isochrone <- function(
    locations = c(lat = 37.87238, lon = -122.25420),
    costing = "pedestrian",
    costing_options = list(
        pedestrian = data.frame(
            walkway_factor = .2,
            alley_factor = 3
        )),
    contours = data.frame(time = seq(10, 30, 10),
                          color = c("440154", "21908C", "FDE725"),
                          stringsAsFactors = FALSE),
    date_time = NULL,
    polygon = FALSE,
    denoise = NULL,
    generalize = NULL,
    id = "my-iso",
    api_key = mz_key()
) {
    url <- build_isochrone_url(
        locations = locations,
        costing = costing,
        costing_options = costing_options,
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
