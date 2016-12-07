matrix_path <- function(endpoint) endpoint

matrix_url <- function(endpoint, ..., api_key = mz_key()) {
    query <- list(...)
    query <- c(api_key = api_key, query)
    structure(
        list(
            scheme = "https",
            hostname = "matrix.mapzen.com",
            path = matrix_path(endpoint),
            query = query),
        class = "url"
    )
}

build_isochrone_url <- function(
    locations,
    costing = mz_costing("pedestrian"),
    contours = list(list(time = 15, color = "000000")),
    date_time = NULL,
    polygon = FALSE,
    denoise = NULL,
    generalize = NULL,
    id,
    api_key = mz_key()
) {
    json <- build_isochrone_json(
        locations = locations,
        costing = costing,
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
    costing = mz_costing("pedestrian"),
    date_time = NULL,
    contours = list(list(time = 15, color = "000000")),
    polygon = FALSE,
    denoise = NULL,
    generalize = NULL) {

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

    assert_that(inherits(costing, "mz_costing"))
    costing_model <- jsonlite::unbox(costing[["model"]])

    res <- c(res, list(costing = costing_model))

    costing_options <- costing[["options"]]
    if (length(costing_options) > 0L)
        res <- c(res, costing_options = costing_options)

    if (!is.null(date_time)) {
        assert_that(is.time(date_time))
        date_time <- format(date_time, "%Y-%m-%dT%H:%M")
        res <- c(res, date_time = list(jsonlite::unbox(date_time)))
    }

    check_contour <- function(contour) {
        assert_that(has_components(contour, c("time", "color")))
    }

    vapply(contours, check_contour,
           FUN.VALUE = logical(1))

    contours <- lapply(contours,
                       function(contour) list(
                           time = jsonlite::unbox(contour[["time"]]),
                           color = jsonlite::unbox(contour[["color"]])
                       ))

    res <- c(res, contours = list(contours))
    jsonlite::toJSON(res)
}

mz_costing <- function(model, ...) {
    structure(
        list(
            model = model,
            costing_options = list(...)),
        class = "mz_costing")
}
