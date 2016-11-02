boundary_check <- function(obj, components) {
    assert_that(is.null(obj) || setequal(names(obj), components))
}

#' @import assertthat
mz_search <- function(
    text, size = 10,
    boundary.country = NULL, boundary.rect = NULL,
    boundary.circle = NULL, focus.point = NULL,
    sources = NULL, layers = NULL, api_key = mz_key())
{
    boundary_check(boundary.rect, c("min.lat", "min.lon", "max.lat", "max.lon"))
    boundary_check(boundary.circle, c("lat", "lon", "radius"))
    boundary_check(focus.point, c("lat", "lon"))

    if (!is.null(sources)) sources <- paste(sources, collapse = ",")
    if (!is.null(layers)) layers <- paste(layers, collapse = ",")

    query <- list(
        text = text, size = size,
        boundary.country = boundary.country, boundary.rect = boundary.rect,
        boundary.circle = boundary.circle, focus.point = focus.point,
        sources = sources, layers = layers,
        api_key = api_key
    )

    do.call(mz_search_main, c(endpoint = "search", query))
}
