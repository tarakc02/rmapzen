#' @import assertthat
build_reverse_url <- function(
    point, size = NULL, layers = NULL, sources = NULL,
    boundary.country = NULL,
    api_key = NULL
) {
    assert_that(is.null(boundary.country) || is.string(boundary.country))
    point <- as.mz_location(point)
    point <- unwrap(point, "point", c("lat", "lon"))

    if (!is.null(sources)) sources <- string_array(sources)
    if (!is.null(layers)) layers <- string_array(layers)

    query <- c(
        point, size = size,
        sources = sources, layers = layers,
        boundary.country = boundary.country,
        list(api_key = api_key)
    )
    query <- query[!is.null(query)]

    do.call(search_url, c(endpoint = "reverse", query))
}

#' @rdname search
#' @export
mz_reverse_geocode <- function(
    point, size = NULL, layers = NULL, sources = NULL,
    boundary.country = NULL,
    api_key = NULL
) {
    url <- build_reverse_url(
        point = point,
        size = size,
        layers = layers,
        sources = sources,
        boundary.country = boundary.country,
        api_key = api_key
    )

    search_get(url)
}
