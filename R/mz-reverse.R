#' @import assertthat
build_reverse_url <- function(
    point, size = NULL, layers = NULL, sources = NULL,
    api_key = mz_key()
) {
    point <- unwrap(point, "point", c("lat", "lon"))

    if (!is.null(sources)) sources <- string_array(sources)
    if (!is.null(layers)) layers <- string_array(layers)

    query <- c(
        point, size = size,
        sources = sources, layers = layers,
        api_key = api_key
    )
    query <- query[!is.null(query)]

    do.call(search_url, c(endpoint = "reverse", query))
}

#' @export
mz_reverse_geocode <- function(
    point, size = NULL, layers = NULL, sources = NULL,
    api_key = mz_key()
) {
    url <- build_reverse_url(
        point = point,
        size = size,
        layers = layers,
        sources = sources,
        api_key = api_key
    )

    search_get(url)
}
