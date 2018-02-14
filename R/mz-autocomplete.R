#' @import assertthat
build_autocomplete_url <- function(
    text,
    boundary.country = NULL,
    boundary.rect = NULL,
    focus.point = NULL,
    sources = NULL,
    layers = NULL,
    api_key = NULL)
{
    assert_that(is.string(text))
    assert_that(is.null(boundary.country) || is.string(boundary.country))
    if (!is.null(boundary.rect))
        boundary.rect <- unwrap(boundary.rect, "boundary.rect",
                                c("min.lat", "min.lon", "max.lat", "max.lon"))

    if (!is.null(focus.point)) {
        focus.point <- as.mz_location(focus.point)
        focus.point <- unwrap(focus.point, "focus.point", c("lat", "lon"))
    }

    if (!is.null(sources)) sources <- string_array(sources)
    if (!is.null(layers)) layers <- string_array(layers)

    query <- list()

    query <- c(
        query,
        text = text,
        boundary.country = boundary.country,
        boundary.rect,
        focus.point,
        sources = sources,
        layers = layers,
        list(api_key = api_key)
    )
    query <- query[!is.null(query)]

    do.call(search_url, c(endpoint = "autocomplete", query))
}

#' @rdname search
#' @export
mz_autocomplete <- function(
    text,
    boundary.country = NULL,
    boundary.rect = NULL,
    focus.point = NULL,
    sources = NULL,
    layers = NULL,
    api_key = NULL)
{
    url <- build_autocomplete_url(
        text = text,
        boundary.country = boundary.country,
        boundary.rect = boundary.rect,
        focus.point = focus.point,
        sources = sources,
        layers = layers,
        api_key = api_key
    )

    search_get(url)
}
