#' @import assertthat
unwrap <- function(obj, objname, components) {
    assert_that(has_components(obj, components))
    names(obj) <- paste0(objname, ".", names(obj))
    as.list(obj)
}

has_components <- function(obj, components) {
    setequal(names(obj), components)
}

#' @import assertthat
#' @export
mz_search <- function(
    text, size = 10,
    boundary.country = NULL, boundary.rect = NULL,
    boundary.circle = NULL, focus.point = NULL,
    sources = NULL, layers = NULL, api_key = mz_key())
{
    if (!is.null(boundary.rect))
        boundary.rect <- unwrap(boundary.rect, "boundary.rect",
                                c("min.lat", "min.lon", "max.lat", "max.lon"))
    if (!is.null(boundary.circle))
        boundary.circle <- unwrap(boundary.circle, "boundary.circle",
                                c("lat", "lon", "radius"))
    if (!is.null(focus.point))
        focus.point <- unwrap(focus.point, "focus.point",
                                c("lat", "lon"))

    if (!is.null(sources)) sources <- paste(sources, collapse = ",")
    if (!is.null(layers)) layers <- paste(layers, collapse = ",")

    query <- list()

    query <- c(
        query,
        text = text, size = size,
        boundary.country = boundary.country,
        boundary.rect, boundary.circle, focus.point,
        sources = sources, layers = layers,
        api_key = api_key
    )
    query <- query[!is.null(query)]

    do.call(mz_search_main, c(endpoint = "search", query))
}
