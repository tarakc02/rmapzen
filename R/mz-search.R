search_query_parameters <- function(
    size = 10,
    boundary.country = NULL, boundary.rect = NULL,
    boundary.circle = NULL, focus.point = NULL,
    sources = NULL, layers = NULL
) {
    assert_that(is.count(size))
    assert_that(is.null(boundary.country) || is.string(boundary.country))

    if (!is.null(boundary.rect))
        boundary.rect <- unwrap(boundary.rect, "boundary.rect",
                                c("min_lat", "min_lon", "max_lat", "max_lon"))
    if (!is.null(boundary.circle))
        boundary.circle <- unwrap(boundary.circle, "boundary.circle",
                                  c("lat", "lon", "radius"))
    if (!is.null(focus.point)) {
        focus.point <- as.mz_location(focus.point)
        focus.point <- unwrap(focus.point, "focus.point", c("lat", "lon"))
    }

    if (!is.null(sources)) sources <- string_array(sources)
    if (!is.null(layers)) layers <- string_array(layers)

    query <- list()
    query <- c(
        query,
        size = size,
        boundary.country = boundary.country,
        boundary.rect, boundary.circle, focus.point,
        sources = sources, layers = layers
    )
    query[!is.null(query)]
}

#' @import assertthat
build_search_url <- function(
    text, size = 10,
    boundary.country = NULL, boundary.rect = NULL,
    boundary.circle = NULL, focus.point = NULL,
    sources = NULL, layers = NULL, api_key = NULL)
{
    assert_that(is.string(text))

    query <- search_query_parameters(
        size = size, boundary.country = boundary.country,
        boundary.rect = boundary.rect,
        boundary.circle = boundary.circle,
        focus.point = focus.point,
        sources = sources, layers = layers
    )

    query <- c(
        text = text,
        query,
        list(api_key = api_key)
    )
    query <- query[!is.null(query)]

    do.call(search_url, c(endpoint = "search", query))
}

#' Mapzen search API
#'
#' Functions to access the various endpoints from the Mapzen Search API.
#' For more details, see \url{https://mapzen.com/documentation/search/}. If your
#' data is already split up by street, city, state, zip, etc., then you might
#' find \code{\link{mz_structured_search}} to be more precise. All arguments
#' besides \code{text} (\code{point} in the case of \code{mz_reverse_geocode}) are
#' optional. If you have parsed addresses (e.g. for geocoding), use \code{\link{mz_structured_search}}
#'
#' @param text Search string
#' @param point For reverse geocoding, the location to reverse geocode. Can be
#' created with \code{\link{mz_location}} or \code{\link{mz_geocode}},
#' otherwise should have names "lat" and "lon"
#' @param size Number of search results requested
#' @param boundary.country ISO-3166 country code to narrow the search.
#' See \code{\link{mz_countries}}
#' @param boundary.rect 4 corners that define a box to narrow the search. Can
#' be the result of \code{\link{mz_bbox}}. Should have named elements with names
#' "min_lon", "min_lat", "max_lon", "max_lat" -- can be created using \code{\link{mz_rect}}.
#' @param boundary.circle A circle to narrow the search. Should have named elements
#' with names "lon", "lat", and "radius"
#' @param focus.point A point to "focus" the search. Can be created with
#' \code{\link{mz_location}} or \code{\link{mz_geocode}}, otherwise should have
#'  names "lat" and "lon"
#' @param sources The originating source of the data (to filter/narrow search
#' results). See \code{\link{mz_sources}}
#' @param layers Which layers (types of places) to search. See
#' \url{https://mapzen.com/documentation/search/search/#filter-by-data-type}
#' for definitions, and use \code{\link{mz_layers}} for convenience
#' @param api_key Your Mapzen API key. The default is to look for the key within
#' the provider information that was set up with `mz_set_host`.
#' @name search
#' @seealso \code{\link{mz_place}}, \code{\link{mz_structured_search}},
#' \code{\link{mz_countries}}, \code{\link{mz_sources}}, \code{\link{mz_layers}}
#'
#' @examples
#' \dontrun{
#' # hard rock cafes in sweden:
#' mz_search("Hard Rock Cafe", boundary.country = "SE")
#'
#' # autocompletions when the user types in "Union Square"
#' # prioritizing San Francisco results first:
#' mz_autocomplete("Union Square",
#'                 focus.point = mz_geocode("San Francisco, CA"))
#' }
#'
#' @export
mz_search <- function(
    text, size = 10,
    boundary.country = NULL, boundary.rect = NULL,
    boundary.circle = NULL, focus.point = NULL,
    sources = NULL, layers = NULL, api_key = NULL)
{
    url <- build_search_url(
        text = text,
        size = size,
        boundary.country = boundary.country,
        boundary.rect = boundary.rect,
        boundary.circle = boundary.circle,
        focus.point = focus.point,
        sources = sources,
        layers = layers,
        api_key = api_key
    )

    search_get(url)
}
