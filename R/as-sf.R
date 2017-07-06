#' Coerce a Mapzen response to a simple features object
#'
#' Coerces responses to class sf
#'
#' @param geo The object to be converted
#' @param ... not currently used
#'
#' @name as_sf
#' @export
as_sf <- function(geo, ...) UseMethod("as_sf")

#' @rdname as_sf
#' @export
as_sf.geo_list <- function(geo, ...) {
    sf::read_sf(as_json(geo), stringsAsFactors = FALSE)
}

#' @rdname as_sf
#' @export
as_sf.mapzen_vector_layer <- function(geo, ...) {
    geo <- recalculate_ids(geo)
    sf::read_sf(as_json(geo))
}
