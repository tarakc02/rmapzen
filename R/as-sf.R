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
#' @importFrom sf summarise.sf ungroup.sf
#' @export
as_sf.mapzen_vector_layer <- function(geo, ...) {
    geo <- recalculate_ids(geo)
    res <- sf::read_sf(as_json(geo))

    # polygons that were separated by tile divisions will have the same ids
    # b/c of recalculate_ids. need to union them into a single polygon and
    # then bring back feature data
    res <- dplyr::group_by_at(
        res,
        setdiff(names(res), "geometry"))
    res <- sf::st_as_sf(res)
    res <- dplyr::summarise(res)
    dplyr::ungroup(res)
}
