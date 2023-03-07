#' Coerce a Mapzen response to a simple features object
#'
#' Coerces responses to class sf. See \code{vignette("sf1", package = "sf")} for
#' more information about Simple Features for R.
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
    if (length(geo$features) == 0L)
        stop("Cannot convert to sf: no data")
    sfread_(as_json(geo), stringsAsFactors = FALSE)
}

#' @rdname as_sf
#' @export
as_sf.mapzen_vector_layer <- function(geo, ...) {
    if (length(geo$features) == 0L)
        stop("Cannot convert to sf: empty layer")

    geo <- recalculate_ids(geo)
    geo <- collapse_properties(geo)
    res <- sfread_(as_json(geo), stringsAsFactors = FALSE)

    # polygons that were separated by tile divisions will have the same ids
    # b/c of recalculate_ids. need to union them into a single polygon and
    # then bring back feature data
    res <- dplyr::group_by_at(
        res,
        setdiff(names(res), "geometry"))
    res <- sf::st_as_sf(res)
    res <- sf::st_make_valid(res)
    res <- dplyr::summarise(res)
    dplyr::ungroup(res)
}

# note: this is more reliable than counting on sf to recognize that the input
# string is not a filename
sfread_ <- function(charstring, ...) {
    tmp <- tempfile(pattern = "geojson", fileext = ".json")
    td <- dirname(tmp)
    if (!dir.exists(td)) dir.create(td)
    writeLines(charstring, tmp)
    sf::read_sf(tmp, ...)
}
