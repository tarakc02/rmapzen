#' Coerce a Mapzen response to an Spatial*DataFrame
#'
#' Coerces responses to SpatialPoints (for search responses) or SpatialLines
#' (for isochrone responses) data frames.
#'
#' @param geo The object to be converted
#' @param ... not currently used
#'
#' @export
as_sp <- function(geo, ...) UseMethod("as_sp")

#' @export
as_sp.geo_list <- function(geo, ...) {
    geojsonio::geojson_sp(geo, stringsAsFactors = FALSE)
}

#' @export
as.data.frame.mapzen_isochrone_list <- function(x, ...) {

    coords <- function(feature)
        dplyr::bind_rows(
            purrr::map2(
                feature$geometry$coordinates,
                seq_along(feature$geometry$coordinates),
                ~tibble::data_frame(lon = .x[[1]], lat = .x[[2]],
                                    order = .y)
            )
        )

    res <- tibble::data_frame(
        contours = purrr::map(
            x$features,
            ~tibble::as_data_frame(
                .$properties
            )
        ),
        coordinates = purrr::map(
            x$features,
            coords
        )
    )
    res <- tidyr::unnest(res, contours)
    tidyr::unnest(res)
}
