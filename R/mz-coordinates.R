#' Extract a data frame of coordinates from a \code{mapzen_geo_list}
#'
#' @param geo A mapzen geo list
#'
#' @return A tibble, with columns \code{lon} and \code{lat}.
#'
#' @examples
#' mz_coordinates(oakland_public)
#'
#' @name mz_coordinates
#' @export
mz_coordinates <- function(geo) UseMethod("mz_coordinates")

#' @rdname mz_coordinates
#' @export
mz_coordinates.mapzen_geo_list <- function(geo) {
    features <- geo$features
    lon <- vapply(features,
                  function(feature) {
                      feature$geometry$coordinates[[1]]
                  }, FUN.VALUE = numeric(1))

    lat <- vapply(features,
                  function(feature) {
                      feature$geometry$coordinates[[2]]
                  }, FUN.VALUE = numeric(1))

    tibble::tibble(lon = lon, lat = lat)
}
