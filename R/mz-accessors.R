#' @export
mz_type <- function(geo) UseMethod("mz_type")

#' Get the bounding box for a \code{mapzen_geo_list}
#'
#' The returned value can be used directly as the \code{boundary.rect}
#' parameter for \link{search} functions
#'
#' @param geo A mapzen geo list
#'
#' @return A tibble
#' @export
mz_bbox <- function(geo) UseMethod("mz_bbox")


#' Get attribution information from a \code{mapzen_geo_list}
#'
#' @param geo A mapzen geo list
#'
#' @export
mz_attribution <- function(geo) UseMethod("mz_attribution")

#' Extract a data frame of coordinates from a \code{mapzen_geo_list}
#'
#' @param geo A mapzen geo list
#'
#' @return A tibble, with columns \code{lon} and \code{lat}
#' @export
mz_coordinates <- function(geo) UseMethod("mz_coordinates")

#' @export
mz_type.mapzen_geo_list <- function(geo) geo$type

#' @export
mz_bbox.mapzen_geo_list <- function(geo) {
    bbox <- geo$bbox
    tibble::data_frame(
        min_lon = bbox[[1]],
        min_lat = bbox[[2]],
        max_lon = bbox[[3]],
        max_lat = bbox[[4]]
    )
}

#' @export
mz_attribution.mapzen_geo_list <- function(geo) geo$geocoding$attribution

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

    tibble::data_frame(lon = lon, lat = lat)
}
