#' @export
mz_type <- function(geo) UseMethod("mz_type")

#' @export
mz_bbox <- function(geo) UseMethod("mz_bbox")

#' @export
mz_attribution <- function(geo) UseMethod("mz_attribution")

#' @export
mz_coordinates <- function(geo) UseMethod("mz_coordinates")

mz_type.mapzen_geo_list <- function(geo) geo$type

mz_bbox.mapzen_geo_list <- function(geo) {
    bbox <- geo$bbox
    tibble::data_frame(
        min.lon = bbox[[1]],
        min.lat = bbox[[2]],
        max.lon = bbox[[3]],
        max.lat = bbox[[4]]
    )
}

mz_attribution.mapzen_geo_list <- function(geo) geo$geocoding$attribution

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
