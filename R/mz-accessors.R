mz_type <- function(geo) UseMethod("mz_type")
mz_bbox <- function(geo) UseMethod("mz_bbox")
mz_attribution <- function(geo) UseMethod("mz_attribution")
mz_coordinates <- function(geo) UseMethod("mz_coordinates")
mz_labels <- function(geo) UseMethod("mz_labels")
mz_confidences <- function(geo) UseMethod("mz_confidences")

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

mz_labels.mapzen_geo_list <- function(geo) {
    labels <- vapply(geo$features,
           function(feature) feature$properties$label,
           FUN.VALUE = character(1))
    tibble::data_frame(label = labels)
}

mz_confidences.mapzen_geo_list <- function(geo) {
    conf <- function(feature) {
        confidence <- feature[[c("properties", "confidence")]]
        if (is.null(confidence))
            NA_real_
        else confidence
    }
    tibble::data_frame(
        confidence = vapply(geo$features, conf, FUN.VALUE = numeric(1))
    )
}
