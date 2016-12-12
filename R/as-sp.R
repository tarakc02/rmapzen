as_sp <- function(geo, ...) UseMethod("as_sp")

as_sp.geo_list <- function(geo, ...) {
    geojsonio::geojson_sp(geo, stringsAsFactors = FALSE)
}
