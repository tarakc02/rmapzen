#' @export
print.mapzen_geo_list <- function(geo) {
    cat("GeoJSON response from Mapzen\n")
    cat("Attribution info:", mz_attribution(geo), "\n")

    n <- length(geo$features)

    bbox <- mz_bbox(geo)
    min_lat <- round(bbox$min_lat, 2)
    min_lon <- round(bbox$min_lon, 2)
    max_lat <- round(bbox$max_lat, 2)
    max_lon <- round(bbox$max_lon, 2)

    cat("Bounds (lon/lat): (",
        min_lon, ", ",
        min_lat, ") - (",
        max_lon, ", ",
        max_lat, ")\n", sep = "")

    cat(n, "locations:\n")

    getname <- function(feature) {
        nm <- feature$properties$name
        if (is.null(nm)) return("???") else return(nm)
    }

    getcoords <- function(feature) {
        coords <- feature$geometry$coordinates
        if (is.null(coords)) return("(???)")
        res <- paste(round(as.numeric(feature$geometry$coordinates), 2), collapse = ", ")
        paste("(", res, ")", sep = "")
    }

    places <- vapply(geo$features, getname, FUN.VALUE = character(1))
    coords <- vapply(geo$features, getcoords, FUN.VALUE = character(1))

    cat(paste("  ", places, coords), sep = "\n")
    geo
}
