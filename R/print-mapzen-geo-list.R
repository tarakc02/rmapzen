#' @export
print.mapzen_geo_list <- function(x, ..., n = 5) {
    geo <- x
    assert_that(is.count(n) && n > 0)

    attribution <- geo$geocoding$attribution
    if (is.null(attribution)) attribution <- NA_character_
    if (!assertthat::is.string(attribution)) attribution <- NA_character_

    cat("GeoJSON response from Mapzen\n")
    cat("Attribution info:", attribution, "\n")

    nlocs <- length(geo$features)

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

    cat(nlocs, "locations")
    if (nlocs > 0)
        cat(":\n")
    else {
        cat("\n")
        return(invisible(geo))
    }

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

    printlen <- pmin(n, length(geo$features))

    places <- vapply(geo$features[1:printlen], getname, FUN.VALUE = character(1))
    coords <- vapply(geo$features[1:printlen], getcoords, FUN.VALUE = character(1))

    cat(paste("  ", places, coords), sep = "\n")
    if (printlen < length(geo$features)) cat("  ...\n")
    invisible(geo)
}
