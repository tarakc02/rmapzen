#' Coerce a Mapzen response to an Spatial*DataFrame
#'
#' Coerces responses to SpatialPoints, SpatialLines, or SpatialPolygons data
#' frames.
#'
#' @param geo The object to be converted
#' @param geometry_type "point", "line", or "polygon" -- can be left NULL and only
#' needs to be specified when an object contains muliple geometry types.
#' @param ... not currently used
#'
#' @name as_sp
#' @export
as_sp <- function(geo, ...) UseMethod("as_sp")

#' @rdname as_sp
#' @export
as_sp.geo_list <- function(geo, ...) {
    geojsonio::geojson_sp(geo, stringsAsFactors = FALSE)
}

#' @rdname as_sp
#' @export
as_sp.mapzen_vector_layer <- function(geo, ..., geometry_type = NULL) {
    features <- geo
    geom_to_wkb <- c(
        point = "wkbPoint",
        line = "wkbLineString",
        polygon = "wkbPolygon"
    )

    json_to_geom <- c(
        LineString = "line",
        MultiLineString = "line",
        Polygon = "polygon",
        MultiPolygon = "polygon",
        Point = "point"
    )

    if (is.null(geometry_type)) {
        geometries <- vapply(
            features$features, function(feature) json_to_geom[[feature$geometry$type]],
            FUN.VALUE = character(1)
        )

        if (length(unique(geometries)) == 1L) {
            geometry_type <- unique(geometries)
        } else {
            smry <- table(geometries)
            msg <- paste(names(smry), ": ", smry, sep = "", collapse = "\n")
            stop("No geometry_type entered. Pick one of:\n", msg)
        }
    }

    assert_that(is.string(geometry_type), geometry_type %in% names(geom_to_wkb))
    geometry_type <- geom_to_wkb[[geometry_type]]

    json <- geojsonio::as.json(features)
    rgdal::readOGR(
        json,
        layer = "OGRGeoJSON",
        disambiguateFIDs = TRUE,
        verbose = FALSE,
        require_geomType = geometry_type,
        stringsAsFactors = FALSE,
        ...)
}
