recalculate_ids <- function(features) {
    features$features <- lapply(features$features, function(feature) {
        feature$properties$id <- NULL
        feature$properties$id <- digest::digest(feature$properties)
        return(feature)
    })
    features
}

as_json <- function(geo) {
    geojsonio::as.json(geo)
}

#' Coerce a Mapzen response to an Spatial*DataFrame
#'
#' Coerces responses to SpatialPoints, SpatialLines, or SpatialPolygons data
#' frames.
#'
#' @param geo The object to be converted
#' @param geometry_type "point", "line", or "polygon" -- can be left NULL and only
#' needs to be specified when an object contains multiple geometry types.
#' @param ... not currently used
#'
#' @name as_sp
#' @export
as_sp <- function(geo, ...) UseMethod("as_sp")

#' @rdname as_sp
#' @export
as_sp.geo_list <- function(geo, ...) {
    if (length(geo$features) == 0L)
        stop("Cannot convert to sp: no data")
    geojsonio::geojson_sp(geo, stringsAsFactors = FALSE)
}

#' @rdname as_sp
#' @export
as_sp.mapzen_vector_layer <- function(geo, ..., geometry_type = NULL) {
    if (length(geo$features) == 0L)
        stop("Cannot convert to sf: empty layer")
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

    features <- recalculate_ids(features)
    features <- collapse_properties(features)
    json <- as_json(features)
    res <- rgdal::readOGR(
        json,
        layer = "OGRGeoJSON",
        disambiguateFIDs = TRUE,
        verbose = FALSE,
        require_geomType = geometry_type,
        stringsAsFactors = FALSE,
        ...)
    if (geometry_type == "wkbPolygon"){
        polygons <- maptools::unionSpatialPolygons(res, IDs = res@data$id)

        return(
            sp::SpatialPolygonsDataFrame(
                polygons,
                data = base::unique(res@data),
                match.ID = "id"
            )
        )
    }
    else return(res)
}
