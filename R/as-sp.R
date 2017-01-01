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
as_sp.mapzen_vector_layer <- function(features, geometry_type = NULL, ...) {
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
