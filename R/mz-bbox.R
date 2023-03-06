#' Get the bounding box
#'
#' Returns the bottom left and top right corners of the box that contains a
#' mapzen object (\code{mz_geo_list}, \code{mz_isochrone_list},
#' or \code{mapzen_vector_tiles}).
#' In the case of \code{mz_rect}, creates such a box from the specified
#' coordinates. The returned value can be used directly as the
#' \code{boundary.rect} parameter for \code{\link{search}} functions, as well as
#' converted to x, y, zoom coordinates to use with \code{\link{mz_vector_tiles}}.
#'
#' @param geo A mapzen geo list or isochrone list
#' @param min_lon,min_lat,max_lon,max_lat The bottom left and top right corners,
#' expressed as latitude and longitude, of a rectangle.
#'
#' @return A single-row tibble with columns \code{min_lon}, \code{min_lat},
#' \code{max_lon}, \code{max_lat}.
#'
#' @name mz_bbox
#'
#' @examples
#' mz_rect(min_lon = -122.2856, min_lat = 37.73742, max_lon = -122.1749, max_lat = 37.84632)
#' mz_bbox(oakland_public)
#'
#' @export
mz_bbox <- function(geo) UseMethod("mz_bbox")

#' @rdname mz_bbox
#' @export
mz_bbox.mapzen_geo_list <- function(geo) {
    bbox <- geo$bbox
    if (is.null(bbox) || length(bbox) != 4L) {
        warning("Unable to read bounding box, returning NA")
        return(
            tibble::tibble(
                min_lon = NA_real_,
                min_lat = NA_real_,
                max_lon = NA_real_,
                max_lat = NA_real_
            )
        )
    }

    res <- tibble::tibble(
        min_lon = as.numeric(bbox[[1]]),
        min_lat = as.numeric(bbox[[2]]),
        max_lon = as.numeric(bbox[[3]]),
        max_lat = as.numeric(bbox[[4]])
    )

    class(res) <- c("mz_bbox", class(res))
    res
}

#' @rdname mz_bbox
#' @export
mz_bbox.mapzen_isochrone_list <- function(geo)  mz_bbox(as_sf(geo)) 

layer_coords <- function(layer) {
    feature_type <- function(feature) {
        known_types <- c(
            "Point", "MultiPoint",
            "LineString", "MultiLineString",
            "Polygon", "MultiPolygon"
        )
        res <- feature$geometry$type
        if (is.null(res))
            stop("Found a feature without a geometry type")
        if (!res %in% known_types)
            stop("Feature type not recognized: ", feature_type, "\n",
                 "expected one of: ", paste(known_types, collapse = ", "))
        res
    }

    position <- function(coords) {
        data.frame(lon = as.numeric(coords[[1]]),
                   lat = as.numeric(coords[[2]]))
    }

    positions <- function(coords) {
        res <- lapply(coords, position)
        do.call("rbind", res)
    }

    positions_array <- function(coords) {
        res <- lapply(coords, positions)
        do.call("rbind", res)
    }

    point_coords <- function(feature)
        position(feature$geometry$coordinates)

    multipoint_coords <- function(feature) {
        positions(feature$geometry$coordinates)
    }

    line_coords <- function(feature) multipoint_coords(feature)

    multiline_coords <- function(feature) {
        positions_array(feature$geometry$coordinates)
    }

    polygon_coords <- function(feature) multiline_coords(feature)
    multipolygon_coords <- function(feature) {
        res <- lapply(feature$geometry$coordinates, positions_array)
        do.call("rbind", res)
    }

    coords <- function(feature)
        switch(feature_type(feature),
               Point = point_coords(feature),
               MultiPoint = multipoint_coords(feature),
               LineString = line_coords(feature),
               MultiLineString = multiline_coords(feature),
               Polygon = polygon_coords(feature),
               MultiPolygon = multipolygon_coords(feature),
               default = stop("Unrecognized feature"))

    res <- lapply(layer$features, coords)
    do.call("rbind", res)
}

#' @export
mz_bbox.mapzen_vector_layer <- function(geo) {
    res <- layer_coords(geo)
    mz_rect(min_lon = min(res$lon),
            min_lat = min(res$lat),
            max_lon = max(res$lon),
            max_lat = max(res$lat))
}

#' @export
mz_bbox.mapzen_vector_tiles <- function(geo) {
    res <- lapply(geo, layer_coords)
    res <- do.call("rbind", res)
    structure(
        data.frame(
            min_lon = min(res$lon),
            min_lat = min(res$lat),
            max_lon = max(res$lon),
            max_lat = max(res$lat)
        ), class = c("mz_bbox", "tbl_df", "tbl", "data.frame")
    )
}

#' @rdname mz_bbox
#' @export
mz_rect <- function(min_lon, min_lat, max_lon, max_lat) {
    assert_that(is.number(min_lon),
                is.number(min_lat),
                is.number(max_lon),
                is.number(max_lat))
    structure(
        data.frame(
            min_lon = min_lon,
            min_lat = min_lat,
            max_lon = max_lon,
            max_lat = max_lat
        ), class = c("mz_bbox", "tbl_df", "tbl", "data.frame")
    )
}

#' @export
mz_bbox.default <- function(geo) {
    bbox <- sf::st_bbox(geo)
    mz_rect(
        min_lon = bbox[["xmin"]],
        min_lat = bbox[["ymin"]],
        max_lon = bbox[["xmax"]],
        max_lat = bbox[["ymax"]]
    )
}
