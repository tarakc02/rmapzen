#' Specify tile coordinates
#'
#' \code{\link{mz_vector_tiles}} requires tile coordinates or some other
#' specification of the region that is to be drawn. \code{\link{mz_vector_tiles}}
#' will automatically convert its inputs to vector tiles, so you generally won't
#' need to use this function directly.
#'
#' @param x integer vector of x-coordinates
#' @param y integer vector of y-coordinates
#' @param z integer between 0 and 19 specifying the zoom level
#' @param obj An object that can be converted to tile coordinates
#' @param ... Other arguments passed on to methods
#' @param height Height in pixels
#' @param width Width in pixels
#'
#' @examples
#' mz_tile_coordinates(19293, 24641, 16)
#'
#' ## can specify multiple contiguous tiles:
#' mz_tile_coordinates(19293:19294, 24641:24642, 16)
#'
#' ## a rectangular bounding box can be converted to tile coordinates:
#' as.mz_tile_coordinates(mz_rect(min_lon = -122.2856,
#'                                min_lat = 37.73742,
#'                                max_lon = -122.1749,
#'                                max_lat = 37.84632))
#'
#' ## zoom level is calculated based on desired pixel dimensions of the map:
#' as.mz_tile_coordinates(mz_rect(min_lon = -122.2856,
#'                                min_lat = 37.73742,
#'                                max_lon = -122.1749,
#'                                max_lat = 37.84632), height = 750, width = 1000)
#'
#' ## a bounding box can also be calculated:
#' as.mz_tile_coordinates(mz_bbox(oakland_public))
#'
#' @seealso \code{\link{mz_vector_tiles}}, \code{\link{mz_bbox}}
#'
#' @name mz_tile_coordinates
#' @export
mz_tile_coordinates <- function(x, y, z) {
    assert_that(is.numeric(x), is.numeric(y), is.number(z))
    x <- seq(from = min(x), to = max(x), by = 1)
    y <- seq(from = min(y), to = max(y), by = 1)

    df <- expand.grid(x = x, y = y, z = z)

    res <- Map(function(.x, .y, .z) {
        list(x = .x, y = .y, z = .z)
    }, df$x, df$y, df$z)

    structure(
        res,
        class = c("mz_tile_coordinates", "list")
    )
}

#' @rdname mz_tile_coordinates
#' @export
as.mz_tile_coordinates <- function(obj, ...) UseMethod("as.mz_tile_coordinates")

#' @rdname mz_tile_coordinates
#' @export
as.mz_tile_coordinates.mz_tile_coordinates <- function(obj, ...) obj

#' @rdname mz_tile_coordinates
#' @export
as.mz_tile_coordinates.mz_bbox <- function(obj, ..., height = 375, width = 500) {
    # given a bounding box defined by bottom left and top right corners,
    # convert to vector tile coordinates (x, y, zoom)

    # http://stackoverflow.com/a/13274361
    getBoundsZoomLevel <- function(bounds, mapDim) {
        WORLD_DIM = list( height = 256, width= 256 )
        ZOOM_MAX = 20;

        latRad <- function(lat) {
            sin = sin(lat * pi / 180)
            radX2 = log((1 + sin) / (1 - sin)) / 2
            pmax(pmin(radX2, pi), -pi) / 2
        }

        zoom <- function(mapPx, worldPx, fraction) {
            floor(log(mapPx / worldPx / fraction) / log(2))
        }

        latFraction = (latRad(bounds[["max_lat"]]) - latRad(bounds[["min_lat"]])) / pi

        lngDiff = bounds[["max_lon"]] - bounds[["min_lon"]]
        if (lngDiff < 0)
            lngFraction <- lngDiff + 360
        else lngFraction <- lngDiff
        lngFraction <- lngFraction / 360

        latZoom = zoom(mapDim$height, WORLD_DIM$height, latFraction)
        lngZoom = zoom(mapDim$width, WORLD_DIM$width, lngFraction)

        pmin(latZoom, lngZoom, ZOOM_MAX)
    }

    # http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#R
    deg2num <- function(pt, zoom){
        lat_deg <- pt[["lat"]]
        lon_deg <- pt[["lon"]]
        lat_rad <- lat_deg * pi /180
        n <- 2.0 ^ zoom
        xtile <- floor((lon_deg + 180.0) / 360.0 * n)
        ytile = floor((1.0 - log(tan(lat_rad) + (1 / cos(lat_rad))) / pi) / 2.0 * n)
        return( c(xtile, ytile))
    }

    zoom <- getBoundsZoomLevel(obj, list(height = height, width = width))

    # tile for bottom left corner:
    ll <- deg2num(mz_location(
        lat = obj[["min_lat"]],
        lon = obj[["min_lon"]]
    ), zoom = zoom)

    # for top right:
    ur <- deg2num(mz_location(
        lat = obj[["max_lat"]],
        lon = obj[["max_lon"]]
    ), zoom = zoom)

    xs <- unique(c(ll[1], ur[1]))
    ys <- unique(c(ll[2], ur[2]))

    mz_tile_coordinates(x = xs, y = ys, z = zoom)
}
