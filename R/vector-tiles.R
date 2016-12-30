vector_get <- function(url) {
    response <- vector_GET(httr::build_url(url))
    vector_process(response)
}

vector_process <- function(response) {
    httr::stop_for_status(response)
    header <- httr::headers(response)
    txt <- httr::content(response, as = "text")
    lst <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
    structure(
        lst,
        class = "mapzen_vector_tiles"
    )
}

mz_tile_coordinates <- function(x, y, z) {
    assert_that(is.number(x), is.number(y), is.number(z))
    structure(
        list(x = x, y = y, z = z),
        class = c("mz_tile_coordinates", "list")
    )
}

as.mz_tile_coordinates <- function(obj, ...) UseMethod("as.mz_tile_coordinates")
as.mz_tile_coordinates.mz_tile_coordinates <- function(obj, ...) obj
as.mz_tile_coordinates.mz_bbox <- function(obj, height = 375, width = 500, ...) {
    # given a bounding box defined by bottom left and top right corners,
    # convert to vector tile coordinates (x, y, zoom)

    center_bbox <- function(bbox) {
        lon <- mean(c(bbox[["min_lon"]], bbox[["max_lon"]]))
        lat <- mean(c(bbox[["min_lat"]], bbox[["max_lat"]]))
        mz_location(lat = lat, lon = lon)
    }

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
        lngFraction <-
            if (lngDiff < 0)
                lngFraction <- lngDiff + 360
        else lngFraction <- lngDiff
        lngFraction <- lngFraction / 360

        latZoom = zoom(mapDim$height, WORLD_DIM$height, latFraction)
        lngZoom = zoom(mapDim$width, WORLD_DIM$width, lngFraction)

        # zoom goes from 0-19, instead of 1-20
        pmin(latZoom, lngZoom, ZOOM_MAX) - 1
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
    center <- deg2num(center_bbox(obj), zoom)
    mz_tile_coordinates(
        center[1], center[2], zoom
    )

}

mz_vector_tiles <- function(tile_coordinates, layers = "all") {
    tile_coordinates <- as.mz_tile_coordinates(tile_coordinates)
    url <- vector_url(
        x = tile_coordinates$x,
        y = tile_coordinates$y,
        z = tile_coordinates$z,
        layers = layers,
        format = "json"
    )
    vector_get(url)

}

vector_GET <- ratelimitr::limit_rate(
    httr::GET,
    ratelimitr::rate(n = 100, period = 1),
    ratelimitr::rate(n = 2000, period = 60)
)

vector_path <- function(layers, x, y, z, format) {
    res <- paste("mapzen", "vector", "v1", layers, z, x, y, sep = "/")
    paste(res, format, sep = ".")
}

vector_url <- function(x, y, z, layers = "all", format = "json", api_key = mz_key()) {
    structure(
        list(
            scheme = "https",
            hostname = "tile.mapzen.com",
            path = vector_path(layers, x, y, z, format),
            query = list(
                api_key = api_key
            )
        ),
        class = "url"
    )
}
