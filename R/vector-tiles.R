vector_get <- function(url) {
    response <- vector_GET(httr::build_url(url))
    vector_process(response)
}

vector_process <- function(response) {
    httr::stop_for_status(response)
    header <- httr::headers(response)
    txt <- httr::content(response, as = "text", encoding = "UTF-8")
    lst <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
    structure(
        lst,
        class = "mapzen_vector_tiles"
    )
}

mz_tile_coordinates <- function(x, y, z) {
    assert_that(is.numeric(x), is.numeric(y), is.number(z))

    df <- expand.grid(x = x, y = y, z = z)

    res <- Map(function(.x, .y, .z) {
        list(x = .x, y = .y, z = .z)
    }, df$x, df$y, df$z)

    structure(
        res,
        class = c("mz_tile_coordinates", "list")
    )
}

as.mz_tile_coordinates <- function(obj, ...) UseMethod("as.mz_tile_coordinates")
as.mz_tile_coordinates.mz_tile_coordinates <- function(obj, ...) obj
as.mz_tile_coordinates.mz_bbox <- function(obj, height = 375, width = 500, ...) {
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

        # zoom goes from 0-19, instead of 1-20
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

mz_vector_tiles <- function(tile_coordinates) {
    tile_coordinates <- as.mz_tile_coordinates(tile_coordinates)

    get_tile <- function(tile_coordinates) {
        url <- vector_url(
            x = tile_coordinates$x,
            y = tile_coordinates$y,
            z = tile_coordinates$z,
            layers = "all",
            format = "json"
        )
        vector_get(url)
    }

    all_tiles <- lapply(tile_coordinates, get_tile)
    structure(
        Reduce(stitch, all_tiles),
        class = c("mapzen_vector_tiles", "list")
    )
}

# stitches together two vector tiles
# it's on the caller to make sure they are actually adjacent
stitch <- function(tile1, tile2) {
    layers <- unique(c(names(tile1), names(tile2)))
    res <- lapply(layers, function(layer) {
        features1 <- tile1[[layer]]
        features2 <- tile2[[layer]]
        if (is.null(features1)) tp <- features2$type
        else if (is.null(features2)) tp <- features1$type
        else {
            stopifnot(features1$type == features2$type)
            tp <- features1$type
        }

        all_features <- c(features1$features, features2$features)
        structure(
            list(type = tp,
                 features = all_features),
            class = c("mapzen_vector_layer", "list"))
    })
    names(res) <- layers
    res
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
