vector_get <- function(url, ...) {
    response <- vector_GET(httr::build_url(url), ...)
    vector_process(response)
}

vector_process <- function(response) {
    httr::stop_for_status(response)
    header <- httr::headers(response)
    mz_update_usage(header, "tile")
    txt <- httr::content(response, as = "text", encoding = "UTF-8")
    lst <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
    structure(
        lst,
        header = header,
        class = "mapzen_vector_tiles"
    )
}

#' Request vector tile data
#'
#' From \url{https://tilezen.readthedocs.io/en/latest/}: "Vector tiles are
#' square-shaped collections of geographic data that contain the map feature
#' geometry, such as lines and points."
#'
#' Multiple tiles are stitched together and returned as one object. Individual
#' layers can be converted to \code{sf} or \code{sp}, making it possible to
#' draw each layer with custom styles.
#'
#' @param tile_coordinates an \code{\link{mz_tile_coordinates}} object, or something
#' that can be coerced to one (including the output of \code{\link{mz_bbox}})
#' @param ... Arguments passed on to \code{\link{as.mz_tile_coordinates}}.
#' @param Origin optional, specify Origin URL in request header
#'
#' @return A list of tile layers (such as "water", "buildings", "roads", etc.).
#' Each layer is an object of class \code{mapzen_vector_layer}, which can be converted
#' to \code{sf} or \code{sp} using \code{\link{as_sf}} or \code{\link{as_sp}}
#'
#' @examples
#' \dontrun{
#' # vector tile at x = 19293, y = 24641, and zoom level 16
#' mz_vector_tiles(mz_tile_coordinates(19293, 24641, 16))
#'
#' # multiple contiguous tiles will be stitched together
#' # this returns the result of stitching together 4 tiles
#' mz_vector_tiles(mz_tile_coordinates(19293:19294, 24641:24642, 16))
#'
#' # can also use a bounding box:
#' mz_vector_tiles(mz_rect(min_lon = -122.2856,
#'                         min_lat = 37.73742,
#'                         max_lon = -122.1749,
#'                         max_lat = 37.84632))
#'
#' # mz_bbox returns a bounding box for any Mapzen object
#' mz_vector_tiles(mz_bbox(oakland_public))
#'
#' # bounding boxes are automatically converted to tile coordinates,
#' # with the zoom level based on the desired size in pixels of the final map
#' mz_vector_tiles(mz_bbox(oakland_public), height = 750, width = 1000)
#' }
#'
#' @seealso \code{\link{mz_tile_coordinates}}
#' @export
mz_vector_tiles <- function(tile_coordinates, ..., Origin = NULL) {
    tile_coordinates <- as.mz_tile_coordinates(tile_coordinates, ...)

    get_tile <- function(tile_coordinates) {
        url <- vector_url(
            x = tile_coordinates$x,
            y = tile_coordinates$y,
            z = tile_coordinates$z,
            layers = "all",
            format = "json"
        )
        if (is.null(Origin)) vector_get(url)
        else vector_get(url, httr::add_headers(Origin = Origin))
    }

    all_tiles <- lapply(tile_coordinates, get_tile)

    if (length(tile_coordinates) == 1L) {
        all_tiles <- all_tiles[[1]]

        all_tiles <- lapply(all_tiles, function(x) {
            class(x) <- c("mapzen_vector_layer", "list")
            x
        })

        return(structure(
            all_tiles,
            class = c("mapzen_vector_tiles", "list")
        ))
    }

    structure(
        Reduce(stitch, all_tiles),
        class = c("mapzen_vector_tiles", "list")
    )
}

# stitches together two vector tiles
# it's on the caller to make sure they are actually adjacent
stitch <- function(tile1, tile2) {
    if (is.null(attr(tile1, "header"))) header <- attr(tile2, "header")
    else header <- attr(tile1, "header")

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
    structure(res, header = header)
}

vector_GET <- httr::GET

vector_path <- function(layers, x, y, z, format) {
    tilepath <- mz_get_host("tile")$path
    if (is.null(tilepath)) {
        res <- paste(layers, z, x, y, sep = "/")
    } else {
        res <- paste(tilepath, layers, z, x, y, sep = "/")
    }
    paste(res, format, sep = ".")
}

vector_url <- function(x, y, z, layers = "all", format = "json", api_key = mz_key(which = "tile")) {
    structure(
        list(
            scheme = mz_get_host("tile")$scheme,
            hostname = mz_get_host("tile")$hostname,
            path = vector_path(layers, x, y, z, format),
            query = list(
                api_key = api_key
            )
        ),
        class = "url"
    )
}
