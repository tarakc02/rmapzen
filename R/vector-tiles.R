vector_get <- function(url) {
    response <- vector_GET(httr::build_url(url))
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
#' @param tile_coordinates an \code{\link{mz_tile_coordinates}} object, or something
#' that can be coerced to one (including the output of \code{\link{mz_bbox}})
#' @param ... Arguments passed on to \code{\link{as.mz_tile_coordinates}}
#'
#' Multiple tiles will be stitched together and returned as one object.
#'
#' @export
mz_vector_tiles <- function(tile_coordinates, ...) {
    tile_coordinates <- as.mz_tile_coordinates(tile_coordinates, ...)

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
