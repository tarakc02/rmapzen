mz_provider <- function(hostname, path = "v1", key, scheme = "https") {
    if (!assertthat::is.string(hostname)) stop("hostname must be a string")
    if (!assertthat::is.string(path)) stop("path must be a string")
    if (!assertthat::is.string(scheme)) stop("scheme must be a string")

    path <- gsub("/$", "", path)

    list(scheme = scheme,
         path = path,
         hostname = hostname,
         key = key)
}

mz_set_host <- function(which, provider) {
    if (!assertthat::is.string(which)) stop("which must be a string")
    which <- toupper(which)
    if (!which %in% c("SEARCH", "MATRIX", "TILE"))
        stop("which must be one of: SEARCH, MATRIX, TILE")
    optname <- paste0("RMAPZEN_", which, "_HOST")
    if (!is.null(getOption(optname))) warning("Replacing existing ", which, " host")
    options(structure(list(provider), names = optname))
}

mz_get_host <- function(which) {
    if (!assertthat::is.string(which)) stop("which must be a string")
    which <- toupper(which)
    if (!which %in% c("SEARCH", "MATRIX", "TILE"))
        stop("which must be one of: SEARCH, MATRIX, TILE")
    res <- getOption(paste0("RMAPZEN_", which, "_HOST"))
    if (is.null(res)) stop("no provider set for ", which, ". See ?mz_set_host")
    res
}

mz_set_search_host_geocode.earth <- function(key) {
    p <- mz_provider(hostname = "api.geocode.earth",
                     path = "v1",
                     key = key)
    mz_set_host("search", p)
}

mz_set_search_host_nyc_geosearch <- function() {
    p <- mz_provider(hostname = "geosearch.planninglabs.nyc",
                     path = "v1",
                     key = NULL)
    mz_set_host("search", p)
}

mz_set_tile_host_nextzen <- function(key) {
    p <- mz_provider(hostname = "tile.nextzen.org",
                     path = "tilezen/vector/v1",
                     key = key)
    mz_set_host("tile", p)

}
