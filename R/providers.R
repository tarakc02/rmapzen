#' Configure provider information
#'
#' rmapzen works with most implementations of PELIAS. This function defines the
#' base URL for a particular API provider, and can be used to provider the
#' \code{provider} argument to \code{\link{mz_set_host}}.
#'
#' @param hostname The hostname in the API URL, for instance \code{www.example.com}
#' @param path Specific path that all API requests must include, e.g. "v1"
#' @param key API key for this provider, if required
#' @param scheme The scheme for the URL, should always be "https"
#'
#' @seealso \code{\link{mz_set_host}}
#' @export
mz_provider <- function(hostname, path = NULL, key = NULL, scheme = "https") {
    if (!assertthat::is.string(hostname)) stop("hostname must be a string")
    if (!is.null(path) && !assertthat::is.string(path))
        stop("path must be a string")
    if (!assertthat::is.string(scheme)) stop("scheme must be a string")

    path <- gsub("/$", "", path)

    list(scheme = scheme,
         path = path,
         hostname = hostname,
         key = key)
}

#' Set up a host provider for a PELIAS service
#'
#' rmapzen works with most implementations of PELIAS. Use this function to set
#' up the basic information required to connect to a particular provider.
#' Provider-specific setup functions include information to set up known
#' providers.
#'
#' @param which One of "search", "matrix", or "tile"
#' @param provider A provider, created using \code{\link{mz_provider}}
#' @param key API key
#'
#' @seealso \code{\link{mz_provider}}
#' @export
mz_set_host <- function(which, provider) {
    if (!assertthat::is.string(which)) stop("which must be a string")
    which <- toupper(which)
    if (!which %in% c("SEARCH", "MATRIX", "TILE"))
        stop("which must be one of: SEARCH, MATRIX, TILE")
    optname <- paste0("RMAPZEN_", which, "_HOST")
    if (!is.null(getOption(optname))) warning("Replacing existing ", which, " host")
    options(structure(list(provider), names = optname))
}

#' @rdname mz_set_host
mz_get_host <- function(which) {
    if (!assertthat::is.string(which)) stop("which must be a string")
    which <- toupper(which)
    if (!which %in% c("SEARCH", "MATRIX", "TILE"))
        stop("which must be one of: SEARCH, MATRIX, TILE")
    res <- getOption(paste0("RMAPZEN_", which, "_HOST"))
    if (is.null(res)) stop("no provider set for ", which, ". See ?mz_set_host")
    res
}

#' @rdname mz_set_host
#' @export
mz_set_search_host_geocode.earth <- function(key = Sys.getenv("GEOCODE.EARTH_KEY")) {
    if (is.null(key)) stop("mz_set_search_host_geocode.earth requires a key")
    p <- mz_provider(hostname = "api.geocode.earth",
                     path = "v1",
                     key = key)
    mz_set_host("search", p)
}

#' @rdname mz_set_host
#' @export
mz_set_search_host_nyc_geosearch <- function() {
    p <- mz_provider(hostname = "geosearch.planninglabs.nyc",
                     path = "v1",
                     key = NULL)
    mz_set_host("search", p)
}

#' @rdname mz_set_host
#' @export
mz_set_tile_host_nextzen <- function(key = Sys.getenv("NEXTZEN_KEY")) {
    if (is.null(key)) stop("mz_set_tile_host_nextzen requires a key")
    p <- mz_provider(hostname = "tile.nextzen.org",
                     path = "tilezen/vector/v1",
                     key = key)
    mz_set_host("tile", p)

}
