#' @import assertthat
#' @export
mz_reverse <- function(
    point, size = NULL, layers = NULL, sources = NULL,
    api_key = mz_key()
) {
    point <- unwrap(point, "point", c("lat", "lon"))

    query <- c(
        point, size = size,
        sources = sources, layers = layers,
        api_key = api_key
    )
    query <- query[!is.null(query)]

    do.call(mz_search_main, c(endpoint = "reverse", query))
}
