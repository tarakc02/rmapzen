search_path <- function(endpoint) {
    spath <- mz_get_host("search")$path
    if (is.null(spath)) {
        res <- endpoint
    } else {
        res <- paste(spath, endpoint, sep = "/")
    }
    res
}

search_url <- function(endpoint, ..., api_key) {
    if (is.null(api_key)) api_key <- mz_key(which = "search")
    query <- list(...)

    query <- c(
        api_key = api_key,
        query
    )

    structure(
        list(
            scheme = mz_get_host("search")$scheme,
            hostname = mz_get_host("search")$hostname,
            path = search_path(endpoint),
            query = query),
        class = "url"
    )
}

search_GET <- httr::GET

#' @import assertthat
search_get <- function(url) {
    response <- search_GET(httr::build_url(url))
    search_process(response)
}

search_process <- function(response) {
    httr::stop_for_status(response)

    # update the usage statistics
    header <- httr::headers(response)
    mz_update_usage(header, "search")

    txt <- httr::content(response, as = "text")
    lst <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
    structure(lst, header = header, class = c("mapzen_geo_list", "geo_list"))
}
