search_path <- function(endpoint) paste0("v1/", endpoint)

search_url <- function(endpoint, ..., api_key = mz_key()) {
    query <- list(...)

    query <- c(
        api_key = api_key,
        query
    )

    structure(
        list(
            scheme = "https",
            hostname = "search.mapzen.com",
            path = search_path(endpoint),
            query = query),
        class = "url"
    )
}

search_GET <- ratelimitr::limit_rate(
    httr::GET,
    ratelimitr::rate(n = 6, period = 1)
)

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
