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

limit_get_61 <- ratelimitr::limit_rate(
    httr::GET,
    ratelimitr::rate(n = 6, period = 1)
)

#' @import assertthat
search_get <- function(url) {
    response <- limit_get_61(httr::build_url(url))
    search_process(response)
}

search_process <- function(response) {
    httr::stop_for_status(response)
    txt <- httr::content(response, as = "text")
    lst <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
    structure(lst, class = c("mapzen_geo_list", "geo_list"))
}
