mz_search_path <- function(endpoint) paste0("v1/", endpoint)

mz_search_url <- function(endpoint, ..., api_key = mz_key()) {
    query <- list(...)
    query <- c(
        api_key = api_key,
        query
    )
    url <- structure(
        list(
            scheme = "https",
            hostname = "search.mapzen.com",
            path = mz_search_path(endpoint),
            query = query), class = "url")
    httr::build_url(url)
}

#' @import assertthat
mz_search_main <- function(endpoint, ..., api_key = mz_key()) {
    url <- mz_search_url(endpoint, ..., api_key = api_key)
    response <- httr::GET(url)
    mz_search_process(response)
}

mz_search_process <- function(response) {
    httr::stop_for_status(response)
    txt <- httr::content(response, as = "text")
    lst <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
    structure(lst, class = "geo_list")
}
