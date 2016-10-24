mz_search_url <- function(search_term, ..., api_key = mz_key()) {
    query <- list(...)
    query <- c(
        api_key = api_key,
        text = search_term,
        query
    )
    url <- structure(
        list(
            scheme = "https",
            hostname = "search.mapzen.com",
            path = "v1/search",
            query = query), class = "url")
    httr::build_url(url)
}

mz_search <- function(search_term, ..., api_key = mz_key()) {
    url <- mz_search_url(search_term, ..., api_key = api_key)
    raw_response <- httr::GET(url)
    mz_search_process(raw_response)
}

mz_search_process <- function(raw_response) {
    txt <- httr::content(raw_response, as = "text")
    lst <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
    structure(lst, class = "geo_list")
}

# geocode <- function(location, api_key = mz_key()) {
#     result <- mz_search(location, size = 1, api_key = api_key)
#     as.numeric(result$features[[1]]$geometry$coordinates)
# }
