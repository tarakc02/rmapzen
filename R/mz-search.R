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

check_search_term <- function(search_term) {
    if (!inherits(search_term, "character"))
        stop("Invalid search term: search term should be a string")
    if (length(search_term) > 1L)
        stop("Multiple search terms entered, only one is allowed")
    if (trimws(search_term) == "")
        stop("No text in the search term")
}

mz_search <- function(search_term, ..., api_key = mz_key()) {
    check_search_term(search_term)
    url <- mz_search_url(search_term, ..., api_key = api_key)
    raw_response <- httr::GET(url)
    mz_search_process(raw_response)
}

mz_resp_to_list <- function(raw_response) {
    txt <- httr::content(raw_response, as = "text")
    jsonlite::fromJSON(txt, simplifyVector = FALSE)
}

mz_search_process <- function(raw_response) {
    httr::stop_for_status(raw_response)
    lst <- mz_resp_to_list(raw_response)
    structure(lst, class = "geo_list")
}
