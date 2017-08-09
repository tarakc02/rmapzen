matrix_path <- function(endpoint) endpoint

matrix_url <- function(endpoint, ..., api_key = mz_key()) {
    query <- list(...)
    query <- c(api_key = api_key, query)
    structure(
        list(
            scheme = "https",
            hostname = "matrix.mapzen.com",
            path = matrix_path(endpoint),
            query = query),
        class = "url"
    )
}

matrix_GET <- httr::GET
