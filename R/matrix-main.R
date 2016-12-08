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

matrix_GET <- ratelimitr::limit_rate(
    httr::GET,
    ratelimitr::rate(n = 2, period = 1)
)
