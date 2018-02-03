matrix_path <- function(endpoint) endpoint

matrix_url <- function(endpoint, ..., api_key = mz_key()) {
    query <- list(...)
    query <- c(api_key = api_key, query)
    structure(
        list(
            scheme = "https",
            hostname = getOption("RMAPZEN.matrix.host"),
            path = matrix_path(endpoint),
            query = query),
        class = "url"
    )
}

matrix_GET <- httr::GET
