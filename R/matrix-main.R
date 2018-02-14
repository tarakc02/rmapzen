matrix_path <- function(endpoint) {
    matpath <- mz_get_host("matrix")$path
    if (is.null(matpath)) {
        res <- endpoint
    } else {
        res <- paste(matpath, endpoint, sep = "/")
    }
    res
}
matrix_url <- function(endpoint, ..., api_key) {
    if (is.null(api_key)) api_key <- mz_key(which = "matrix")
    query <- list(...)
    query <- c(api_key = api_key, query)
    structure(
        list(
            scheme = mz_get_host("matrix")$scheme,
            hostname = mz_get_host("matrix")$hostname,
            path = matrix_path(endpoint),
            query = query),
        class = "url"
    )
}

matrix_GET <- httr::GET
