#' @export
mz_geocode <- function(location, api_key = mz_key()) {
    result <- tryCatch(
        mz_search(location, size = 10, api_key = api_key),
        error = function(c) list(features = list())
    )

    if (is.null(result$features) || length(result$features) <= 0L)
        return(
            tibble::data_frame(
                geocode_address = location,
                geocode_longitude = NA,
                geocode_latitude = NA,
                geocode_confidence = NA
            )
        )
    confidence <- as.numeric(result$features[[1]]$properties$confidence[[1]])
    longitude <- as.numeric(result$features[[1]]$geometry$coordinates[[1]])
    latitude <- as.numeric(result$features[[1]]$geometry$coordinates[[2]])
    res <- tibble::data_frame(
        geocode_address = result$features[[1]]$properties$label,
        geocode_longitude = longitude,
        geocode_latitude = latitude,
        geocode_confidence = confidence
    )
    if (nrow(res) > 1)
        stop("There was an error while geocoding")
    res
}
