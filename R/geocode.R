#' Geocode an address or other location
#'
#' This is a convenience function that calls \code{\link{mz_search}} to retrieve
#' latitude and longitude.
#'
#' @param location An address or other suitably specific search string
#' @param api_key Your Mapzen API key, defaults to the value of the MAPZEN_KEY environment variable
#'
#' @return A tibble, with the parsed address used to retrieve the geocode, lat/lon,
#' and the confidence (between 0 and 1)
#'
#' @examples
#' \dontrun{
#' mz_geocode("1600 Pennsylvania Ave., Washington DC")
#'
#' # can also be a landmark
#' mz_geocode("Statue of Liberty, New York")
#' }
#'
#' @seealso \code{\link{mz_search}}, \code{\link{mz_reverse_geocode}}
#' @export
mz_geocode <- function(location, api_key = NULL) {
    result <- mz_search(location, size = 10, api_key = api_key)

    if (is.null(result$features) || length(result$features) <= 0L)
        stop("Tried to geocode ", location, " but there were no results")
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
    structure(res, class = c("mz_geocode_result", class(res)))
}
