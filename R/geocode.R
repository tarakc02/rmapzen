search_results_to_geocodes <- function(result, location) {
    if (is.null(result$features) || length(result$features) <= 0L)
        stop("Tried to geocode '", location, "' but there were no results",
             call. = FALSE)
    confidence <- as.numeric(result$features[[1]]$properties$confidence[[1]])
    longitude <- as.numeric(result$features[[1]]$geometry$coordinates[[1]])
    latitude <- as.numeric(result$features[[1]]$geometry$coordinates[[2]])
    res <- tibble::tibble(
        geocode_address = result$features[[1]]$properties$label,
        geocode_longitude = longitude,
        geocode_latitude = latitude,
        geocode_confidence = confidence
    )
    if (nrow(res) > 1)
        stop("There was an error while geocoding")
    structure(res, class = c("mz_geocode_result", class(res)))
}

#' Geocode an address or other location
#'
#' This is a convenience function that calls \code{\link{mz_search}} to retrieve
#' latitude and longitude.
#'
#' @param location An address or other suitably specific search string
#' @param ... Additional arguments passed on to \code{\link{mz_search}}
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
mz_geocode <- function(location, ...) {
    search_results_to_geocodes(mz_search(location, size = 10, ...),
                               location = location)
}

#' Geocode a structured address
#'
#' \code{\link{mz_geocode}} allows you to search using an unstructured string of
#' text, but if your address data has more structure (eg separate columns for
#' address, city, state, zip), then using the structured search service may
#' provide more precision. For more information, see
#' \url{https://github.com/pelias/documentation/}. Note that
#' all of the arguments are optional, but at least one of them must be non-NULL.
#' Furthermore, \code{postalcode} can not be used by itself.
#'
#' @inheritParams mz_structured_search
#' @param ... Arguments passed on to \code{\link{mz_structured_search}}
#'
#' @return A tibble, with the parsed address used to retrieve the geocode, lat/lon,
#' and the confidence (between 0 and 1)
#'
#' @seealso \code{\link{mz_geocode}}, \code{\link{mz_structured_search}}
#' @export
mz_geocode_structured <- function(address = NULL,
                                  neighbourhood = NULL,
                                  borough = NULL,
                                  locality = NULL,
                                  county = NULL,
                                  region = NULL,
                                  postalcode = NULL,
                                  country = NULL, ...) {
    # get info for error message if necessary:
    info <- list(address, neighbourhood, borough,
                 locality, county, region, postalcode, country)
    info <- Filter(function(x) !is.null(x), info)
    info <- paste(info, collapse = ", ")

    search_results_to_geocodes(mz_structured_search(
        address = address,
        neighbourhood = neighbourhood,
        borough = borough,
        locality = locality,
        county = county,
        region = region,
        postalcode = postalcode,
        size = 10), location = info)
}
