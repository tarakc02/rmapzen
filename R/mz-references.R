#' Reference lists
#'
#' Lists of sources, layers, and countries, as they are expected to appear in
#' the \code{\link{mz_search}} functions. These data objects are provided as a
#' convenience, to be able to quickly and easily look up acceptable values for
#' the optional arguments of search functions. Object names match the argument
#' names for which they are appropriate. So \code{mz_sources} provide acceptable
#' arguments for the \code{source} argument in \code{\link{mz_search}},
#' \code{mz_layers} for the \code{layer} argument, and \code{\link{mz_countries}}
#' for the \code{boundary.country} argument. Mapzen's documentation
#' (\url{https://github.com/pelias/documentation/}) explains more about
#' each of these arguments.
#'
#' @examples
#' \dontrun{
#' # look for YMCAs in Jamaica:
#' # Note that boundary.country is supplied via ISO3166 code,
#' # but mz_countries will look up the code
#' mz_search("YMCA",
#'           boundary.country = mz_countries$Jamaica,
#'           layers = c(mz_layers$venue, mz_layers$address))
#' }
#'
#' @name mapzen_references
NULL

#' @rdname mapzen_references
#' @export
mz_sources <- list(
    openstreetmap = "osm",
    osm = "osm",
    openaddresses = "oa",
    oa = "oa",
    whosonfirst = "wof",
    wof = "wof",
    geonames = "gn",
    gn = "gn"
)

#' @rdname mapzen_references
#' @export
mz_layers <- list(
    venue = "venue",
    address = "address",
    street = "street",
    country = "country",
    macroregion = "macroregion",
    region = "region",
    macrocounty = "macrocounty",
    county = "county",
    locality = "locality",
    local_admin = "local_admin",
    borough = "borough",
    neighbourhood = "neighbourhood",
    coarse = "coarse"
)

make_mz_countries <- function() {
    iso3166 <- maps::iso3166
    ind <- grepl("^[A-Z]+$", iso3166$a2)
    iso3166 <- iso3166[ind, , drop = FALSE]

    v1 <- data.frame(desc = iso3166$a2, code = iso3166$a2, stringsAsFactors = FALSE)
    v2 <- v1 <- data.frame(desc = iso3166$a3, code = iso3166$a2, stringsAsFactors = FALSE)
    v3 <- data.frame(desc = iso3166$ISOname, code = iso3166$a2, stringsAsFactors = FALSE)
    v4 <- data.frame(desc = iso3166$mapname, code = iso3166$a2, stringsAsFactors = FALSE)

    v <- Reduce(dplyr::union, list(v2, v3, v4), init = v1)
    v <- dplyr::distinct(v)
    structure(as.list(v$code), names = v$desc)
}

#' @rdname mapzen_references
#' @export
mz_countries <- make_mz_countries()
