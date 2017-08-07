#' Reference lists
#'
#' Lists of sources, layers, and countries, as they are expected to appear in
#' the \code{\link{mz_search}} functions.
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
