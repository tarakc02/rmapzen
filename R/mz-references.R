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
    iso3166 <- dplyr::filter(iso3166, grepl("^[A-Z]+$", a2))

    v1 <- dplyr::transmute(iso3166, desc = a2, code = a2)
    v2 <- dplyr::transmute(iso3166, desc = a3, code = a2)
    v3 <- dplyr::transmute(iso3166, desc = ISOname, code = a2)
    v4 <- dplyr::transmute(iso3166, desc = mapname, code = a2)

    v <- Reduce(dplyr::union, list(v2, v3, v4), init = v1)
    v <- dplyr::distinct(v)
    structure(as.list(v$code), names = v$desc)
}

#' @rdname mapzen_references
#' @export
mz_countries <- make_mz_countries()
