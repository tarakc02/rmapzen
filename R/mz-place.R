#' @export
mz_place <- function(ids, ..., api_key) UseMethod("mz_place")

build_place_url <- function(ids, api_key = mz_key()) {
    ids <- string_array(ids)

    query <- list(
        ids = ids,
        api_key = api_key
    )

    do.call(search_url, c(endpoint = "place", query))
}

#' @export
mz_place.character <- function(ids, api_key = mz_key()) {
    search_get(build_place_url(ids, api_key))
}

#' @export
mz_place.mapzen_geo_list <- function(geolist, gid = "gid", api_key = mz_key()) {
    getgid <- function(feature) {
        feature$properties[[gid]]
    }

    ids <- vapply(geolist$features, getgid, FUN.VALUE = character(1))
    search_get(build_place_url(ids, api_key))
}

