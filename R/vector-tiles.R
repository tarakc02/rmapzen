as_sp.mapzen_vector_tiles <- function(features, geometry_type, ...) {
    json <- geojsonio::as.json(features)
    rgdal::readOGR(
        json,
        layer = "OGRGeoJSON",
        disambiguateFIDs = TRUE,
        verbose = FALSE,
        require_geomType = geometry_type,
        stringsAsFactors = FALSE,
        ...)
}

vector_GET <- ratelimitr::limit_rate(
    httr::GET,
    ratelimitr::rate(n = 100, period = 1),
    ratelimitr::rate(n = 2000, period = 60)
)

vector_path <- function(layers, z, x, y, format) {
    res <- paste("mapzen", "vector", "v1", layers, z, x, y, sep = "/")
    paste(res, format, sep = ".")
}

vector_url <- function(x, y, z, layers = "all", format = "topojson", api_key = mz_key()) {
    structure(
        list(
            scheme = "https",
            hostname = "tile.mapzen.com",
            path = vector_path(layers, z, x, y, format),
            query = list(
                api_key = api_key
            )
        ),
        class = "url"
    )
}
