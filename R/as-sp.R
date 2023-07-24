recalculate_ids <- function(features) {
    features$features <- lapply(features$features, function(feature) {
        feature$properties$id <- NULL
        feature$properties$id <- digest::digest(feature$properties)
        return(feature)
    })
    features
}

as_json <- function(geo) {
    geojsonio::as.json(geo)
}
