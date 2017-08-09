collapse_properties <- function(geo) {

    collapser <- function(feature) {
        feature$properties <- lapply(
            feature$properties,
            function(x) paste0(x, collapse = ";")
        )
        feature
    }

    geo$features <- lapply(geo$features, collapser)
    geo
}
