#' @export
print.mapzen_vector_tiles <- function(x, ...) {
    cat("Mapzen vector tile data\n")
    cat("Layers: (count of features in parentheses)\n")
    smry <- vapply(x, function(layer) length(layer$features), integer(1))
    layers <- paste0("    ", names(smry), " (", unname(smry), ")", collapse = "\n")
    cat(layers)
    invisible(x)
}

#' @export
print.mapzen_vector_layer <- function(x, ...) {
    cat("Mapzen vector tile layer\n")
    if (length(x$features) <= 0) {
        cat("No data")
        return(invisible(x))
    }
    cat("Geometry type(s): (counts in parentheses)\n")

    json_to_geom <- c(
        LineString = "line",
        MultiLineString = "line",
        Polygon = "polygon",
        MultiPolygon = "polygon",
        Point = "point"
    )

    feature_types <- vapply(x$features, function(feat) feat$geometry$type,
                            character(1))
    feature_types <- json_to_geom[feature_types]
    feature_summary <- table(feature_types)
    features <- names(feature_summary)
    counts <- as.numeric(feature_summary)
    fun <- function(f, c) {
        cat("    ", f, " (", c, ")\n", sep = "")
    }
    Map(fun, features, counts)
    invisible(x)
}
