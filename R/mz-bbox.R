#' Get the bounding box
#'
#' Returns the bottom left and top right corners of the box that contains a
#' mapzen object (mz_geo_list or mz_isochrone_list). The returned value can be
#' used directly as the \code{boundary.rect} parameter for
#' \code{\link{search}} functions.
#'
#' @param geo A mapzen geo list or isochrone list
#'
#' @return A tibble
#' @name mz_bbox
#' @export
mz_bbox <- function(geo) UseMethod("mz_bbox")

#' @rdname mz_bbox
#' @export
mz_bbox.mapzen_geo_list <- function(geo) {
    bbox <- geo$bbox
    if (is.null(bbox) || length(bbox) != 4L) {
        warning("Unable to read bounding box, returning NA")
        return(
            tibble::data_frame(
                min_lon = NA_real_,
                min_lat = NA_real_,
                max_lon = NA_real_,
                max_lat = NA_real_
            )
        )
    }

    tibble::data_frame(
        min_lon = as.numeric(bbox[[1]]),
        min_lat = as.numeric(bbox[[2]]),
        max_lon = as.numeric(bbox[[3]]),
        max_lat = as.numeric(bbox[[4]])
    )
}

#' @rdname mz_bbox
#' @export
mz_bbox.mapzen_isochrone_list <- function(geo) {
    default <- function() {
        warning("Unable to read bounding box, returning NA")
        return(tibble::data_frame(
            min_lon = NA_real_,
            min_lat = NA_real_,
            max_lon = NA_real_,
            max_lat = NA_real_
        ))
    }
    features <- geo$features
    if (is.null(features) || length(features) <= 0L) return(default())

    bbox <- sp::bbox(as_sp(geo))
    if (dim(bbox) != c(2L, 2L) | rownames(bbox) != c("x", "y")) return(default())
    tibble::data_frame(
        min_lon = bbox["x", "min"],
        min_lat = bbox["y", "min"],
        max_lon = bbox["x", "max"],
        max_lat = bbox["y", "max"]
    )
}
