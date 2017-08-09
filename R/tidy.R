#' @export
as.data.frame.mapzen_geo_list <- function(x, ...) {
    if (length(x$features) == 0L)
        stop("Cannot convert to data.frame: no data")
    coords <- mz_coordinates(x)
    rest <- dplyr::bind_rows(
        lapply(x$features,
               function(feature) tibble::as_data_frame(feature$properties))
    )
    dplyr::bind_cols(rest, coords)
}

#' @export
as.data.frame.mapzen_isochrone_list <- function(x, ...) {
    if (length(x$features) == 0L)
        stop("Cannot convert to data.frame: empty layer")
    coords <- function(feature)
        purrr::map2_df(
            feature$geometry$coordinates,
            seq_along(feature$geometry$coordinates),
            ~tibble::data_frame(lon = .x[[1]], lat = .x[[2]],
                                order = .y)
        )

    res <- tibble::data_frame(
        contours = purrr::map(
            x$features,
            ~tibble::as_data_frame(
                .$properties
            )
        ),
        coordinates = purrr::map(
            x$features,
            coords
        )
    )
    res <- tidyr::unnest_(res, "contours")
    tidyr::unnest(res)
}
