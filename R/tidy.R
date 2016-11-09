tidy <- broom::tidy
tidy.mapzen_geo_list <- function(x, ...) {
    coords <- mz_coordinates(x)
    rest <- dplyr::bind_rows(
        lapply(x$features,
               function(feature) tibble::as_data_frame(feature$properties))
    )
    dplyr::bind_cols(rest, coords)

}
