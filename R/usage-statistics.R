usage_recorder <- function() {
    search_usage <- list(
        last_updated = NULL,
        remaining_day = NULL,
        remaining_second = NULL
    )

    matrix_usage <- list(
        last_updated = NULL,
        remaining_day = NULL,
        remaining_second = NULL
    )

    tile_usage <- list(
        last_updated = NULL,
        remaining_day = NULL,
        remaining_second = NULL
    )

    update_search <- function(header) {
        if (!is.null(header$`x-cache`) && header$`x-cache` == "MISS") {
            search_usage$last_updated <<- header$date
            search_usage$remaining_day <<- header$`x-apiaxleproxy-qpd-left`
            search_usage$remaining_second <<- header$`x-apiaxleproxy-qps-left`
        }
    }

    update_matrix <- function(header) {
        matrix_usage$last_updated <<- header$date
        matrix_usage$remaining_day <<- header$`x-apiaxleproxy-qpd-left`
        matrix_usage$remaining_second <<- header$`x-apiaxleproxy-qps-left`
    }

    update_tile <- function(header) {
        if (!is.null(header$`x-apiaxleproxy-qpd-left`) &&
            !is.null(header$`x-apiaxleproxy-qps-left`)) {
            tile_usage$last_updated <<- header$date
            tile_usage$remaining_day <<- header$`x-apiaxleproxy-qpd-left`
            tile_usage$remaining_second <<- header$`x-apiaxleproxy-qps-left`
        }
    }

    query <- function() {
        structure(
            list(search = search_usage,
                 matrix = matrix_usage,
                 tile = tile_usage),
            class = c("mz_usage_statistics", "list")
        )
    }

    function(r)
        switch(
            r,
            "update_search" = function(hdr) update_search(hdr),
            "update_matrix" = function(hdr) update_matrix(hdr),
            "update_tile" = function(hdr) update_tile(hdr),
            "view" = function() query()
        )
}

#' @export
print.mz_usage_statistics <- function(x, ...) {
    search_usage <- x$search
    matrix_usage <- x$matrix
    tile_usage <- x$tile
    if (!is.null(search_usage$last_updated)) {
        cat("for the search endpoint:\n",
            "  updated on: ", search_usage$last_updated, "\n",
            "  remaining queries today: ", search_usage$remaining_day, "\n",
            "  remaining queries this second: ", search_usage$remaining_second, "\n",
            sep = "")
    } else cat("No data for the search endpoint\n")

    if (!is.null(matrix_usage$last_updated)) {
        cat("for the matrix endpoint:\n",
            "  updated on: ", matrix_usage$last_updated, "\n",
            "  remaining queries today: ", matrix_usage$remaining_day, "\n",
            "  remaining queries this second: ", matrix_usage$remaining_second, "\n",
            sep = "")
    } else cat("No data for the matrix endpoint\n")

    if (!is.null(tile_usage$last_updated)) {
        cat("for the tile (vector tiles) endpoint:\n",
            "  updated on: ", tile_usage$last_updated, "\n",
            "  remaining queries today: ", tile_usage$remaining_day, "\n",
            "  remaining queries this second: ", tile_usage$remaining_second, "\n",
            sep = "")
    } else cat("No data for the tile (vector tiles) endpoint\n")

}

usage_statistics <- usage_recorder()

#' Check usage statistics
#'
#' Prints out remaining queries for various time periods. \code{rmapzen} manages
#' rate limiting for the per-second limits, but does not keep track of the daily
#' limits.
#'
#' This function is populated from the headers of responses to various API requests.
#' If no queries have been made, or if the only queries so far have hit cache servers,
#' then no information will be available.
#'
#' @export
mz_check_usage <- function() usage_statistics("view")()

mz_update_usage <- function(header, type) {
    switch(
        type,
        search = usage_statistics("update_search")(header),
        matrix = usage_statistics("update_matrix")(header),
        tile = usage_statistics("update_tile")(header),
        stop("Unable to update usage statistics for ", type)
    )
}
