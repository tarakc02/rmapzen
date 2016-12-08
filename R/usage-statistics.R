usage_recorder <- function() {
    last_updated <- NULL
    remaining_day <- NULL
    remaining_second <- NULL

    update <- function(header) {
        if (!is.null(header$`x-cache`) && header$`x-cache` == "MISS") {
            last_updated <<- header$date
            remaining_day <<- header$`x-apiaxleproxy-qpd-left`
            remaining_second <<- header$`x-apiaxleproxy-qps-left`
        }
    }

    query <- function()
        list(
            last_updated = last_updated,
            remaining_day = remaining_day,
            remaining_second = remaining_second
        )

    function(r)
        switch(
            r,
            "update" = function(hdr) update(hdr),
            "view" = function() query()
        )
}

usage_statistics <- usage_recorder()

#' @export
mz_check_usage <- function() usage_statistics("view")()

mz_update_usage <- function(header) usage_statistics("update")(header)
