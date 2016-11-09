usage_statistics <- function() {
    last_updated <- NULL
    remaining_day <- NULL
    remaining_second <- NULL

    update <- function(header) {
        if (header$`x-cache` == "MISS") {
            last_updated <<- header$date
            remaining_day <<- header$`x-apiaxleproxy-qpd-left`
            remaining_second <<- header$`x-apiaxleproxy-qps-left`
        }
    }
}
