.onLoad <- function(libname, pkgname) {
    rmapzen_default_options <- list(
        RMAPZEN.search.host = "search.mapzen.com"
    )
    op <- options()
    toset <- !(names(rmapzen_default_options) %in% names(op))
    if(any(toset)) options(rmapzen_default_options[toset])

    invisible()
}
