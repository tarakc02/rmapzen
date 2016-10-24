mz_key <- function() {
    key <- Sys.getenv("MAPZEN_KEY")
    if (identical(key, ""))
        stop("Set the MAPZEN_KEY environment variable")
    key
}
