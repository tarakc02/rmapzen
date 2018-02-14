mz_key <- function(which) {
    getOption(paste0("RMAPZEN_", toupper(which), "_HOST"))$key
}
