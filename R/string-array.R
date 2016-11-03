#' @import assertthat
string_array <- function(strings) {
    assert_that(is.character(strings))
    paste(unique(strings), collapse = ",")
}
