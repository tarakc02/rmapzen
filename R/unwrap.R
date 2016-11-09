#' @import assertthat
unwrap <- function(obj, objname, components) {
    assert_that(has_components(obj, components))

    lengths <- vapply(obj, length, FUN.VALUE = integer(1))
    if (any(lengths != 1L))
        stop("The following components are invalid for ", objname, "\n",
             paste(names(obj[lengths != 1L]), collapse = ", "), sep = "")

    names(obj) <- paste0(objname, ".", names(obj))
    as.list(obj)
}

has_components <- function(obj, components) {
    setequal(names(obj), components)
}

#' @import assertthat
on_failure(has_components) <- function(call, env) {
    paste0(
        env$objname, " requires the following components:\n",
        paste(env$components, collapse = ", "),
        " (you entered: ", paste(names(env$obj), collapse = ", "), ")"
    )
}
