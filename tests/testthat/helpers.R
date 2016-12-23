expect_dict_equal <- function(x1, x2) {
    # test whether two lists are "equal" up to re-ordering
    # like setequal, but also looks at element names
    actual_query <- x1[sort(names(x1))]
    expected_query <- x2[sort(names(x2))]
    expect_equal(actual_query, expected_query)

}
