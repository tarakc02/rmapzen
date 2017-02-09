mz_structured_search <- function(
    address = NULL,
    neighbourhood = NULL,
    borough = NULL,
    locality = NULL,
    county = NULL,
    region = NULL,
    postalcode = NULL,
    country = NULL, api_key = mz_key(), ...
)
{
    url <- build_structured_search_url(
        address = address,
        neighbourhood = neighbourhood,
        borough = borough,
        locality = locality,
        county = county,
        region = region,
        postalcode = postalcode,
        country = country,
        api_key = api_key,
        ...
    )

    search_get(url)
}


#' @import assertthat
build_structured_search_url <- function(
    address = NULL,
    neighbourhood = NULL,
    borough = NULL,
    locality = NULL,
    county = NULL,
    region = NULL,
    postalcode = NULL,
    country = NULL, api_key = mz_key(), ...
) {
    assert_that(
        is.null(address) || is.string(address),
        is.null(neighbourhood) || is.string(neighbourhood),
        is.null(borough) || is.string(borough),
        is.null(locality) || is.string(locality),
        is.null(county) || is.string(county),
        is.null(region) || is.string(region),
        is.null(address) || is.string(address)
    )

    query <- c(
        address = address,
        neighbourhood = neighbourhood,
        borough = borough,
        locality = locality,
        county = county,
        region = region,
        country = country
    )

    if (length(query) <= 0L)
        stop("Structured search requires at least one of the following:\n",
             "(can additionally include postalcode)\n",
             "  address\n",
             "  neighbourhood\n",
             "  borough\n",
             "  locality\n",
             "  county\n",
             "  region\n",
             "  country")

    additional_params <- search_query_parameters(...)
    query <- c(api_key = api_key, query, additional_params)
    if (!is.null(postalcode)) query <- c(query, postalcode = postalcode)
    do.call(search_url, c(endpoint = "search/structured", query))
}
