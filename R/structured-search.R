#' Structured search
#'
#' \code{\link{mz_search}} allows you to search using an unstructured string of
#' text, but if your address data has more structure (eg separate columns for
#' address, city, state, zip), then using the structured search service may
#' provide more precision. For more information, see
#' \url{https://github.com/pelias/documentation}. Note that
#' all of the arguments are optional, but at least one of them must be non-NULL.
#' Furthermore, \code{postalcode} can not be used by itself.
#'
#' @param address Can be a numbered street address or just the name of the street
#' @param neighbourhood Neighborhood name (eg "Notting Hill" in London)
#' @param borough eg "Manhattan"
#' @param locality The city (eg "Oakland")
#' @param county The county
#' @param region States in the case of US/Canada, or state-like
#' administrative division in other countries
#' @param postalcode AKA the zip code. Can not be used alone, must have at least
#' one other argument
#' @param country The country - Can be the full name or the abbreviation from
#' \code{\link{mz_countries}}
#' @param api_key Your Mapzen API key. The default is to look for the key within
#' the provider information that was set up with `mz_set_host`.
#' @param ... Any of the parameters, other than "text", that appear in
#' \code{\link{mz_search}}, can appear here, for example \code{size},
#' \code{boundary.country}, etc.
#'
#' @seealso \code{\link{mz_search}}
#' @export
mz_structured_search <- function(
    address = NULL,
    neighbourhood = NULL,
    borough = NULL,
    locality = NULL,
    county = NULL,
    region = NULL,
    postalcode = NULL,
    country = NULL, api_key = NULL, ...
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
    country = NULL, api_key = NULL, ...
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
    query <- c(list(api_key = api_key), query, additional_params)
    if (!is.null(postalcode)) query <- c(query, postalcode = postalcode)
    do.call(search_url, c(endpoint = "search/structured", query))
}
