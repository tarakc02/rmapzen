rmapzen
================

-   [Introduction](#introduction)
-   [Search](#search)
-   [Tidy-friendly](#tidy-friendly)
-   [Accessors](#accessors)
-   [Rate limits](#rate-limits)
-   [Additional convenience features](#additional-convenience-features)
-   [Other services and related projects](#other-services-and-related-projects)
-   [Installation](#installation)

<!-- README.md is generated from README.Rmd. Please edit that file -->
Introduction
------------

rmapzen is a client for the Mapzen API. For more information, see <https://mapzen.com/documentation/>.

Search
------

So far, all of the services in [Mapzen search](https://mapzen.com/documentation/search/) have been implemented. Search functions:

-   `mz_search`
-   `mz_reverse_geocode`
-   `mz_autocomplete`
-   `mz_place`

For example, to search for Hard Rock Cafes in Sweden (see [Additional convenience features](#additional-convenience-features) below for more ways to get the correct ISO-3166 country code):

``` r
library(rmapzen)

hard_rock <- mz_search("Hard Rock Cafe", boundary.country = "SE")
hard_rock
#> GeoJSON response from Mapzen
#> Attribution info: https://search.mapzen.com/v1/attribution 
#> Bounds (lon/lat): (11.97, 55.78) - (18.08, 62.39)
#> 10 locations:
#>    Hard Rock Café (18.05, 59.34)
#>    Hard Rock Cafe (11.97, 57.7)
#>    Rock'n Roll Café (13.83, 55.93)
#>    Carolas Cafe (13.1, 55.78)
#>    Cafe Charm (17.3, 62.39)
#>   ...
```

All of the search functions return a `geo_list` object. These are R list representations of valid geojson (which is also valid json), and can be converted to `SpatialPointsDataFrame` objects via the package [geojsonio](https://github.com/ropensci/geojsonio) -- for example:

``` r
hard_rock_sp <- geojsonio::geojson_sp(hard_rock)
str(hard_rock_sp)
#> Formal class 'SpatialPointsDataFrame' [package "sp"] with 5 slots
#>   ..@ data       :'data.frame':  10 obs. of  18 variables:
#>   .. ..$ id               : Factor w/ 10 levels "node:1233020660",..: 7 3 6 8 1 2 10 4 5 9
#>   .. ..$ gid              : Factor w/ 10 levels "openstreetmap:venue:node:1233020660",..: 7 3 6 8 1 2 10 4 5 9
#>   .. ..$ layer            : Factor w/ 1 level "venue": 1 1 1 1 1 1 1 1 1 1
#>   .. ..$ source           : Factor w/ 1 level "openstreetmap": 1 1 1 1 1 1 1 1 1 1
#>   .. ..$ source_id        : Factor w/ 10 levels "node:1233020660",..: 7 3 6 8 1 2 10 4 5 9
#>   .. ..$ name             : Factor w/ 10 levels "Cafe capri","Cafe Charm",..: 9 8 10 7 2 1 6 4 3 5
#>   .. ..$ confidence       : num [1:10] 0.902 0.962 0.662 0.661 0.661 0.661 0.661 0.661 0.661 0.661
#>   .. ..$ accuracy         : Factor w/ 1 level "point": 1 1 1 1 1 1 1 1 1 1
#>   .. ..$ country          : Factor w/ 1 level "Sweden": 1 1 1 1 1 1 1 1 1 1
#>   .. ..$ country_gid      : Factor w/ 1 level "whosonfirst:country:85633789": 1 1 1 1 1 1 1 1 1 1
#>   .. ..$ country_a        : Factor w/ 1 level "SWE": 1 1 1 1 1 1 1 1 1 1
#>   .. ..$ region           : Factor w/ 6 levels "Jönköping","Östergötland",..: 4 6 3 3 5 4 1 4 2 4
#>   .. ..$ region_gid       : Factor w/ 6 levels "whosonfirst:region:85688377",..: 5 4 1 1 6 5 2 5 3 5
#>   .. ..$ locality         : Factor w/ 6 levels "Furulund","Göteborg",..: 4 2 NA 1 5 6 NA 4 3 4
#>   .. ..$ locality_gid     : Factor w/ 6 levels "whosonfirst:locality:101752295",..: 2 1 NA 6 3 4 NA 2 5 2
#>   .. ..$ neighbourhood    : Factor w/ 5 levels "Fruänge","Hjorthagen",..: 5 4 NA NA NA 3 NA 2 NA 1
#>   .. ..$ neighbourhood_gid: Factor w/ 5 levels "whosonfirst:neighbourhood:85902171",..: 5 4 NA NA NA 2 NA 1 NA 3
#>   .. ..$ label            : Factor w/ 10 levels "Cafe capri, Upplands Väsby, Sweden",..: 9 8 10 7 2 1 6 4 3 5
#>   ..@ coords.nrs : num(0) 
#>   ..@ coords     : num [1:10, 1:2] 18.1 12 13.8 13.1 17.3 ...
#>   .. ..- attr(*, "dimnames")=List of 2
#>   .. .. ..$ : NULL
#>   .. .. ..$ : chr [1:2] "coords.x1" "coords.x2"
#>   ..@ bbox       : num [1:2, 1:2] 12 55.8 18.1 62.4
#>   .. ..- attr(*, "dimnames")=List of 2
#>   .. .. ..$ : chr [1:2] "coords.x1" "coords.x2"
#>   .. .. ..$ : chr [1:2] "min" "max"
#>   ..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slot
#>   .. .. ..@ projargs: chr "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
```

Additionally, `mz_geocode` is a convenient function to geocode an address, utilizing the more general `mz_search` function.

``` r
mz_geocode("UC Berkeley, Berkeley, CA")
#> # A tibble: 1 × 4
#>                  geocode_address geocode_longitude geocode_latitude
#> *                          <chr>             <dbl>            <dbl>
#> 1 UC Berkeley, Berkeley, CA, USA         -122.2542         37.87238
#> # ... with 1 more variables: geocode_confidence <dbl>
```

Tidy-friendly
-------------

`as.data.frame` makes it easy to include search responses in tidy pipelines:

``` r
library(dplyr)
as.data.frame(hard_rock) %>%
    select(name, confidence, country, region, locality, neighbourhood)
#> # A tibble: 10 × 6
#>                  name confidence country          region       locality
#>                 <chr>      <dbl>   <chr>           <chr>          <chr>
#> 1      Hard Rock Café      0.902  Sweden       Stockholm      Stockholm
#> 2      Hard Rock Cafe      0.962  Sweden Västra Götaland       Göteborg
#> 3    Rock'n Roll Café      0.662  Sweden           Skåne           <NA>
#> 4        Carolas Cafe      0.661  Sweden           Skåne       Furulund
#> 5          Cafe Charm      0.661  Sweden  Västernorrland      Sundsvall
#> 6          Cafe capri      0.661  Sweden       Stockholm Upplands Väsby
#> 7           Cafeteria      0.661  Sweden       Jönköping           <NA>
#> 8  CaféHarpaviljongen      0.661  Sweden       Stockholm      Stockholm
#> 9       Cafe Columbia      0.661  Sweden    Östergötland           Kisa
#> 10         Cafékoppen      0.661  Sweden       Stockholm      Stockholm
#> # ... with 1 more variables: neighbourhood <chr>
```

Accessors
---------

Currently, the following accessors are available to pull out commonly used pieces from a search response:

-   `mz_coordinates`
-   `mz_bbox`

``` r
mz_coordinates(hard_rock)
#> # A tibble: 10 × 2
#>         lon      lat
#>       <dbl>    <dbl>
#> 1  18.05484 59.34408
#> 2  11.97425 57.70123
#> 3  13.83493 55.93419
#> 4  13.09754 55.77635
#> 5  17.30462 62.38921
#> 6  17.94834 59.39728
#> 7  14.13232 57.58279
#> 8  18.08074 59.34991
#> 9  15.63464 57.98744
#> 10 17.96348 59.28589
```

Rate limits
-----------

Mapzen's published rate limits (6 requests per second) are automatically observed, using the [ratelimitr package](https://github.com/tarakc02/ratelimitr). You can check usage statistics at any time using the function `mz_check_usage`.

Additional convenience features
-------------------------------

Several of the search functions take, optionally, the arguments `layers`, `sources`, and `boundary.country` (the latter requires [ISO-3166](https://en.wikipedia.org/wiki/ISO_3166) codes). If you're using an IDE with auto-complete, the objects `mz_layers`, `mz_sources`, and `mz_countries` should make it easier to get the correct codes.

![Easy lookup for ISO-3166 codes](fig/mz-countries.png)

Other services and related projects
-----------------------------------

The package does not yet support [other Mapzen API services](https://mapzen.com/documentation/). But do check out these related R packages:

-   [elevatr](https://github.com/jhollist/elevatr) for accessing elevation data, including Mapzen Terrain and Elevation
-   [postr](https://github.com/Ironholds/poster) for address parsing and normalization using the [libpostal](https://github.com/openvenues/libpostal) library

Installation
------------

This is a very young package, things may break or change. To install:

    # rate-limits are enforced using the ratelimitr package
    devtools::install_github("tarakc02/ratelimitr")
    devtools::install_github("tarakc02/rmapzen")

You'll also need to [acquire an API key](https://mapzen.com/developers), and then set the `MAPZEN_KEY` environment variable:

    Sys.setenv(MAPZEN_KEY = "mapzen-xxxxxx")
