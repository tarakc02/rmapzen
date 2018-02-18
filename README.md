rmapzen
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
Introduction
------------

NOTE: [Mapzen has shut down](https://mapzen.com/blog/shutdown/). However, the rmapzen package can be used with [other PELIAS providers](https://mapzen.com/blog/migration/). The specific hosting service can be set up with `mz_set_host`. For known providers, `rmapzen` includes pre-configured API set up functions:

-   `mz_set_search_host_geocode.earth` to set up [geocode.earth](https://geocode.earth/) as a search (includes geocoding, autocomplete, etc) provider
-   `mz_set_search_host_nyc_geosearch()` to set up [GeoSearch from NYC Planning Labs](https://geosearch.planninglabs.nyc/) (currently in beta) as a search provider.
-   `mz_set_tile_host_nextzen` to set up [Nextzen](https://www.nextzen.org/) as a vector tile provider

If you know of an API that uses PELIAS that is not included here, [let me know](https://github.com/tarakc02/rmapzen/issues/new).

------------------------------------------------------------------------

`rmapzen` is a client for the Mapzen API. For an introduction, detailed examples, and installation instructions, see: <https://tarakc02.github.io/rmapzen/>. The package is available [on CRAN](https://cran.r-project.org/package=rmapzen), to install:

``` r
install.packages("rmapzen")
```

For more information about the Mapzen API, see <https://mapzen.com/documentation/>.
