rmapzen
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
Introduction
------------

NOTE: [Mapzen has shut down](https://mapzen.com/blog/shutdown/). However, the rmapzen package can be used with [other providers](https://mapzen.com/blog/migration/). The hosting service can be updated via `options`, for example, to update the search host (for `mz_search`, `mz_geocode`, and `mz_structured_search`):

``` r
options(RMAPZEN_SEARCH_HOST = "api.geocode.earth")
```

------------------------------------------------------------------------

`rmapzen` is a client for the Mapzen API. For an introduction, detailed examples, and installation instructions, see: <https://tarakc02.github.io/rmapzen/>. The package is available [on CRAN](https://cran.r-project.org/package=rmapzen), to install:

``` r
install.packages("rmapzen")
```

For more information about the Mapzen API, see <https://mapzen.com/documentation/>.
