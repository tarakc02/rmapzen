# rmapzen 0.4.2
* added optional `Origin` argument to `mz_vector_tiles`, see issue #17
* minor modifications to code to avoid using deprecated tidyverse functions

# rmapzen 0.4.1
* updated maintainer email address

# rmapzen 0.4.0
* added `mz_geocode_structured` for geocoding structured address data
* added additional ways to specify tile coordinates for vector tiles. Vector tile coordinates can now be specified using: 
    - the x, y, z tile coordinate system, 
    - a lat/lon bounding box with a zoom level, 
    - a bounding box with height/width in pixels, and 
    - a point location plus a zoom level 
* added additional methods for `mz_bbox`

# rmapzen 0.3.5
* Updated `as_sf` and `as_sp` methods for vector tile data, previous versions used functions that are no longer exported from the `sf` package (as of version 0.6-2)

# rmapzen 0.3.4
* Made service host configurable, since Mapzen is no more :(. See `?mz_set_host` for how to set up a specific API provider. Thanks to @dpprdan for the initial update. 

# rmapzen 0.3.3
* Added additional documentation and updated DESCRIPTION

# rmapzen 0.3.2
* Added ability to convert to `sf`
* All objects returned from the Mapzen API now include the header from the response as an attribute called "header"
* conversion functions now error on empty inputs (e.g. a search with 0 results)
* request functions are no longer rate-limited, because Mapzen's service has been updated and only has monthly (not per-second or per-minute) limits

# rmapzen 0.3.1
* Added support for [structured geocoding](https://mapzen.com/documentation/search/structured-geocoding/)
* Simple print methods for vector tiles and individual vector tile layers

# rmapzen 0.3.0.9007
* new function `mz_rect` for directly creating a bounding box
* `mz_vector_tiles` gained a `...` argument to pass height/width/zoom specifications to `as.mz_tile_coordinates`.

# rmapzen 0.3.0.9006

* Included tracking of vector tile service to `mz_check_usage()`
* Added `mz_bbox()` method for vector tiles
* Bugfix on converting vector tiles to Spatial* - re-calculate IDs (to avoid issue where features with the same ID on separate tiles got messed up when stitching tiles) and merge polygons (so polygons spanning multiple tiles don't appear as multiple polygons when tiles are stitched).
* Added example data from each of the three services: `oakland_public` (`mz_search`), `marina_walks` (`mz_isochrone`), `ca_tiles` (`mz_vector_tiles`)

# rmapzen 0.3.0.9005

* Added a `NEWS.md` file to track changes to the package.
* Added support for vector tile service, see `?mz_vector_tiles`


