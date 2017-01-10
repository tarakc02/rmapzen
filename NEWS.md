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


