% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/isochrone.R
\name{mz_isochrone}
\alias{mz_isochrone}
\title{Retrieve isochrones}
\usage{
mz_isochrone(
  locations,
  costing_model,
  contours,
  date_time = NULL,
  polygons = NULL,
  denoise = NULL,
  generalize = NULL,
  id = "my-iso",
  api_key = NULL
)
}
\arguments{
\item{locations}{An \code{mz_location}, or something that can be coerced to an
\code{\link{mz_location}}, as the departure point for the isochrone. This can be the
result of \code{\link{mz_geocode}}. Despite the argument name, the isochrone
service currently can only accept a single location}

\item{costing_model}{The costing model, see \code{\link{mz_costing}}}

\item{contours}{Up to 4 contours, see \code{\link{mz_contours}}}

\item{date_time}{The local date and time at the location, and whether it is
the departure or arrival time. See \code{\link{mz_date_time}}}

\item{polygons}{Whether to return polygons (TRUE) or linestrings (FALSE, default)}

\item{denoise}{A value between 0 and 1 (default 1) to remove smaller contours.
A value of 1 will only return the largest contour for a given time value. A
value of 0.5 drops any contours that are less than half the area of the
largest contour.}

\item{generalize}{Tolerance in meters for the Douglas-Peucker generalization.}

\item{id}{A descriptive identifier, the response will contain the id as an element.}

\item{api_key}{Your Mapzen API key. The default is to look for the key within
the provider information that was set up with `mz_set_host`.}
}
\value{
A \code{mapzen_isochrone_list}, which can be converted to \code{sf}
using \code{\link{as_sf}}.
}
\description{
From \url{https://valhalla.readthedocs.io/en/latest/}:
"An isochrone is a line that connects points of equal travel time about a
given location, from the Greek roots of 'iso' for equal and 'chrone' for time.
The Mapzen Isochrone service computes areas that are reachable within
specified time intervals from a location, and returns the reachable regions
as contours of polygons or lines that you can display on a map."
}
\examples{
\dontrun{
mz_isochrone(
    mz_location(lat = 37.87416, lon = -122.2544),
    costing_model = mz_costing$auto(),
    contours = mz_contours(c(10, 20, 30))
)

# departure point can be specified as a geocode result
mz_isochrone(
    mz_geocode("UC Berkeley"),
    costing_model = mz_costing$pedestrian(),
    contours = mz_contours(c(10, 20, 30))
)
}

}
\seealso{
\code{\link{mz_costing}}
}
