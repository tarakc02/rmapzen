costing_model <- function(type, ...) {
    costing <- jsonlite::unbox(type)
    costing_options <- list(...)
    costing_options <- Filter(function(df) nrow(df) > 0L, costing_options)

    if (length(costing_options) == 0L)
        return(list(costing = costing))

    costing_options <- lapply(
        costing_options,
        jsonlite::unbox
    )

    structure(
        list(
            costing = costing,
            costing_options = costing_options),
        class = "mz_costing_model"
    )
}

#' Costing model constructors and helpers
#'
#' Mapzen's Isochrone service (\code{\link{mz_isochrone}}) as well as other
#' mobility services (currently not implemented in this package, read more at
#' \url{https://mapzen.com/documentation/mobility/}) require users to specify a
#' "costing model." See \url{https://mapzen.com/documentation/mobility/turn-by-turn/api-reference/#costing-models}
#' for details. These can be difficult to construct correctly, so the objects
#' \code{mz_costing} and \code{mz_costing_options} exist to make that process
#' less error-prone and more convenient.
#'
#' @examples
#' ## creates a pedestrian costing model with walking speed of 2 km/hr
#' ## that also avoids alleys.
#' ## non-multimodal costing models will accept 0 or more options from the
#' ## appropriate list.
#' mz_costing$pedestrian(
#'     mz_costing_options$pedestrian$walking_speed(2.0),
#'     mz_costing_options$pedestrian$alley_factor(0)
#' )
#'
#' ## creates a multimodal costing model that favors buses over rails, and
#' ## has a slower than default walking speed
#' ## (note multimodal has named arguments requiring list inputs)
#' mz_costing$multimodal(
#'     transit = list(
#'         mz_costing_options$transit$use_bus(1.0),
#'         mz_costing_options$transit$use_rail(5)
#'     ),
#'     pedestrian = list(
#'         mz_costing_options$pedestrian$walking_speed(4.1)
#'     )
#' )
#' @seealso \code{\link{mz_isochrone}}
#' @name costing_models
#' @export
mz_costing <- list(
    pedestrian = function(...) {
        costing_model("pedestrian", pedestrian = data.frame(...))},
    auto = function(...) {
        costing_model("auto", auto = data.frame(...))},
    bicycle = function(...) {
        costing_model("bicycle", bicycle = data.frame(...))},
    multimodal = function(transit = NULL, pedestrian = NULL) {
        transit <- data.frame(transit)
        pedestrian <- data.frame(pedestrian)
        costing_model("multimodal", transit = transit, pedestrian = pedestrian)
    }

)

costopt <- function(x, validate = assertthat::is.number) {
    assertthat::assert_that(validate(x))
    jsonlite::unbox(x)
}

#' @rdname costing_models
#' @export
mz_costing_options <- list(
    pedestrian = list(
        walking_speed = function(speed) list(walking_speed = costopt(speed)),
        walkway_factor = function(factor) list(walkway_factor = costopt(factor)),
        alley_factor = function(factor) list(alley_factor = costopt(factor)),
        driveway_factor = function(factor) list(driveway_factor = costopt(factor)),
        step_penalty = function(seconds) list(step_penalty = costopt(seconds))
    ),

    auto = list(
        maneuver_penalty = function(penalty) list(maneuver_penalty = costopt(penalty)),
        gate_cost = function(cost) list(gate_cost = costopt(cost)),
        toll_booth_cost = function(cost) list(toll_booth_cost = costopt(cost)),
        toll_booth_penalty = function(penalty) list(toll_booth_penalty = costopt(penalty)),
        ferry_cost = function(cost) list(ferry_cost = costopt(cost)),
        use_ferry = function(value) list(use_ferry = costopt(value)),
        country_crossing_cost = function(cost) list(country_crossing_cost = costopt(cost)),
        country_crossing_penalty = function(penalty) list(country_crossing_penalty = costopt(penalty))
    ),

    bicycle = list(
        maneuver_penalty = function(penalty) list(maneuver_penalty = costopt(penalty)),
        gate_cost = function(cost) list(gate_cost = costopt(cost)),
        country_crossing_cost = function(cost) list(country_crossing_cost = costopt(cost)),
        country_crossing_penalty = function(penalty) list(country_crossing_penalty = costopt(penalty)),
        bicycle_type = function(type) list(bicycle_type = costopt(bicycle_type, assertthat::is.string)),
        cycling_speed = function(speed) list(cycling_speed = costopt(speed)),
        use_roads = function(propensity) list(use_roads = costopt(propensity)),
        use_hills = function(propensity) list(use_hills = costopt(propensity))
    ),

    transit = list(
        use_bus = function(value) list(use_bus = costopt(value)),
        use_rail = function(value) list(use_rail = costopt(value)),
        use_transfers = function(value) list(use_transfers = costopt(value)),
        transit_start_end_max_distance = function(distance) list(transit_start_end_max_distance = costopt(distance)),
        transit_transfer_max_distance = function(distance) list(transit_transfer_max_distance = costopt(distance))
    )
)
