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
