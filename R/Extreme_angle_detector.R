#' Detect cells with outlying angle profiles
#'
#' Detects cells with angle profiles containing angles greater than a given value.
#' Use to tag nuclei with angles over a critical value that are likely artefacts.
#' @param data a nuclear measurements dataset exported from NMA
#' @param angle the angle in degrees. Defaults to 280.
#' @return a vector containing T for cells with angle > value, F otherwise
#' @export
#' @examples
#' cell.vector <- detectCellsWithAngleGreaterThan(NMA_toy_dataset, 280)
detectCellsWithAngleGreaterThan <- function(data, angle=280) {
  data %>% dplyr::select(dplyr::starts_with("Angle_profile_")) %>%
    apply(., 1,  function(r) any(r>angle))
}
