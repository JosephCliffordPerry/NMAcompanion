#' Detect Regions of Interest from Raw Morphological Data
#'
#' This function identifies and extracts regions of interest (ROIs) from raw morphological data
#' by filtering out rows with extreme angle values (potential detection errors),
#' and selecting features that show statistical evidence of non-unimodality.
#'
#' @param rawdata A standard NMA full profiles export
#'
#' @return A list of data frames or vectors representing selected regions of interest
#'   from angle, diameter, radius, and other relevant numeric data.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Applies \code{Extreme_angle_detector} to remove rows where any angle exceeds 280 degrees.
#'   \item Segments the dataset into angle, diameter, radius, outline, and other numeric features.
#'   \item Applies dip tests for non-unimodality on each data type to identify statistically relevant regions.
#'   \item Returns a combined list of selected ROIs across all relevant data segments.
#' }
#'
#' Internally uses \code{\link{get.dip.test.regions}} and \code{\link{monohartigansdipper}}
#' to assess modality in the data.
#'
#' @seealso \code{\link{get.dip.test.regions}}, \code{\link{monohartigansdipper}}
#'
#' @examples
#' \dontrun{
#' selected_rois <- get_regions_of_interest(an_NMA_export)
#' }
#' @importFrom stringr str_extract_all
#' @importFrom dplyr %>%
#' @importFrom dplyr starts_with
#' @importFrom dplyr select
#' @importFrom dplyr select_if
#' @importFrom dplyr summarize
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom fossil rand.index
#' @export
# accessible region of interest data detection script
get_regions_of_interest <- function(rawdata) {
  # Identify cells with possible errors in edge detection via angle profile.
  raw.angle.profile <- rawdata %>% dplyr::select(starts_with("Angle_profile_"))

  # Mark any cells with a profile angle >280 degrees as potential error.
  # Angles this sharp should not occur normally.
  is_possible_error <- apply(raw.angle.profile, 1, \(nucleus.profile) any(nucleus.profile > 280))

  # Filter to only the cells we are confident do not have errors
  data <- dplyr::filter(rawdata, !is_possible_error)


  # cutting dataset into different portions based on content
  angle_data <- data %>% dplyr::select(starts_with("Angle_profile_"))
  diameter_data <- data %>% dplyr::select(starts_with("Diameter_profile_"))
  radius_data <- data %>% dplyr::select(starts_with("Radius_profile_"))
  outlinedata <- data %>% dplyr::select(starts_with("Outline_Oriented"))

  # Select columns that are numeric and don't contain the specified words ( redundant data and stuff)
  other_data <- data %>%
    select_if(is.numeric) %>%
    select(-matches("Radius_profile_|Diameter_profile_|Angle_profile_|pixels|seg|Seg|Outline_"))

  # checks all portions for bimodality
  selected_angle_data <- get.dip.test.regions(angle_data)
  selected_diameter_data <- get.dip.test.regions(diameter_data)
  selected_radius_data <- get.dip.test.regions(radius_data)
  selected_other_data <- monohartigansdipper(dataset = other_data)

  selected_datasets <- c(selected_angle_data, selected_diameter_data, selected_radius_data, selected_other_data)
  return(selected_datasets)
}
