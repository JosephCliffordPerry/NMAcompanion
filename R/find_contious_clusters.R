#' Identify Continuous Outlier Clusters in Morphological Profiles
#'
#' This function identifies clusters of outlier values in morphological profile data (angle, diameter, radius)
#' after filtering out rows with potential detection errors.
#'
#' @param rawdata A standard NMA full profiles export
#'
#' @return A named list containing three elements:
#' \describe{
#'   \item{angle_outliers}{Outlier clusters detected in the angle profile data.}
#'   \item{diameter_outliers}{Outlier clusters detected in the diameter profile data.}
#'   \item{radius_outliers}{Outlier clusters detected in the radius profile data.}
#' }
#'
#' @details
#' The function follows these steps:
#' \enumerate{
#'   \item Applies \code{Extreme_angle_detector} to remove rows where angle values exceed 280 degrees.
#'   \item Separates the filtered data into angle, diameter, and radius profiles.
#'   \item Applies \code{make_outlier_cluster} to each profile type to identify continuous outlier clusters.
#' }
#'
#' @seealso \code{\link{Extreme_angle_detector}}, \code{\link{make_outlier_cluster}}
#'
#' @examples
#' \dontrun{
#' clusters <- find_contious_clusters(raw_morphology_data)
#' }
#'
#' @export
find_contious_clusters<-function(rawdata){
  error_tagged_angle_dataset <- Extreme_angle_detector(data = rawdata)
  filtereddata <- filter(error_tagged_angle_dataset, suspected_detection_error == 1)
  angle_data <- filtereddata  %>% dplyr::select(starts_with("Angle_profile_"))
  diameter_data <- filtereddata  %>% dplyr::select(starts_with("Diameter_profile_"))
  radius_data <- filtereddata  %>% dplyr::select(starts_with("Radius_profile_"))
  angle_outliers <- make_outlier_cluster(angle_data, "angle",filtereddata)
  diameter_outliers <- make_outlier_cluster(diameter_data, "diameter",filtereddata)
  radius_outliers <- make_outlier_cluster(radius_data, "radius",filtereddata)
  output<-list(angle_outliers=angle_outliers,radius_outliers=radius_outliers,diameter_outliers=diameter_outliers)
return(output)
  }
