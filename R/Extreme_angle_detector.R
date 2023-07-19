#'This tags the nuclei with angles over 280 that are likely artifacts
#' @param data the nuclear measurements exported dataset from NMA
#' @importFrom dplyr %>%
#' @importFrom dplyr starts_with

Extreme_angle_detector <- function(data){

  #cutting dataset into different portions based on content
    dataset<- data %>%(dplyr::select(starts_with("Angle_profile_")))

  # Iterate through the dataset row by row
  for (i in 1:nrow(dataset)) {
    # Check if any value in the row is over 280
    if (any(dataset[i, ] > 280)) {
      # Tag the row with 2 in "suspected detection error" column
      data$suspected_detection_error[i] <- 2
    } else {
      # Tag the row with 1 in "suspected detection error" column
      data$suspected_detection_error[i] <- 1
    }
  }
  return(data)}
