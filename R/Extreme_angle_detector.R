# simple bad edge detection detector
Extreme_angle_detector <- function(dataset){
  # Iterate through the dataset row by row
  for (i in 1:nrow(dataset)) {
    # Check if any value in the row is over 280
    if (any(dataset[i, ] > 280)) {
      # Tag the row with 2 in "suspected detection error" column
      dataset$suspected_detection_error[i] <- 2
    } else {
      # Tag the row with 1 in "suspected detection error" column
      dataset$suspected_detection_error[i] <- 1
    }
  }
  return(dataset)}
