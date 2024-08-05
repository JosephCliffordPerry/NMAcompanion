#accessible region of interest data detection script
get_regions_of_interest<-function(rawdata){
Extreme_angle_detector <- function(data) {
    # cutting dataset into different portions based on content
    dataset <- data %>% dplyr::select(starts_with("Angle_profile_"))

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
    return(data)
  }

  error_tagged_angle_dataset <- Extreme_angle_detector(data = rawdata)
  data <- filter(error_tagged_angle_dataset, suspected_detection_error == 1)
  # cutting dataset into different portions based on content
  angle_data <- data %>% dplyr::select(starts_with("Angle_profile_"))
  diameter_data <- data %>% dplyr::select(starts_with("Diameter_profile_"))
  radius_data <- data %>% dplyr::select(starts_with("Radius_profile_"))
  outlinedata <- data %>% dplyr::select(starts_with("Outline_Oriented"))

  # Select columns that are numeric and don't contain the specified words ( redundant data and stuff)
  other_data <- data %>%
    select_if(is.numeric) %>%
    select(-matches("Radius_profile_|Diameter_profile_|Angle_profile_|pixels|seg|Seg|suspected_detection_error|Outline_"))




  # checks all portions for bimodality
  selected_angle_data <- get.dip.test.regions(angle_data)
  selected_diameter_data <- get.dip.test.regions(diameter_data)
  selected_radius_data <- get.dip.test.regions(radius_data)
  selected_other_data <- monohartigansdipper(dataset = other_data)

selected_datasets <- c(selected_angle_data, selected_diameter_data, selected_radius_data, selected_other_data)
return(selected_datasets)
}


#a<-get_regions_of_interest(rawdata)
#b<-get_regions_of_interest(rawdata)
