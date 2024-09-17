find_contious_clusters<-function(rawdata){
  error_tagged_angle_dataset <- Extreme_angle_detector(data = rawdata)
  data <- filter(error_tagged_angle_dataset, suspected_detection_error == 1)
  angle_data <- data %>% dplyr::select(starts_with("Angle_profile_"))
  diameter_data <- data %>% dplyr::select(starts_with("Diameter_profile_"))
  radius_data <- data %>% dplyr::select(starts_with("Radius_profile_"))
  angle_outliers <- make_outlier_cluster(angle_data, "angle")
  diameter_outliers <- make_outlier_cluster(diameter_data, "diameter")
  radius_outliers <- make_outlier_cluster(radius_data, "radius")
  output<-list(angle_outliers=angle_outliers,radius_outliers=radius_outliers,diameter_outliers=diameter_outliers)
return(output)
  }
