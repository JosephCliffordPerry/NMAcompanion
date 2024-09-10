Amalgamate_morphological_features<-function(data,clusters){
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

  error_tagged_angle_dataset <- Extreme_angle_detector(data = data)
  filtereddata <- filter(error_tagged_angle_dataset, suspected_detection_error == 1)
# calculate rand index matrix
rand_data <- make_randindex_data(data = filtereddata, clusters = clusters)
rand_matrix <- calculate_rand_indexes(rand_data)
# calculate IDs from rand index confidence groups
confidence_groups <- give_featureidentities(rand_matrix)
ID_list <- ID_creation(confidence_groups[["high_confidence_grouping"]])
ID_list2 <- ID_creation(confidence_groups[["medium_confidence_grouping"]])
ID_list3 <- ID_creation(confidence_groups[["low_confidence_grouping"]])
full_id_list <- c(ID_list, ID_list2, ID_list3)
# make consensus images of hamming amalgamated confidence grouping ids
outlinedata <- data %>% dplyr::select(starts_with("Outline_Oriented"))
hamming_consensus <- hamming_amalgamate_Clustering(data = data, rand_data = rand_data, ID_list = full_id_list, outlinedata = outlinedata)
return(hamming_consensus)
}


