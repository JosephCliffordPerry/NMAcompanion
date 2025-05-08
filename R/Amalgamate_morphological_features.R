#' Amalgamate Morphological Features from Clustered Data
#'
#' This function processes morphological data by filtering out suspected detection errors
#' based on extreme angle values, calculating clustering confidence using Rand indices,
#' and generating consensus morphological representations based on confidence groupings.
#'
#' @param data A standard NMA full profiles export
#' @param clusters The list of clusters provided by Cluster_ROI_list
#'
#' @return A  list of consensus images representing amalgamated morphological features
#'   based on high, medium, and low clustering confidence groups.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Detects and removes rows with suspected detection errors (angle > 280).
#'   \item Computes Rand indices to assess clustering confidence.
#'   \item Assigns feature identities to high, medium, and low confidence groups.
#'   \item Constructs a consensus representation using Hamming amalgamation.
#' }
#'
#' @seealso \code{\link{make_randindex_data}}, \code{\link{calculate_rand_indexes}},
#'   \code{\link{give_featureidentities}}, \code{\link{ID_creation}},
#'   \code{\link{hamming_amalgamate_Clustering}}
#'
#' @export

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
full_id_list<-list()
full_id_list <-append(x = full_id_list,values = ID_creation(confidence_groups[["high_confidence_grouping"]]))
full_id_list <- append(x = full_id_list,values = ID_creation(confidence_groups[["medium_confidence_grouping"]]))
full_id_list <-append(x = full_id_list,values = ID_creation(confidence_groups[["low_confidence_grouping"]]))

# make consensus images of hamming amalgamated confidence grouping ids
outlinedata <- filtereddata %>% dplyr::select(starts_with("Outline_Oriented"))
hamming_consensus <- hamming_amalgamate_Clustering(data = filtereddata, rand_data = rand_data, ID_list = full_id_list, outlinedata = outlinedata)
return(hamming_consensus)
}


