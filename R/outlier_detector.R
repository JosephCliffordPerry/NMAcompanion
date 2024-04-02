# outlier processing for potentially valuable rare phenotypes
#
# find outliers
# using profile points to find variable outliers with interquartile ranges to find outlier groupings for each dataset
# itterate through profile
# add outlier rows as booleen logic to corresponding column in booleen dataframe
# this can then be added to clustering to search for rare phenotypes
#
# take rows with consistent number of outlier angles
#
# find consitencies within the outlier data
# compare to the overall data find distinct consistencies
#
# cluster on those consistent regions
#
# Use as a new distinct set of cluster identities
#####################################################
get_outlier_features <- function(profile_data) {
  boolean_matrix <- matrix(FALSE, nrow = nrow(profile_data), ncol = ncol(profile_data))
  for (columnumber in 1:ncol(profile_data)) {
    vec <- as.vector(unlist(profile_data[, columnumber]))
    Q1 <- quantile(vec, 0.25)
    Q3 <- quantile(vec, 0.75)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    outliers <- vec < lower_bound | vec > upper_bound
    boolean_matrix[, columnumber] <- outliers
  }
  return(boolean_matrix)
}
#############################################################
make_outlier_cluster <- function(profile_data, profile_type) {
  boolean_matrix <- get_outlier_features(profile_data)
  tboolean <- which(boolean_matrix, arr.ind = TRUE)
  outlier_threshold <- nrow(profile_data) / 20
  booleancount <- table(tboolean[, 1])
  outliercluster <- cbind(data$CellID, 1)
  outlier_rows <- names(head(sort(booleancount, decreasing = TRUE), outlier_threshold))
  outliercluster[as.numeric(outlier_rows), 2] <- 2
  outliercluster <- as.data.frame(outliercluster)
  names(outliercluster)[names(outliercluster) == "V1"] <- paste0(profile_type, " ", "outliers")
  names(outliercluster)[names(outliercluster) == "V2"] <- "Clustering_file"
  return(list(outliercluster))
}



#####################################


