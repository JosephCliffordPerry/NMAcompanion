
#outlier processing for potentially valuable rare phenotypes
#
#find outliers
# using profile points to find variable outliers with interquartile ranges to find outlier groupings for each dataset
# itterate through profile
# add outlier rows as booleen logic to corresponding column in booleen dataframe
#
#take rows with consistent number of outlier angles
#
# find consitencies within the outlier data
# compare to the overall data find distinct consistencies
#
# cluster on those consistent regions
#
# Use as a new distinct set of cluster identities


get.outlier.features<- function(variables) {

# Create a logical copy of the DataFrame
boolean_matrix <- matrix(FALSE, nrow = nrow(angle_data), ncol = ncol(angle_data))
for (columnumber in 1:length(angle_data)) {
  vec<-as.vector(unlist(angle_data[columnumber]))
  # Calculate quartiles and IQR
  Q1 <- quantile(vec, 0.25)
  Q3 <- quantile(vec, 0.75)
  IQR <- Q3 - Q1

  # Define lower and upper bounds for outliers
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR

  # Identify outliers
  outliers <- vec < lower_bound | vec > upper_bound
  boolean_matrix[,columnumber]<-outliers


}

}


tboolean<-which(boolean_matrix, arr.ind = TRUE)
outlier_threshold<-nrow(data)/20
booleancount<-table(tboolean[,1])
outliercluster<-cbind(data$CellID,1)
outlier_rows <- names(head(sort(booleancount, decreasing = TRUE), outlier_threshold))
outliercluster[as.numeric(outlier_rows),2]<-2

angle_outliers<- angle_data[as.numeric(outlier_rows),]
variance_vector<-c()
for (i in 1:length(angle_outliers)) {
variance_vector[i]<-var(angle_outliers[i])
}
