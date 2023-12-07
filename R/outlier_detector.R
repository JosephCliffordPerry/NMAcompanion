#outlier processing for potentially valuable rare phenotypes
#
#find outliers
# using profile points to find variable outliers with gesd to find outlier groupings for each dataset
# itterate through profile
# add outlier rows as booleen logic to corresponding column in booleen dataframe
# take rows with consistent match to
#
# find consitencies within the outlier data
# compare to the overall data find distinct consistencies
#
# cluster on those consistent regions
#
# Use as a new distinct set of cluster identities


get.outlier.features<- function(variables) {

# Create a logical copy of the DataFrame
boolean_dataframe <- as.data.frame(matrix(FALSE, nrow = nrow(angle_data), ncol = ncol(angle_data)))
for (columnumber in 1:length(angle_data)) {


outliers<-gesdTest(angle_data[columnumber],(nrow(angle_data)/20))
pvaluevector<-outliers$p.value
indexvector<-outliers$ix
for (i in 1:length(pvaluevector)) {
if (pvaluevector[i]<0.05) {
boolean_dataframe[indexvector[i],columnumber] <- TRUE
}
which()
}

}

}
