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




make_outlier_data <- function(profile_data, profile_type) {
  boolean_matrix<-get_outlier_features(profile_data)
  tboolean <- which(boolean_matrix, arr.ind = TRUE)
  outlier_threshold <- nrow(data) / 20
  booleancount <- table(tboolean[, 1])
  outlier_rows <- names(head(sort(booleancount, decreasing = TRUE), outlier_threshold))
  profile_outliers <- profile_data[as.numeric(outlier_rows), ]
  outlier_sd_vector <- c()
  for (i in 1:length(profile_outliers)) {
   outlier_sd_vector[i] <- sd(profile_outliers[[i]])
  }
  profile_sd_vector <- c()
  for (q in 1:length(profile_data)) {
   profile_sd_vector[q] <- sd(profile_data[[q]])
  }
lowsd_Outlier_features<-which(x = outlier_sd_vector <= max(profile_sd_vector))
  if (length(lowsd_Outlier_features)>0) {


lowsd_Outliers<-profile_outliers[lowsd_Outlier_features]
unsampled_control<-profile_data[lowsd_Outlier_features]
control<-unsampled_control[sample(nrow(unsampled_control),5000),]
is_outlier_normal_vector <-c()
is_control_normal_vector <- c()
for (j in 1:length(lowsd_Outlier_features)) {
is_outlier_normal_vector[j]<-shapiro.test(lowsd_Outliers[[j]])[["p.value"]]<=0.05
is_control_normal_vector[j]<-shapiro.test(control[[j]])[["p.value"]]<=0.05
}
#false = normally distributed
normal_columns <-lowsd_Outlier_features[which(!is_outlier_normal_vector & !is_control_normal_vector)]

abnormal_columns <- lowsd_Outlier_features[which(is_outlier_normal_vector & is_control_normal_vector)]
if (length(normal_columns)>1) {


normal_outliers <-profile_outliers[normal_columns]
normal_outlier_control <- profile_data[normal_columns]
normal_significantly_different_vector<-c()
for(c in 1:length(normal_columns))
  normal_significantly_different_vector[c]<-t.test(normal_outliers[[c]],normal_outlier_control[[c]])<=0.05
}
if (exists("normal_significantly_different_vector")) {
selected_normal_columns<-abnormal_columns[normal_significantly_different_vector]
}
if (length(abnormal_columns)>1) {


  abnormal_outliers <-profile_outliers[abnormal_columns]
  abnormal_outlier_control <- profile_data[abnormal_columns]
  abnormal_significantly_different_vector<-c()
  for(c in 1:length(abnormal_columns)){
  abnormal_significantly_different_vector[c]<-wilcox.test(abnormal_outliers[[c]],abnormal_outlier_control[[c]])[["p.value"]]<=0.05
  }
  }
  if (exists("abnormal_significantly_different_vector")) {
   selected_abnormal_columns<-abnormal_columns[abnormal_significantly_different_vector]
  }

column_list<-list()
if (exists("abnormal_significantly_different_vector")) {
  abnormaldf<-as.data.frame(profile_data[selected_abnormal_columns])
  column_list<-append(column_list,list(abnormaldf))
}
if (exists("normal_significantly_different_vector")) {
  normaldf<-as.data.frame(profile_data[selected_normal_columns])
 column_list<-append(column_list,list(normaldf))
}
return(column_list)

}else{
cat("no consistent", profile_type,"outliers")
}

}

