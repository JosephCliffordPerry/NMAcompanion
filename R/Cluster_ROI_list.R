# clusters ROI data
#'@export
Cluster_ROI_list<-function(roi_list){
clusters<- targeted_profile_clusterer(selected_datasets = roi_list)
for (i in length(clusters):1) {
  # Check if any element in clusters[[name]] contains "error"
  if (any(grepl("no profiles", clusters[[i]], ignore.case = TRUE))) {
    # Print message for section removed
    cat("Section removed due to error:", names(clusters[i]), "\n")

    # Remove the section from the list
    clusters[[i]] <- NULL
  }
}
return(clusters)
}
