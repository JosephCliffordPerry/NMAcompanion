# clusters ROI data

Cluster_ROI_list<-function(roi_list){
clusters<- targeted_profile_clusterer(selected_datasets = roi_list)
return(clusters)
}
