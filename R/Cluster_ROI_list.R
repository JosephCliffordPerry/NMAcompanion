# clusters data

Cluster_ROI_list<-function(roi_list){
c<- targeted_profile_clusterer(selected_datasets = b)

#angle_outliers <- make_outlier_cluster(angle_data, "angle")


diameter_clusters <- targeted_profile_clusterer(selected_datasets = selected_diameter_data)

diameter_outliers <- make_outlier_cluster(diameter_data, "diameter")


radius_clusters <- targeted_profile_clusterer(selected_datasets = selected_radius_data)

radius_outliers <- make_outlier_cluster(radius_data, "radius")



other_clusters <- targeted_profile_clusterer(selected_datasets = selected_other_data)

dataset_names <- c("angle_clusters", "diameter_clusters", "radius_clusters", "other_clusters","angle_outliers","diameter_outliers", "radius_outliers","biased_angle_clusters"," biased_diameter_clusters","biased_radius_clusters")

clusters <- combine_clusters(dataset_names)}
