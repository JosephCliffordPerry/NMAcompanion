targeted_profile_analysis <- function(rawdata){
#read in dataset
error_tagged_angle_dataset<-Extreme_angle_detector(data = rawdata)
data <- filter(error_tagged_angle_dataset, suspected_detection_error == 1)
error <- filter(error_tagged_angle_dataset, suspected_detection_error == 2)

bad_edge_detected <- nrow(error)/nrow(data)
percent_bad_edge_detected <-round(bad_edge_detected*100,digits = 3)
number_of_cells_used<-nrow(data)
#cutting dataset into different portions based on content
angle_data<- data %>% dplyr::select(starts_with("Angle_profile_"))
diameter_data<- data %>% dplyr::select(starts_with("Diameter_profile_"))
radius_data<- data %>% dplyr::select(starts_with("Radius_profile_"))
other_data <- dplyr::select_if(data[7:27], is.numeric)


#makes list for bimodality check
selected_datasets <- list()
#checks all portions for bimodality
selected_datasets <- hartigansdipper(angle_data,selected_datasets)
selected_datasets <- hartigansdipper(diameter_data,selected_datasets)
selected_datasets <- hartigansdipper(radius_data,selected_datasets)
selected_datasets <- monohartigansdipper(other_data,selected_datasets)
#clusters data
clusters <- list()
clusters <-targeted_profile_clusterer(selected_datasets = selected_datasets)

#creates list of graphs
testgraphlist2<-plotbuilder3(clusters = clusters,originaldata = data,angle_data = angle_data,diameter_data = diameter_data,radius_data = radius_data )
testgraphlist2[length(testgraphlist2)+1][[1]] <- fulldatasetclustergrapher(data = data)

#creates a popout to view the graphs
graphview<-graphviewerbuilder(testgraphlist = testgraphlist2)

return(graphview)}
