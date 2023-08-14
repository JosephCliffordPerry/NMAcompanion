#'Targeted profile analysis
#'This function takes a NMA dataset and produces a page of graphs that help show
#'it's structure
#' @param Data the file path to nuclear measurements exported dataset
#' from NMA
#' @param verbose_output gives the output as a list of graphs and data instead
#' of a Shiny interface
#' @importFrom factoextra fviz_nbclust
#' @importFrom factoextra hkmeans
#' @importFrom umap umap
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 geom_line
#' @importFrom dplyr filter
#' @importFrom shiny shinyUI
#' @importFrom shiny fluidPage
#' @importFrom shiny titlePanel
#' @importFrom shiny mainPanel
#' @importFrom shiny selectInput
#' @importFrom shiny plotOutput
#' @importFrom shiny sidebarLayout
#' @importFrom shiny sidebarPanel
#' @importFrom shiny verbatimTextOutput
#' @importFrom shiny shinyServer
#' @importFrom shiny observe
#' @importFrom shiny renderPlot
#' @importFrom shiny renderPrint
#' @importFrom shiny shinyApp
#' @importFrom shiny tabsetPanel
#' @importFrom shiny tabPanel
#' @importFrom stringr str_extract_all
#' @importFrom dplyr %>%
#' @importFrom dplyr starts_with
#' @importFrom dplyr select
#' @importFrom dplyr select_if
#' @export
targeted_profile_analysis <- function(Data, verbose_output = FALSE){
  if (is.data.frame(Data)) {
    print("It's a data frame.")
    rawdata <-Data
  } else {
    print("It's not a data frame.")
    rawdata <- read.table(Data, header = TRUE, sep = "\t")
  }
  Extreme_angle_detector <- function(data){

    #cutting dataset into different portions based on content
    dataset<- data %>% dplyr::select(starts_with("Angle_profile_"))

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
    return(data)}

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

# Select columns that are numeric and don't contain the specified words ( redundant data and stuff)
other_data <- data %>%
  select_if(is.numeric) %>%
  select(-matches("Radius_profile_|Diameter_profile_|Angle_profile_|pixels|seg|Seg|suspected_detection_error"))




#checks all portions for bimodality
selected_angle_data<- get.dip.test.regions(angle_data)
selected_diameter_data <- get.dip.test.regions(diameter_data)
selected_radius_data <- get.dip.test.regions(radius_data)

selected_other_data <- data.frame(lapply(X = other_data,FUN = get.dip.test.regions))

selected_datasets <- c(selected_angle_data, selected_diameter_data, selected_radius_data, selected_other_data)


#clusters data
angle_clusters <- targeted_profile_clusterer(selected_datasets = selected_angle_data)
clusters <-targeted_profile_clusterer(selected_datasets = selected_datasets)
#umapping
umaplist <- Umaping(originaldata = data,angle_data = angle_data,diameter_data = diameter_data,radius_data = radius_data)
#creates list of graphs and umaps
testgraphlist2<-plotbuilder3(clusters = clusters,originaldata = data,angle_data = angle_data,diameter_data = diameter_data,radius_data = radius_data,
                             umaplist = umaplist,selected_datasets = selected_datasets)


#add a entire dataset clustered graph set
print("whole dataset clustering")
testgraphlist2[length(testgraphlist2)+1][[1]] <- fulldatasetclustergrapher(data = data,umaplist = umaplist)


#creates a popout to view the graphs
graphview<-graphviewerbuilder(testgraphlist = testgraphlist2)

#Creates the verbose output
veboseoutput<- append(clusters,testgraphlist2)

if (verbose_output){
  return(veboseoutput)}
else{
  return(graphview)}
}
