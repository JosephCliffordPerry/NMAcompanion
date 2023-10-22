#' Targeted profile analysis
#' This function takes a NMA dataset and produces a page of graphs that help show
#' it's structure
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
#' @importFrom ggplot2 scale_color_discrete
#' @importFrom ggplot2 theme
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
#' @importFrom dplyr summarize
#' @importFrom dplyr group_by
#' @export

targeted_profile_analysis <- function(Data, verbose_output = FALSE, make_whole_dataset_tab = TRUE) {
  if (is.data.frame(Data)) {
    print("It's a data frame.")
    rawdata <- Data
  } else {
    print("It's not a data frame.")
    rawdata <- read.table(Data, header = TRUE, sep = "\t")
  }
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

  error_tagged_angle_dataset <- Extreme_angle_detector(data = rawdata)
  data <- filter(error_tagged_angle_dataset, suspected_detection_error == 1)
  error <- filter(error_tagged_angle_dataset, suspected_detection_error == 2)

  bad_edge_detected <- nrow(error) / nrow(data)
  percent_bad_edge_detected <- round(bad_edge_detected * 100, digits = 3)
  number_of_cells_used <- nrow(data)
  # cutting dataset into different portions based on content
  angle_data <- data %>% dplyr::select(starts_with("Angle_profile_"))
  diameter_data <- data %>% dplyr::select(starts_with("Diameter_profile_"))
  radius_data <- data %>% dplyr::select(starts_with("Radius_profile_"))

  # Select columns that are numeric and don't contain the specified words ( redundant data and stuff)
  other_data <- data %>%
    select_if(is.numeric) %>%
    select(-matches("Radius_profile_|Diameter_profile_|Angle_profile_|pixels|seg|Seg|suspected_detection_error"))




  # checks all portions for bimodality
  selected_angle_data <- get.dip.test.regions(angle_data)
  selected_diameter_data <- get.dip.test.regions(diameter_data)
  selected_radius_data <- get.dip.test.regions(radius_data)
  selected_other_data <- monohartigansdipper(dataset = other_data)

  selected_datasets <- c(selected_angle_data, selected_diameter_data, selected_radius_data, selected_other_data)


  # clusters data
  angle_clusters <- targeted_profile_clusterer(selected_datasets = selected_angle_data)
  diameter_clusters <- targeted_profile_clusterer(selected_datasets = selected_diameter_data)
  radius_clusters <- targeted_profile_clusterer(selected_datasets = selected_radius_data)
  other_clusters <- targeted_profile_clusterer(selected_datasets = selected_other_data)
  clusters <- c(angle_clusters, diameter_clusters, radius_clusters, other_clusters)
  # clusters <-targeted_profile_clusterer(selected_datasets = selected_datasets)
  # umapping
  umaplist <- Umaping(originaldata = data, angle_data = angle_data, diameter_data = diameter_data, radius_data = radius_data)
  miniumaps <- make_miniumaps(clusters = clusters)
  # makes graphs of umaps of individual multimodal regions
  miniumapgraphs <- make_miniumap_graphlist(selected_datasets = selected_datasets, miniumaps = miniumaps, clusters = clusters)

  # makes consensus images
  outlinedata <- data %>% dplyr::select(starts_with("Outline_Oriented"))
  Cluster_consensus_images <- make_consensus_for_all_clusters(clusters, outlinedata = outlinedata)
  # creates list of graphs and umaps
  testgraphlist2 <- plotbuilder3(
    clusters = clusters, originaldata = data, angle_data = angle_data, diameter_data = diameter_data, radius_data = radius_data,
    umaplist = umaplist, selected_datasets = selected_datasets, miniumapgraphs = miniumapgraphs, Cluster_consensus_images = Cluster_consensus_images
  )

  if (make_whole_dataset_tab) {
    # #add a entire dataset clustered graph set
    # print("whole dataset clustering")
    testgraphlist2[length(testgraphlist2) + 1][[1]] <- fulldatasetclustergrapher(data = data, umaplist = umaplist)
  }

  # calculate rand index matrix
  rand_data <- make_randindex_data(data = data, clusters = clusters)
  rand_matrix <- calculate_rand_indexes(rand_data)
  # calculate IDs from rand index confidence groups
  confidence_groups <- give_featureidentities(rand_matrix)
  ID_list <- ID_creation(confidence_groups[["high_confidence_grouping"]])
  ID_list2 <- ID_creation(confidence_groups[["medium_confidence_grouping"]])
  ID_list3 <- ID_creation(confidence_groups[["low_confidence_grouping"]])
  full_id_list <- c(ID_list, ID_list2, ID_list3)
  # make consensus images of hamming amalgamated confidence grouping ids
  hamming_consensus <- hamming_amalgamate_Clustering(data = data, rand_data = rand_data, ID_list = full_id_list, outlinedata = outlinedata)

  # creates a popout to view the graphs
  graphview <- graphviewerbuilder(testgraphlist = testgraphlist2, clusters = clusters, data = data, hamming_consensus = hamming_consensus)

  # Creates the verbose output
  veboseoutput <- append(clusters, testgraphlist2)

  if (verbose_output) {
    return(veboseoutput)
  } else {
    return(graphview)
  }
}
