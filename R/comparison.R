# Data prep
make_comparison_data <- function(...) {
  # Combine all datasets using do.call and rbind
  combined_data <- do.call(rbind, list(...))
  combined_data$Dataset_id <- combined_data$Dataset
  return(combined_data)
}

# Cluster id + dataset id table maker
make_comparison_tables <- function(clusters, data) {
  datasetlist <- list()
  dataset_table <- as.data.frame(table(data$Dataset_id))
  for (j in 1:length(unique(data$Dataset_id))) {
    datasetlist[[dataset_table[j, 1]]] <- dataset_table[j, 2]
  }
  table_list <- list()
  for (i in 1:length(clusters)) {
    cluster_table <- as.data.frame(table(clusters[[i]][["Clustering_file"]]))


    clusdataid <- cbind(clusters[[i]][["Clustering_file"]], data$Dataset_id)
    clusdataiddf <- as.data.frame(clusdataid)
    clusdataidtable <- table(clusdataiddf)
    comparison_df <- as.data.frame(clusdataidtable)
    colnames(comparison_df) <- c("Cluster", "Dataset", "Freq")
    # dataset_vector
    dataset_vector <- as.numeric(datasetlist[as.integer(comparison_df$Dataset)])
    # Calculate the percentage of each dataset for each cluster
    comparison_df$Percentage_Of_Dataset <- ((comparison_df$Freq) / dataset_vector) * 100
    # Calculate the percentage of the cluster for each dataset
    comparison_df$Percentage_Of_Cluster <- ((comparison_df$Freq) / cluster_table[(as.numeric(comparison_df$Cluster)), 2]) * 100

    table_list[[i]] <- kable(comparison_df)
  }
  return(table_list)
}
# targeted profile comparison
targeted_profile_comparison <- function(comparison_data, verbose_output = FALSE, make_whole_dataset_tab = TRUE) {
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

  error_tagged_angle_dataset <- Extreme_angle_detector(data = comparison_data)
  data <- filter(error_tagged_angle_dataset, suspected_detection_error == 1)
  error <- filter(error_tagged_angle_dataset, suspected_detection_error == 2)

  bad_edge_detected <- nrow(error) / nrow(data)
  percent_bad_edge_detected <- round(bad_edge_detected * 100, digits = 3)
  number_of_cells_used <- nrow(data)
  # cutting dataset into different portions based on content
  angle_data <- data %>% dplyr::select(starts_with("Angle_profile_"))
  diameter_data <- data %>% dplyr::select(starts_with("Diameter_profile_"))
  radius_data <- data %>% dplyr::select(starts_with("Radius_profile_"))
  outlinedata <- data %>% dplyr::select(starts_with("Outline_Oriented"))

  # Select columns that are numeric and don't contain the specified words ( redundant data and stuff)
  other_data <- data %>%
    select_if(is.numeric) %>%
    select(-matches("Radius_profile_|Diameter_profile_|Angle_profile_|pixels|seg|Seg|suspected_detection_error|Outline_|Dataset"))




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
  # clusters <- list(angle_clusters, diameter_clusters, radius_clusters, other_clusters)
  # makes a cluster dataset that can deal with clusterings having no multimodal regions
  dataset_names <- c("angle_clusters", "diameter_clusters", "radius_clusters", "other_clusters")
  clusters <- combine_clusters(dataset_names)

  # clusters <-targeted_profile_clusterer(selected_datasets = selected_datasets)
  # umapping
  umaplist <- Umaping(originaldata = data, angle_data = angle_data, diameter_data = diameter_data, radius_data = radius_data)
  miniumaps <- make_miniumaps(clusters = clusters)
  # makes graphs of umaps of individual multimodal regions
  miniumapgraphs <- make_miniumap_graphlist(selected_datasets = selected_datasets, miniumaps = miniumaps, clusters = clusters)


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
  hamming_consensus <- comparison_hamming_amalgamate_Clustering(data = data, rand_data = rand_data, ID_list = full_id_list, outlinedata = outlinedata)

  # makes consensus images

  Cluster_consensus_images <- make_consensus_for_all_clusters(clusters, outlinedata = outlinedata)
  # make comparison
  table_list <- make_comparison_tables(clusters = clusters, data = data)

  # creates list of graphs and umaps
  testgraphlist2 <- comparisonplotbuilder3(
    clusters = clusters, originaldata = data, angle_data = angle_data, diameter_data = diameter_data, radius_data = radius_data,
    umaplist = umaplist, selected_datasets = selected_datasets, miniumapgraphs = miniumapgraphs, Cluster_consensus_images = Cluster_consensus_images,
    table_list = table_list
  )

  if (make_whole_dataset_tab) {
    # #add a entire dataset clustered graph set
    # print("whole dataset clustering")
    testgraphlist2[length(testgraphlist2) + 1][[1]] <- fulldatasetclustergrapher(data = data, umaplist = umaplist)
  }


  # creates a popout to view the graphs
  graphview <- comparisongraphviewerbuilder(testgraphlist = testgraphlist2, clusters = clusters, data = data, hamming_consensus = hamming_consensus)

  # Creates the verbose output
  veboseoutput <- append(clusters, testgraphlist2)

  if (verbose_output) {
    return(veboseoutput)
  } else {
    return(graphview)
  }
}
