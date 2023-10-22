calc_clus_centroids <- function(umap, clusters, clustertype) {
  centroids <- list()

  for (i in 1:length(clusters)) {
    x <- clusters[[i]][["Clustering_file"]]

    z <- as.data.frame(umap$layout)
    umapclusframe <- data.frame(z, x) # Convert to data frame

    section_name <- paste0(i, clustertype) # Create the section name
    section_centroids <- list()

    for (j in 1:max(x)) {
      part <- umapclusframe %>% filter(x == j)
      centroid <- c(mean(part$V1), mean(part$V2))
      inner_name <- paste0(section_name, "_", ".", j) # Create the centroid name
      section_centroids[[inner_name]] <- centroid # Assign the centroid to the section list
    }
    #   umapclusframe %>% dplyr::group_by("Clustering_file") %>% dplyr::summarise(meanX = mean(V1), meanY = mean(V2))%>%
    # dplyr::mutate(names = paste0(row_number(),clustertype))
    centroids[[section_name]] <- section_centroids # Assign the section to the main list
  }

  return(centroids)
}
################
calc_clus_centroid_matrix <- function(umap, clusters, clustertype) {
  centroids <- list()

  for (i in 1:length(clusters)) {
    x <- clusters[[i]][["Clustering_file"]]

    z <- as.data.frame(umap$layout)
    umapclusframe <- data.frame(z, x) # Convert to data frame

    section_name <- paste0(i) # Create the section name
    section_centroids <- list()

    for (j in 1:max(x)) {
      part <- umapclusframe %>% filter(x == j)
      centroid <- c(mean(part$V1), mean(part$V2))
      inner_name <- paste0(section_name, ".", j) # Create the centroid name
      section_centroids[[inner_name]] <- centroid # Assign the centroid to the section list
    }

    centroids[[section_name]] <- section_centroids # Assign the section to the main list
  }

  return(centroids)
}



# full_centroid_list<- calc_clus_centroids(umap = umaplist[[1]],clusters = clusters,"whole_dataset")

####################
build_full_centroid_list <- function(umaplist, angle_clusters, radius_clusters, diameter_clusters, other_clusters) {
  angle_centroids <- calc_clus_centroids(umap = umaplist[[2]], clusters = angle_clusters, "angle_cluster")
  radius_centroids <- calc_clus_centroids(umap = umaplist[[4]], clusters = radius_clusters, "radius_cluster")
  diameter_centroids <- calc_clus_centroids(umap = umaplist[[3]], clusters = diameter_clusters, "diameter_cluster")
  other_centroids <- calc_clus_centroids(umap = umaplist[[1]], clusters = other_clusters, "other_cluster")
  full_centroid_list <- c(angle_centroids, radius_centroids, diameter_centroids, other_centroids)
  return(full_centroid_list)
}
# full_centroid_list<-build_full_centroid_list(umaplist,angle_clusters,radius_clusters ,diameter_clusters ,other_clusters )
#######################
build_distance_matrix <- function(full_centroid_list) {
  # Extract centroid coordinates from the nested list
  centroid_coords <- unlist(full_centroid_list, recursive = FALSE)
  centroid_matrix <- do.call(rbind, centroid_coords)
  centroid_distance_matrix <- as.matrix(dist(centroid_matrix))
  return(centroid_distance_matrix)
}

# cdm<-build_distance_matrix(full_centroid_list)
#######################
extract_centroid_table <- function(threshold, centroid_distance_matrix) {
  centroid_table <- list() # Initialize an empty data frame
  centroid_df <- as.data.frame(centroid_distance_matrix)
  for (col_name in names(centroid_df)) {
    # Use logical indexing to select rows below the threshold
    selected_rows <- rownames(centroid_df[centroid_df[, col_name] < threshold, ])
    # Check if there are any selected rows before creating the new column
    if (length(selected_rows) > 0) {
      # Create a new column in the new data frame for the selected row names
      centroid_table[[col_name]] <- selected_rows
    }
  }
  return(centroid_table)
}

# ct<-extract_centroid_table(threshold = 1, centroid_distance_matrix = cdm )
