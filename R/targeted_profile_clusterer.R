
###TODO fix documentation
#############################################
#' clusters data automatically using hkmeans
#' @param selected_datasets this is the output of the hartigansdippers and mono
#' @param allow_further_itteration if true increases clustering itterations
#' hartigans dippers.
# function that takes a list of datasets itterates through them determining optimum clusters then clustering
targeted_profile_clusterer <- function(selected_datasets, allow_further_itteration = FALSE) {

   clusters <- list()
  # function that calculates the number of clusters from the WSS cluster number calculator
  WSSNclustercalc <- function(clusterWSS) {
    x <- clusterWSS[["data"]][["y"]]
    x2 <- diff(x)
    x3 <- which(x2 > x2[1] / 5)
    return(min(x3))
  }


  # Create an empty list to store the clustering results
  clustering_results <- list()
  set.seed(0800001066)
if(length(selected_datasets)>0 & is.list(selected_datasets)){
  # Iterate through the selected datasets
  for (i in 1:length(selected_datasets)) {
    # Get the current dataset
    dataset <- selected_datasets[[i]]
    print(paste(i, "/", length(selected_datasets)))
    # Scale the selected columns
    scaleddata <- scale(dataset)
    if (nrow(dataset) >= 1000) {
      # sample out the maximum amount of data that can be processed with  fviz_nbclust
      sampled_data <- as.data.frame(scaleddata[sample(nrow(dataset), 1000), ])
    } else {
      sampled_data <- scaleddata
    }

    # Perform clustering and determine the optimal number of clusters
    clustersSil <- fviz_nbclust(sampled_data, FUNcluster = kmeans, method = "silhouette")

    clusterWSS <- fviz_nbclust(sampled_data, FUNcluster = kmeans, method = "wss")

    suppressWarnings(clustergap <- fviz_nbclust(sampled_data, FUNcluster = kmeans, method = "gap"))
    gapNclusters <- which.max(clustergap[["data"]][["gap"]])
    SilNclusters <- which.max(clustersSil[["data"]][["y"]])

    WSSNclusters <- WSSNclustercalc(clusterWSS)

    Nclusters <- round(mean(c(WSSNclusters, SilNclusters, gapNclusters))) # add clusterNBCN if needed
    # clusters dataset
    # Attempt clustering with default iteration limit
    clusters <- if (allow_further_itteration == TRUE) {
       hkmeans(scaleddata, k = Nclusters, iter.max = 100)
      } else {
       hkmeans(scaleddata, k = Nclusters)  # Retry anyway, or leave it here
      }


    # Extract the clustering results
    Clustering_file <- clusters$cluster


    # Create a data frame with the original data and clustering results
    clustering_data <- cbind(dataset, Clustering_file)

    # Add the clustering results to the list
    clustering_results[[i]] <- clustering_data
  }}else{
    clustering_results[[1]] <-paste0("no profiles")

     if (length(selected_datasets == 0)) {
       clustering_results[[1]] <-paste0(clustering_results[[1]]," no profiles in selected dataset")
    }
    if(is.list(selected_datasets)==FALSE){
      clustering_results[[1]] <-paste0(clustering_results[[1]]," dataset is not a list")
    }
  }
names(clustering_results)<-names(selected_datasets)

  return(clustering_results)
}

########################
# cluster list builder

combine_clusters <- function(dataset_names) {
  # Initialize an empty list
  clusters <- list()

  # Check if the datasets exist and add them to the list
  for (name in dataset_names) {
    if (exists(name)) {
      clusters <- c(clusters, list(get(name)))
    }
  }

  # Check if any datasets were added to the list
  if (length(clusters) == 0) {
    cat("No multimodal regions\n")
  }

  # Now 'clusters' contains the existing datasets as a list
  return(clusters)
}
