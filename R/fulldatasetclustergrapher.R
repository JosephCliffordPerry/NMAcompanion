#' @importFrom factoextra fviz_nbclust
#' @importFrom factoextra kmeans
#' @importFrom factoextra hkmeans
#' @importFrom umap umap
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 geom_line
#' @importFrom dplyr filter



fulldatasetclustergrapher <- function(data){

  dataset <- dplyr::select_if(data[7:327], is.numeric)

  #function that calculates the number of clusters from the WSS cluster number calculator
  WSSNclustercalc <- function(clusterWSS){
    x <- clusterWSS[["data"]][["y"]]
    x2 <- diff(x)
    x3 <- which(x2 > x2[1]/5)
    return(min(x3))}


  # Scale the selected columns
  scaleddata <- scale(dataset)
  #sample out the maximum amount of data that can be processed with  fviz_nbclust
  sampled_data <- as.data.frame(scaleddata[sample(nrow(data),1000), ])
  # Perform clustering and determine the optimal number of clusters
  clustersSil <- fviz_nbclust(sampled_data, FUNcluster = kmeans, method = "silhouette")
  #clusterNBC <- NbClust(sampled_data, distance = "euclidean",
  #         min.nc = 2, max.nc = 9,
  #        method = "complete", index ="all")
  #   n <- fviz_nbclust(clusterNBC)
  #clusterNBCN <- as.numeric(gsub("\\D", "", n[["labels"]][["title"]]))
  clusterWSS <- fviz_nbclust(sampled_data, FUNcluster = kmeans, method = "wss")

  clustergap <- fviz_nbclust(sampled_data, FUNcluster = kmeans, method = "gap")
  gapNclusters <- which.max(clustergap[["data"]][["gap"]])
  SilNclusters <- which.max(clustersSil[["data"]][["y"]])

  WSSNclusters <- WSSNclustercalc(clusterWSS)

  Nclusters <- round(mean(c(WSSNclusters,SilNclusters,gapNclusters)))#add clusterNBCN if needed
  #clusters dataset
  clusters <- hkmeans(scaleddata, k = Nclusters)

  # Extract the clustering results
  Clustering_file <- clusters$cluster

  umapo <- umap(dataset, preserve.seed = TRUE)
  print("umap1 done")
  angleumap <- umap(angle_data, preserve.seed = TRUE)
  print("umap2 done")
  umapodata <- as.data.frame(umapo$layout)
  angleumapdata <- as.data.frame(angleumap$layout)

  title <- "Whole dataset clustered"

  # Convert clusters to factor to preserve the correct order
  clusters_factor <- factor(Clustering_file)
  umapo_cluster<-cbind(umapodata,clusters_factor)
  angleumapcluster <-cbind(angleumapdata,clusters_factor)
  gromph1 <- ggplot(data = umapo_cluster, aes(V1, V2, color = clusters_factor)) +
    geom_point() + labs(title = title) +
    facet_wrap(clusters_factor)

  gromph2 <- ggplot(data = umapo_cluster, aes(V1, V2, color = clusters_factor)) +
    geom_point() + labs(title = title)

  graph1 <- gromph1 + gromph2

  gromph3 <- ggplot(data = angleumapcluster, aes(V1, V2, color = clusters_factor)) +
    geom_point() + labs(title = title) +
    facet_wrap(clusters_factor)

  gromph4 <- ggplot(data = angleumapcluster, aes(V1, V2, color = clusters_factor)) +
    geom_point() + labs(title = title)

  graph2 <- gromph3 + gromph4

  graphs <- list(graph1 = graph1, graph2 = graph2, graph3 = NULL)
  diameter_clusters <- cbind(diameter_data, Clustering_file)
  d1 <- list()

  for (j in 1:max(Clustering_file)) {
    D1 <- diameter_clusters %>% filter(Clustering_file == j)
    d1[[j]] <- apply(D1[1:100], 2, median)
  }

  q <- length(d1)
  d2 <- data.frame(x = 1:100, y = unlist(d1), group = rep(1:q, each = 100))

  d3 <- ggplot(d2, aes(x, y, group = group, color = as.factor(group))) +
    geom_line() +
    labs(x = "X", y = "Y", color = "Group",title = "Diameter profiles")
  radius_clusters <- cbind(radius_data, Clustering_file)
  r1 <- list()

  for (j in 1:max(Clustering_file)) {
    R1 <- radius_clusters %>% filter(Clustering_file == j)
    r1[[j]] <- apply(R1[1:100], 2, median)
  }

  q <- length(r1)
  r2 <- data.frame(x = 1:100, y = unlist(r1), group = rep(1:q, each = 100))

  r3 <- ggplot(r2, aes(x, y, group = group, color = as.factor(group))) +
    geom_line() +
    labs(x = "X", y = "Y", color = "Group",title = "radius profiles")

  angle_clusters <- cbind(angle_data, Clustering_file)
  a1 <- list()

  for (j in 1:max(Clustering_file)) {
    A1 <- angle_clusters %>% filter(Clustering_file == j)
    a1[[j]] <- apply(A1[1:100], 2, median)
  }

  q <- length(a1)
  a2 <- data.frame(x = 1:100, y = unlist(a1), group = rep(1:q, each = 100))

  a3 <- ggplot(a2, aes(x, y, group = group, color = as.factor(group))) +
    geom_line() +
    labs(x = "X", y = "Y", color = "Group",title = "angle profiles")

  graph3 <- d3+a3+r3

  graphs <- list(graph1 = graph1, graph2 = graph2, graph3 = graph3)

  return(graphs)}
