#' Build graphs for whole dataset tab
fulldatasetclustergrapher <- function(data, umaplist) {
  print("clustering whole dataset")
  dataset <- cbind(other_data, radius_data, angle_data, diameter_data)

  # function that calculates the number of clusters from the WSS cluster number calculator
  WSSNclustercalc <- function(clusterWSS) {
    x <- clusterWSS[["data"]][["y"]]
    x2 <- diff(x)
    x3 <- which(x2 > x2[1] / 5)
    return(min(x3))
  }


  # Scale the selected columns
  scaleddata <- scale(dataset)
  # sample out the maximum amount of data that can be processed with  fviz_nbclust
  sampled_data <- as.data.frame(scaleddata[sample(nrow(dataset), 1000), ])
  # Perform clustering and determine the optimal number of clusters
  clustersSil <- fviz_nbclust(sampled_data, FUNcluster = kmeans, method = "silhouette")
  # clusterNBC <- NbClust(sampled_data, distance = "euclidean",
  #         min.nc = 2, max.nc = 9,
  #        method = "complete", index ="all")
  #   n <- fviz_nbclust(clusterNBC)
  # clusterNBCN <- as.numeric(gsub("\\D", "", n[["labels"]][["title"]]))
  clusterWSS <- fviz_nbclust(sampled_data, FUNcluster = kmeans, method = "wss")

  clustergap <- fviz_nbclust(sampled_data, FUNcluster = kmeans, method = "gap")
  gapNclusters <- which.max(clustergap[["data"]][["gap"]])
  SilNclusters <- which.max(clustersSil[["data"]][["y"]])

  WSSNclusters <- WSSNclustercalc(clusterWSS)

  Nclusters <- round(mean(c(WSSNclusters, SilNclusters, gapNclusters))) # add clusterNBCN if needed
  # clusters dataset
  clusters <- hkmeans(scaleddata, k = Nclusters)
  print("Finished clustering whole dataset")
  # Extract the clustering results
  Clustering_file <- clusters$cluster

  # make umap dataframes
  umapo <- umaplist[1]
  angleumap <- umaplist[2]
  diameterumap <- umaplist[3]
  radiusumap <- umaplist[4]

  umapodata <- as.data.frame(umapo[[1]][["layout"]])
  angleumapdata <- as.data.frame(angleumap[[1]][["layout"]])
  radiusumapdata <- as.data.frame(diameterumap[[1]][["layout"]])
  diameterumapdata <- as.data.frame(radiusumap[[1]][["layout"]])
  title <- "Whole dataset clustered"

  # Convert clusters to factor to preserve the correct order
  clusters_factor <- factor(Clustering_file)
  umapo_cluster <- cbind(umapodata, clusters_factor)
  angleumapcluster <- cbind(angleumapdata, clusters_factor)
  radiusumapcluster <- cbind(radiusumapdata, clusters_factor)
  diameterumapcluster <- cbind(diameterumapdata, clusters_factor)
  gromph1 <- ggplot(data = umapo_cluster, aes(umapo_cluster$V1, umapo_cluster$V2, color = clusters_factor)) +
    geom_point() +
    labs(title = title, x = "whole dataset variable 1", y = "whole dataset variable 2") +
    facet_wrap(clusters_factor)

  gromph2 <- ggplot(data = umapo_cluster, aes(umapo_cluster$V1, umapo_cluster$V2, color = clusters_factor)) +
    geom_point() +
    labs(title = title, x = "whole dataset variable 1", y = "whole dataset variable 2")

  graphA <- gromph1 + gromph2

  gromph3 <- ggplot(data = angleumapcluster, aes(V1, V2, color = clusters_factor)) +
    geom_point() +
    labs(title = title, x = "Angle Umap variable 1", y = "Angle Umap variable 2") +
    facet_wrap(clusters_factor)
  gromph4 <- ggplot(data = radiusumapcluster, aes(V1, V2, color = clusters_factor)) +
    geom_point() +
    labs(title = title, x = "Radius Umap variable 1", y = "Diameter Umap variable 2") +
    facet_wrap(clusters_factor)
  gromph5 <- ggplot(data = diameterumapcluster, aes(V1, V2, color = clusters_factor)) +
    geom_point() +
    labs(title = title, x = "Diameter Umap variable 1", y = "Diameter Umap variable 2") +
    facet_wrap(clusters_factor)

  gromph6 <- ggplot(data = angleumapcluster, aes(V1, V2, color = clusters_factor)) +
    geom_point() +
    labs(title = title, x = "Angle Umap variable 1", y = "Angle Umap variable 2")
  gromph7 <- ggplot(data = radiusumapcluster, aes(V1, V2, color = clusters_factor)) +
    geom_point() +
    labs(title = title, x = "Radius Umap variable 1", y = "Diameter Umap variable 2")
  gromph8 <- ggplot(data = diameterumapcluster, aes(V1, V2, color = clusters_factor)) +
    geom_point() +
    labs(title = title, x = "Diameter Umap variable 1", y = "Diameter Umap variable 2")

  graphB <- gromph3 + gromph6
  graphC <- gromph5 + gromph8
  graphD <- gromph4 + gromph7

  diameter_clusters <- cbind(diameter_data, Clustering_file)
  d1 <- list()

  for (j in 1:max(Clustering_file)) {
    D1 <- diameter_clusters %>% filter(Clustering_file == j)
    d1[[j]] <- apply(D1[1:100], 2, median)
  }

  q <- length(d1)
  d2 <- data.frame(x = 1:100, y = unlist(d1), group = rep(1:q, each = 100))

  graphE <- ggplot(d2, aes(x = x, y = y, group = group, color = as.factor(group))) +
    geom_line() +
    labs(x = "Profile position", y = "Length of Diameter", color = "Group", title = "Diameter profiles")
  radius_clusters <- cbind(radius_data, Clustering_file)
  r1 <- list()

  for (j in 1:max(Clustering_file)) {
    R1 <- radius_clusters %>% filter(Clustering_file == j)
    r1[[j]] <- apply(R1[1:100], 2, median)
  }

  q <- length(r1)
  r2 <- data.frame(x = 1:100, y = unlist(r1), group = rep(1:q, each = 100))

  graphF <- ggplot(r2, aes(x, y, group = group, color = as.factor(group))) +
    geom_line() +
    labs(x = "Profile position", y = "Radius Length", color = "Group", title = "radius profiles")

  angle_clusters <- cbind(angle_data, Clustering_file)
  a1 <- list()

  for (j in 1:max(Clustering_file)) {
    A1 <- angle_clusters %>% filter(Clustering_file == j)
    a1[[j]] <- apply(A1[1:100], 2, median)
  }

  q <- length(a1)
  a2 <- data.frame(x = 1:100, y = unlist(a1), group = rep(1:q, each = 100))

  graphG <- ggplot(a2, aes(x, y, group = group, color = as.factor(group))) +
    geom_line() +
    labs(x = "Profile position", y = "Angle", color = "Group", title = "angle profiles")

  dataset$Clustering_file <- Clustering_file
  graphH <- make_cluster_consensus(cluster = dataset, outlinedata = outlinedata)


  graphs <- list(
    graphA = graphA, graphB = graphB, graphC = graphC,
    graphD = graphD, graphE = graphE, graphF = graphF,
    graphG = graphG, graphH = graphH
  )

  return(graphs)
}
