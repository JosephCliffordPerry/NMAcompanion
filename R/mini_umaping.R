make_miniumaps <- function(clusters) {
  miniumaps <- list()
  for (i in 1:length(clusters)) {
    cluster_column <- clusters[[i]]$Clustering_file
    other_columns <- as.data.frame(clusters[[1]][, -which(names(clusters[[1]]) == "Clustering_file")])
    if (is.numeric(other_columns[[1]])) {
      cat("umap", i, "/", length(clusters), "started", "....")
      umap_result <- umap(other_columns)
      cat("umap", i, "/", length(clusters), "finished", ".....")
      miniumaps[[i]] <- umap_result
    } else {
      cat("umap not applicable")
      miniumaps[[i]] <- "umap not applicable"
    }
  }
  cat("miniumaps complete")
  return(miniumaps)
}

# miniumaps<-make_miniumaps(clusters = clusters)


make.miniumapgraph <- function(clusters, umap, graphtype) {
  # Convert clusters to factor to preserve the correct order
  clusters_factor <- factor(clusters)
  umapdf <- as.data.frame(umap[["layout"]])
  umapcluster <- as.data.frame(cbind(umapdf, clusters_factor))
  title <- paste0(graphtype)
  gromph1 <- ggplot(data = umapcluster, aes(V1, V2, color = clusters_factor)) +
    geom_point() +
    labs(title = NULL, x = NULL, y = NULL, colour = "clusters") +
    facet_wrap(clusters_factor) +
    theme_minimal() +
    theme(legend.position = "none") +
    coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")

  gromph2 <- ggplot(data = umapcluster, aes(V1, V2, color = clusters_factor)) +
    geom_point() +
    labs(title = "UMAP of multimodal regions", x = "UMAP1", y = "UMAP2", colour = "clusters") +
    theme_minimal() +
    theme(legend.position = "none") +
    coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")

  graph1 <- gromph2 + gromph1
  graph1
}

make_miniumap_graphlist <- saveminiumap <- function(selected_datasets, miniumaps, clusters) {
  miniumaplist <- list()
  for (i in 1:length(selected_datasets)) {
    miniumapgraph1 <- make.miniumapgraph(clusters = clusters[[i]][["Clustering_file"]], umap = miniumaps[[i]], graphtype = names(selected_datasets)[i])

    miniumaplist[[i]] <- miniumapgraph1
  }
  return(miniumaplist)
}

# # saveminiumap<-function(clusternum,selected_datasets,miniumaps,clusters){
# # miniumapgraph1<-make.miniumapgraph(clusters = clusters[[clusternum]][["Clustering_file"]],umap = miniumaps[[clusternum]],graphtype = names(selected_datasets)[clusternum])
# ggsave(plot = miniumapgraph1,filename = paste0("miniumap",clusternum,".png"),path = "C:/Users/User/Documents/0stuff/Masters project/Write up/Figures/Results figures/Miniumapgraphs", dpi=300, units="mm", width = 170, height = 78)
# }

# miniumapgraphs<-make_miniumap_graphlist(selected_datasets = selected_datasets,miniumaps = miniumaps,clusters = clusters)
