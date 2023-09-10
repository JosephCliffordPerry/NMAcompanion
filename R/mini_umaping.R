make_miniumaps <- function(clusters) {
 miniumaps<-list()
   for (i in 1:length(clusters)) {
    cluster_column <- clusters[[i]]$Clustering_file
    other_columns <- as.data.frame(clusters[[i]][, -which(names(clusters[[i]]) == "Clustering_file")])

  cat("umap",i,"/",length(clusters),"started","....")
    umap_result <- umap(other_columns)
    cat("umap",i,"/",length(clusters),"finished",".....")
    miniumaps[[i]]<-umap_result


     }
 cat("miniumaps complete")
 return(miniumaps)
}

#miniumaps<-make_miniumaps(clusters = clusters)


make.miniumapgraph <- function(clusters, umap ,graphtype){
  # Convert clusters to factor to preserve the correct order
  clusters_factor <- factor(clusters)

  umapcluster <-cbind(umap,clusters_factor)

  gromph1 <- ggplot(data = umapcluster, aes(V1, V2, color = clusters_factor)) +
    geom_point() + labs(title = title,x = paste0(graphtype, " variable 1"),y = paste0(graphtype, " variable 2"),colour = "clusters") +
    facet_wrap(clusters_factor)

  gromph2 <- ggplot(data = umapcluster, aes(V1, V2, color = clusters_factor)) +
    geom_point() + labs(title = title,x = paste0(graphtype, " variable 1"),y = paste0(graphtype, " variable 2"), colour = "clusters")

  graph1 <- gromph1 + gromph2
  graph1}


