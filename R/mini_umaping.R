make_miniumap <- function(clusters) {
 miniumaps<-list()
   for (i in 1:length(clusters)) {
    cluster_column <- clusters[[i]]$Clustering_file
    other_columns <- as.data.frame(clusters[[i]][, -which(names(clusters[[i]]) == "Clustering_file")])

  paste0("umap",i,"/",length(clusters),"started")
    umap_result <- umap(other_columns)
    paste0("umap",i,"/",length(clusters),"finished")

    miniumaps[[i]]<-make.umapgraph(clusters = cluster_column, umap = umap_result, graphtype = "angle cluster")
   }
 paste0("miniumaps complete")
 return(miniumaps)
}

miniumaps<-make_miniumap(clusters = clusters)

make.umapgraph <- function(clusters, umap ,graphtype){
  # Convert clusters to factor to preserve the correct order
  clusters_factor <- factor(clusters)
umapdata<-as.data.frame(umap$layout)
 umapcluster <-cbind(umapdata,clusters_factor)

  gromph1 <- ggplot(data =umapcluster, aes(V1, V2, color = clusters_factor)) +
    geom_point() + labs(title = title,x = paste0(graphtype, " variable 1"),y = paste0(graphtype, " variable 2"),colour = "clusters") +
    facet_wrap(clusters_factor)

  gromph2 <- ggplot(data = umapcluster, aes(V1, V2, color = clusters_factor)) +
    geom_point() + labs(title = title,x = paste0(graphtype, " variable 1"),y = paste0(graphtype, " variable 2"), colour = "clusters")

  graph1 <- gromph1 + gromph2
  graph1}``



make.umapgraph <- function(clusters, umap, graphtype) {
  # Convert clusters to factor to preserve the correct order
  clusters_factor <- factor(clusters)
  umapdata <- as.data.frame(umap$layout)
  umapcluster <- cbind(umapdata, clusters_factor)

  # Define the title as a character string
  title <- paste0(graphtype, " variable")

  gromph1 <- ggplot(data = umapcluster, aes(V1, V2, color = clusters_factor)) +
    geom_point() + labs(title = title, x = paste0(graphtype, " variable 1"),
                        y = paste0(graphtype, " variable 2"), colour = "clusters") +
    facet_wrap(clusters_factor)

  gromph2 <- ggplot(data = umapcluster, aes(V1, V2, color = clusters_factor)) +
    geom_point() + labs(title = title, x = paste0(graphtype, " variable 1"),
                        y = paste0(graphtype, " variable 2"), colour = "clusters")

  graph1 <- gromph1 + gromph2
  return(graph1)
}
