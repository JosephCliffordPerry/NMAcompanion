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

#miniumaps<-make_miniumap(clusters = clusters)


