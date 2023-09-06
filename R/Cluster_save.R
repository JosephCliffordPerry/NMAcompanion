#' save cluster
export_cluster <- function(filename, data, clusters, clusternumber) {
  cellid <- data$CellID
  clust <- clusters[[clusternumber]][["Clustering_file"]]
  export <- data.frame(CellID = cellid, Cluster = clust)

  write.table(export, file = filename, sep = "\t", quote = FALSE, row.names = FALSE)
}

#export_cluster(filename = "cluster11.txt",data =data,clusters = clusters, clusternumber = 11 )
