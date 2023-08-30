export_cluster <- function(filename, data, clusternumber) {
  cellid <- data$CellID
  clust <- clusters[[clusternumber]][["Clustering_file"]]
  export <- data.frame(CellID = cellid, Cluster = clust)

  write.table(export, file = filename, sep = "\t", quote = FALSE, row.names = FALSE)
}
