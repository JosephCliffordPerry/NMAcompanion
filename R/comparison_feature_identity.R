
# a function to make consensus images of hamming amalgamated data and a comparison table
comparison_hamming_amalgamate_Clustering <- function(data = data, ID_list = ID_list, rand_data = rand_data, outlinedata = outlinedata) {
  hamming_consensus_list <- list()
  for (i in 1:length(ID_list)) {
    X <- as.numeric(ID_list[[i]])
    titleX <- X
    # adds one so that it X matches column indexes in rand data
    X <- X + 1

    iddf <- data.frame(
      combined_column = apply(rand_data[, X], 1, function(row) {
        paste(row, collapse = "")
      })
    )
    # makes a vector of all cluster ids from the feature group
    ids <- iddf[[1]]
    # makes a hamming cluster
    hamming_cluster <- cluster_characterising(data = data, ids = ids)
    # makes the right column for cluster consensus then makes the consensus
    hamming_cluster$Clustering_file <- hamming_cluster$Cluster_characterising_ids
    comparison_table<-make_comparison_tables(clusters = list(hamming_cluster),data = data)
    hamming_consensus <- Make_hamming_consensus_images(outlinedata = outlinedata, cluster = hamming_cluster)

    # Determine the range of numbers

    hamming_consensus <- hamming_consensus + labs(title = paste0("Morphotypes from clusterings", " ", paste(titleX, collapse = ", ")))

    hamming_consensus_list[[i]] <- list(graph1 = hamming_consensus,table_bable = comparison_table[[1]])
  }
  cat("done")

  return(hamming_consensus_list)
}
