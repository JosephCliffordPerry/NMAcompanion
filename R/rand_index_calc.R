# install.packages("fossil")
# library(fossil)
# rand index calc
make_randindex_data <- function(data, clusters) {
  clusties <- data$CellID

  for (i in 1:length(clusters)) {
    x <- as.numeric(clusters[[i]][["Clustering_file"]])
    # Assign a unique name to x based on the cluster index (i)
    col_name <- paste0("Clustering_", i)
    # Create a new data frame with the same number of rows as data
    x_df <- as.data.frame(x = x)

    # Rename the column to col_name
    colnames(x_df) <- col_name
    # Combine x into clusties with the assigned column name
    clusties <- cbind(clusties, x_df)
  }
  return(clusties)
}

#rand_data<-make_randindex_data(data = data,clusters = clusters)


######################
calculate_rand_indexes <- function(rand_data) {
  n <- ncol(rand_data)
  result_matrix <- matrix(NA, nrow = n - 1, ncol = n - 1)

  for (i in 2:(n - 1)) {
    for (j in (i + 1):n) {
      # Extract the vectors from the data frame
      vector1 <- rand_data[[i]]
      vector2 <- rand_data[[j]]



      rand_idx <- rand.index(vector1, vector2)
      result_matrix[i - 1, j - 1] <- rand_idx
      # result_matrix[j - 1, i - 1] <- rand_idx
      # Print progress
      cat("Calculated Rand Index for", colnames(rand_data)[i], "vs", colnames(rand_data)[j], "\n")
    }
  }

  return(result_matrix)
}


# rand_matrix <- calculate_rand_indexes(rand_data)
