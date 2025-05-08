#' Create Rand Index Input Data
#'
#' This internal function prepares a data frame suitable for calculating pairwise
#' Rand Index comparisons between multiple clustering results.
#'
#' @param data A data frame containing at least a \code{CellID} column used to align clustering results.
#' @param clusters A list of clustering outputs, where each element contains a \code{Clustering_file}
#'   vector assigning cluster labels to the observations in \code{data}.
#'
#' @return A data frame where the first column is \code{CellID} and each additional column contains
#' numeric cluster assignments from a specific clustering result.
#'
#' @keywords internal
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

#' Calculate Pairwise Rand Index Scores Between Clusterings
#'
#' This internal function computes the Rand Index between all pairs of clustering results
#' provided in the input data frame.
#'
#' @param rand_data A data frame returned by \code{make_randindex_data()}, where each column
#' (after the first) represents a set of cluster labels.
#'
#' @return A symmetric matrix of Rand Index values comparing each pair of clusterings.
#'
#' @details
#' The Rand Index quantifies the similarity between two clusterings by considering all
#' pairs of elements and counting pairs that are assigned in the same or different clusters
#' in both clusterings.
#'@import fossil
#' @importFrom fossil rand.index
#' @seealso \code{\link[fossil]{rand.index}} from the \pkg{fossil} package.
#' @keywords internal
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
