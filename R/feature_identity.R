# library(dplyr)
# library(tidyr)
#
#
#
give_featureidentities <- function(rand_matrix) {
  rand_df <- as.data.frame(rand_matrix)
  groupinglist <- list()
  # Create a logical matrix for each condition
  condition_1 <- rand_matrix >= 0.5
  condition_2 <- rand_matrix > 0.6
  condition_3 <- rand_matrix > 0.7

  low_confidence_grouping <- which(condition_1, arr.ind = TRUE)
  medium_confidence_grouping <- which(condition_2, arr.ind = TRUE)
  high_confidence_grouping <- which(condition_3, arr.ind = TRUE)
  groupinglist[["low_confidence_grouping"]] <- low_confidence_grouping
  groupinglist[["medium_confidence_grouping"]] <- medium_confidence_grouping
  groupinglist[["high_confidence_grouping"]] <- high_confidence_grouping
  return(groupinglist)
}

# #confidence_groups<-give_featureidentities(rand_matrix)

# #ids <- paste0(rand_data$Clustering_1, rand_data$Clustering_2,rand_data$Clustering_3,rand_data$Clustering_4,rand_data$Clustering_5,rand_data$Clustering_6,rand_data$Clustering_7,rand_data$Clustering_8,rand_data$Clustering_9,rand_data$Clustering_10,rand_data$Clustering_11 )
# ids<-paste0(rand_data$Clustering_5,rand_data$Clustering_9)
# ids<-paste0(rand_data$Clustering_7,rand_data$Clustering_8,rand_data$Clustering_9,rand_data$Clustering_10,rand_data$Clustering_11,rand_data$Clustering_12,rand_data$Clustering_13,rand_data$Clustering_14,rand_data$Clustering_15,rand_data$Clustering_16)

###############
ID_creation <- function(df) {
  result_list <- list() # Initialize an empty list to store the results
  if (nrow(df) == 0) {
    return(result_list) # Return an empty list if the dataframe is empty
  }

  # Iterate through the rows of the dataframe
  for (i in 1:nrow(df)) {
    row_i <- df[i, ] # Get the i-th row
    pair <- unlist(row_i) # Convert the row to a vector

    # Check if this pair overlaps with any of the existing results
    is_overlapping <- FALSE
    for (j in seq_along(result_list)) {
      combined_pair <- c(result_list[[j]], pair)

      # Check for duplicates (overlapping)
      if (length(combined_pair) != length(unique(combined_pair))) {
        result_list[[j]] <- unique(combined_pair) # Update the result
        is_overlapping <- TRUE
        break
      }
    }

    # If it doesn't overlap, create a new entry in the result list
    if (!is_overlapping) {
      result_list <- append(result_list, list(pair))
    }
  }

  return(result_list)
}
# ID_list<-ID_creation(confidence_groups[["high_confidence_grouping"]])
# ID_list2<-ID_creation(confidence_groups[["medium_confidence_grouping"]])
# ID_list3<-ID_creation(confidence_groups[["low_confidence_grouping"]])
# full_id_list<-c(ID_list,ID_list2,ID_list3)
###############
# Function to calculate Hamming distance between two integer vectors
hamming_distance_calc <- function(vector1, vector2) {
  sum(charToRaw(vector1) != charToRaw(vector2))
}
###############
Make_cluster_id_df <- function(data, ids) {
  cellcluster <- cbind(data$CellID, ids)
  # Create a data frame to store UUIDs and numeric strings
  df <- data.frame(UUID = character(), Cluster_characterising_ids = character(), stringsAsFactors = FALSE)
  cat("dataframe made")
  # Iterate through rows and add UUIDs and numeric strings to the data frame
  for (i in 1:nrow(cellcluster)) {
    uuid <- cellcluster[i, 1]

    numeric_section <- cellcluster[i, 2]

    df <- rbind(df, data.frame(UUID = uuid, Cluster_characterising_ids = as.vector(numeric_section), stringsAsFactors = FALSE))
  }
  cat("dataframe values added")

  return(df)
}



###########################
# hamming amalgamation data
Do_hamming_amalgamation <- function(df, CharVectors) {
  # Iterate through the dataframe and replace character vectors
  for (i in 1:nrow(df)) {
    char_vector <- df$Cluster_characterising_ids[i]
    min_distance <- Inf
    closest_char_vector <- NULL

    # Find the closest character vector in the vector
    for (cv in CharVectors) {
      distance <- sum(sapply(char_vector, function(x) hamming_distance_calc(x, cv)))
      if (distance < min_distance) {
        min_distance <- distance
        closest_char_vector <- cv
      }
    }

    # Update the dataframe with the closest character vector
    df$Cluster_characterising_ids[i] <- closest_char_vector
  }


  doof <- df

  doof$idtitles <- doof$Cluster_characterising_ids

  # Convert 'ids' to a factor
  doof$Cluster_characterising_ids <- factor(doof$Cluster_characterising_ids)

  # Convert the factor levels to integer values
  doof$Cluster_characterising_ids <- as.integer(doof$Cluster_characterising_ids)

  return(doof)
}

# datafrumb<-cluster_characterising(data = data,ids = ids)
####################
cluster_characterising <- function(data = data, ids = ids) {
  id_dataframe <- Make_cluster_id_df(data = data, ids = ids)
  seeds <- as.vector(Make_hamming_seeds(id_dataframe, data))
  output <- Do_hamming_amalgamation(df = id_dataframe, CharVectors = seeds)
  return(output)
}

####################
# a function to make consensus images of hamming amalgamated data
hamming_amalgamate_Clustering <- function(data = data, ID_list = ID_list, rand_data = rand_data, outlinedata = outlinedata) {
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

    hamming_consensus <- Make_hamming_consensus_images(outlinedata = outlinedata, cluster = hamming_cluster)

    # Determine the range of numbers

    hamming_consensus <- hamming_consensus + labs(title = paste0("Morphotypes from clusterings", " ", paste(titleX, collapse = ", ")))

    hamming_consensus_list[[i]] <- list(graph1 = hamming_consensus)
  }
  cat("done")

  return(hamming_consensus_list)
}
