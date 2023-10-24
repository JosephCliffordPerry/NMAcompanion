# library(devtools)
# load_all()
# rawdata<-NMA_Testdata
#
# rawdata<-read.table("Testing_dataset_stats_outlines.txt",header = TRUE, sep = "\t")
# library(NMAcompanion)
# targeted_profile_analysis()
# library(dplyr)
#
#
#
# df<- id_dataframe
#
# simple size based seeds
# Make_hamming_seeds<-function(df, data){
#   #seed id creation
#   result <- df %>%
#     group_by(Cluster_characterising_ids) %>%
#     summarize(UUIDs = list(UUID))
#   result <- result %>%
#     mutate(UUIDs_length = sapply(UUIDs, length))
#  orderedresult <- result[order(-result$UUIDs_length),]
#
#
#
#  output1 <- as.vector(orderedresult[,1])
#  output1<- unlist(output1)
#    return(output1)
#    }

# distance based seeds that cluster everything
########################
# Make_hamming_seeds<-function(df){
#   #seed id creation
#   result <- df %>%
#     group_by(Cluster_characterising_ids) %>%
#     summarize(UUIDs = list(UUID))
#
#   # filternum <- 1
#   # while (nrow(filtered_df) > 350) {
#   #   filtered_df <- result %>%
#   #     filter(lengths(UUIDs) > filternum)
#   #   filternum <- filternum + 1
#   # }
#   #make character Vectors
#   CharVectors <- result$Cluster_characterising_ids
#
#   n <- length(CharVectors)
#   result_matrix <- matrix(NA, nrow = n, ncol = n)
#
#   for (i in 1:(n - 1)) {
#     for (j in (i + 1):n) {
#       # Extract the vectors from the data frame
#       char1 <- CharVectors[i]
#       char2 <- CharVectors[j]
#
#
#
#       hamming_distance <- hamming_distance_calc(char1, char2)
#       result_matrix[i, j] <- hamming_distance
#       # result_matrix[j - 1, i - 1] <- rand_idx
#       # Print progress
#       cat("Calculated hamming distance", i, "vs", j, "\n")
#     }
#   }
#   unique_values<- 1:17
#   hamming_distance_threshold <- 1
#   while (length(unique_values) >= 16) {
#     hamming_threshold_condition <- result_matrix <= hamming_distance_threshold
#     similar_ids <- which(hamming_threshold_condition, arr.ind = TRUE)
#     vector_donky <- CharVectors
#     # Replace values based on the replacement matrix
#     for (i in 1:nrow(similar_ids)) {
#       target_idx <- similar_ids[i, 1]
#       source_idx <- similar_ids[i, 2]
#       vector_donky[target_idx] <- vector_donky[source_idx]
#     }
#
#     hamming_distance_threshold <- hamming_distance_threshold + 1
#     cat(hamming_distance_threshold, "_")
#
#     # Extract unique values
#     unique_values <- unique(vector_donky)
#   }
#
#
#   CharVectors <- unique_values
#   return(CharVectors)}


#########################
# # hamming seeds with a combination of both size based
# #filtering and amalgamating similar groups
# Make_hamming_seeds<-function(df){
#
# #seed id creation
# #filters dataset by the ids to find their prevelance
# result <- df %>%
#   group_by(Cluster_characterising_ids) %>%
#   summarize(UUIDs = list(UUID))
# #turning that into a number
# result <- result %>%
#   mutate(UUIDs_length = sapply(UUIDs, length))
# orderedresult <- result[order(-result$UUIDs_length),]

# ids <- orderedresult$Cluster_characterising_ids
# hamming_threshold <- 0
#
# while (length(ids) > 16) {
#   #seed position
#   seedpos <- 1
#   #vector to store new ids
#   newids <- integer(length(ids))
#   while (seedpos < length(ids)) {
#     seedseed <- ids[seedpos]
#
#     for (i in 1:length(ids)) {
#       seedcomp <- ids[i]
#       ham <- hamming_distance_calc(seedcomp, seedseed)
#
#       if (ham <= hamming_threshold) {
#         newids[i] <- seedseed
#       } else {
#         newids[i] <- seedcomp  # Keep the original value
#       }
#     }
#     seedpos <- seedpos + 1
#     ids <- unique(newids)
#   }
#
#   hamming_threshold <- hamming_threshold + 1
#
# }
#
# return(ids)}














# hamming seeds with a combination of both size based
# filtering and then filtering the remaining groups
Make_hamming_seeds <- function(df, data) {
  # seed id creation
  # filters dataset by the ids to find their prevelance
  result <- df %>%
    group_by(Cluster_characterising_ids) %>%
    summarize(UUIDs = list(UUID))
  # turning that into a number
  result <- result %>%
    mutate(UUIDs_length = sapply(UUIDs, length))
  orderedresult <- result[order(-result$UUIDs_length), ]
  # checks if the dataset needs condensing
  if (nrow(orderedresult) >= 16) {
    # amount of numbers needed to cover 25% of the cells in the dataset
    dataset_covered <- 1
    # amount of data
    datanum <- 0
    while (datanum < nrow(data) / 4) {
      datanum <- sum(orderedresult[1:dataset_covered, 3])
      dataset_covered <- dataset_covered + 1
    }
    # ids that represent the largest groups covering 25% of cells in the dataset
    CharVectors <- unlist(orderedresult[1:(dataset_covered - 1), 1])
    # a matrix is made
    n <- length(CharVectors)
    result_matrix <- matrix(NA, nrow = n, ncol = n)

    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        # Extract the vectors from the data frame
        char1 <- CharVectors[i]
        char2 <- CharVectors[j]



        hamming_distance <- hamming_distance_calc(char1, char2)
        result_matrix[i, j] <- hamming_distance
        # result_matrix[j - 1, i - 1] <- rand_idx
        # Print progress
        cat("Calculated hamming distance", i, "vs", j, "\n")
      }
    }

    unique_values <- 1:17
    hamming_distance_threshold <- 1
    while (length(unique_values) >= 16) {
      hamming_threshold_condition <- result_matrix <= hamming_distance_threshold
      similar_ids <- which(hamming_threshold_condition, arr.ind = TRUE)
      vector_donky <- CharVectors
      # Replace values based on the replacement matrix
      for (i in 1:nrow(similar_ids)) {
        target_idx <- similar_ids[i, 1]
        source_idx <- similar_ids[i, 2]
        vector_donky[target_idx] <- vector_donky[source_idx]
      }

      hamming_distance_threshold <- hamming_distance_threshold + 1
      cat(hamming_distance_threshold, "_")

      # Extract unique values
      unique_values <- unique(vector_donky)
    }


    CharVectors <- unique_values
  } else {
    CharVectors <- unlist(orderedresult[, 1])
  }
  return(CharVectors)
}
