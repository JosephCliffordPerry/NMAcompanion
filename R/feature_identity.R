#library(dplyr)
#library(tidyr)
#
#
#
give_featureidentities<- function(rand_matrix){
rand_df<- as.data.frame(rand_matrix)
groupinglist<-list()
  # Create a logical matrix for each condition
  condition_1 <- rand_matrix >= 0.5
  condition_2 <- rand_matrix > 0.6
  condition_3 <- rand_matrix > 0.7

  low_confidence_grouping<- which(condition_1, arr.ind = TRUE)
  medium_confidence_grouping<- which(condition_2, arr.ind = TRUE)
  high_confidence_grouping<- which(condition_3, arr.ind = TRUE)
  groupinglist[["low_confidence_grouping"]]<-low_confidence_grouping
  groupinglist[["medium_confidence_grouping"]]<-medium_confidence_grouping
  groupinglist[["high_confidence_grouping"]]<-high_confidence_grouping
return(groupinglist)
}

# #confidence_groups<-give_featureidentities(rand_matrix)

# #ids <- paste0(rand_data$Clustering_1, rand_data$Clustering_2,rand_data$Clustering_3,rand_data$Clustering_4,rand_data$Clustering_5,rand_data$Clustering_6,rand_data$Clustering_7,rand_data$Clustering_8,rand_data$Clustering_9,rand_data$Clustering_10,rand_data$Clustering_11 )
# #ids<-paste0(rand_data$Clustering_5,rand_data$Clustering_9)
# ids<-paste0(rand_data$Clustering_7,rand_data$Clustering_8,rand_data$Clustering_9,rand_data$Clustering_10,rand_data$Clustering_11,rand_data$Clustering_12,rand_data$Clustering_13,rand_data$Clustering_14,rand_data$Clustering_15,rand_data$Clustering_16)

###############
ID_creation<- function(df) {
  result_list <- list()  # Initialize an empty list to store the results

  # Iterate through the rows of the dataframe
  for (i in 1:nrow(df)) {
    row_i <- df[i, ]  # Get the i-th row
    pair <- unlist(row_i)  # Convert the row to a vector

    # Check if this pair overlaps with any of the existing results
    is_overlapping <- FALSE
    for (j in seq_along(result_list)) {
      combined_pair <- c(result_list[[j]], pair)

      # Check for duplicates (overlapping)
      if (length(combined_pair) != length(unique(combined_pair))) {
        result_list[[j]] <- unique(combined_pair)  # Update the result
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
ID_list<-ID_creation(confidence_groups[["high_confidence_grouping"]])
###############
cluster_characterising<-function(data,ids){

cellcluster<- cbind(data$CellID,ids)
# Create a data frame to store UUIDs and numeric strings
df <- data.frame(UUID = character(), Numeric = character(), stringsAsFactors = FALSE)
cat("dataframe made")
# Iterate through rows and add UUIDs and numeric strings to the data frame
for (i in 1:nrow(cellcluster)) {
  uuid <- cellcluster[i, 1]
 numeric_section <- gsub("[^0-9]", "", cellcluster[i, 2])
  df <- rbind(df, data.frame(UUID = uuid, Numeric = numeric_section, stringsAsFactors = FALSE))
}
cat("dataframe values added")
# # Group UUIDs by identical numeric strings using the dplyr package


result <- df %>%
 group_by(Numeric) %>%
  summarize(UUIDs = list(UUID))


#take big groups

filtered_df <- result %>%
  filter(lengths(UUIDs) > 50)

#make character Vectors
CharVectors<-filtered_df$Numeric

###########
#hamming aglomerate data

# Function to calculate Hamming distance between two character vectors
hamming_distance <- function(str1, str2) {
  sum(charToRaw(str1) != charToRaw(str2))
}

# Iterate through the dataframe and replace character vectors
for (i in 1:nrow(df)) {
  char_vector <- df$Numeric[i]
min_distance <- Inf
  closest_char_vector <- NULL

  # Find the closest character vector in the vector
  for (cv in CharVectors) {
    distance <- sum(sapply(char_vector, function(x) hamming_distance(x, cv)))
    if (distance < min_distance) {
      min_distance <- distance
     closest_char_vector <- cv
    }
  }

  # Update the dataframe with the closest character vector
  df$Numeric[i] <- closest_char_vector
}


doof<-df



# Convert 'ids' to a factor
  doof$Numeric<- factor(doof$Numeric)

# Convert the factor levels to integer values
 doof$Numeric <- as.integer(doof$Numeric)

 return(doof)
 }
#
# #datafrumb<-cluster_characterising(data = data,ids = ids)
####################
#a function to make the cluster dataframes

####################
# #write.table(datafrumb, file = "C:/Users/User/Documents/0stuff/Masters project/2023_02_17WWLL/2023-02-21_11-45-43/hamming_clusters.txt", sep = "\t", quote = FALSE, row.names = FALSE)
