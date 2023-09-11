give_featureidentities<- function(rand_matrix){
rand_df<- as.data.frame(rand_matrix)
groupinglist<-list()
  # Create a logical matrix for each condition
  condition_1 <- rand_matrix >= 0.5 & rand_matrix <= 0.6
  condition_2 <- rand_matrix > 0.6 & rand_matrix <= 0.7
  condition_3 <- rand_matrix > 0.7

  low_confidence_grouping<- which(condition_1, arr.ind = TRUE)
  medium_confidence_grouping<- which(condition_2, arr.ind = TRUE)
  high_confidence_grouping<- which(condition_3, arr.ind = TRUE)
  groupinglist[["low_confidence_grouping"]]<-low_confidence_grouping
  groupinglist[["medium_confidence_grouping"]]<-medium_confidence_grouping
  groupinglist[["high_confidence_grouping"]]<-high_confidence_grouping
return(groupinglist)
}

#chumbo<-give_featureidentities(rand_matrix)
#numbers <- as.numeric(gsub("[^0-9.]+", "", ct[["1whole_dataset.1whole_dataset_.1"]]))

ids <- paste0(rand_data$Clustering_1, rand_data$Clustering_2,rand_data$Clustering_3,rand_data$Clustering_4,rand_data$Clustering_5,rand_data$Clustering_6,rand_data$Clustering_7,rand_data$Clustering_8,rand_data$Clustering_9,rand_data$Clustering_10,rand_data$Clustering_11 )
cellcluster<- cbind(data$CellID,ids)
## Create a data frame to store UUIDs and numeric strings
df <- data.frame(UUID = character(), Numeric = character(), stringsAsFactors = FALSE)

# Iterate through rows and add UUIDs and numeric strings to the data frame
for (i in 1:nrow(cellcluster)) {
  uuid <- cellcluster[i, 1]
  numeric_section <- gsub("[^0-9]", "", cellcluster[i, 2])
  df <- rbind(df, data.frame(UUID = uuid, Numeric = numeric_section, stringsAsFactors = FALSE))
}

# Group UUIDs by identical numeric strings using the dplyr package
#library(dplyr)
#library(tidyr)

#result <- df %>%
 #group_by(Numeric) %>%
 # summarize(UUIDs = list(UUID))



###################


#Randids



#ids <- paste0( rand_data$Clustering_1,rand_data$Clustering_2,rand_data$Clustering_4,rand_data$Clustering_5,rand_data$Clustering_6,rand_data$Clustering_11 )
#cellcluster1<- as.data.frame(cbind(data$CellID,ids))

#####################
#take big groups

#filtered_df <- result %>%
 # filter(lengths(UUIDs) > 50)
#

#unested_df<- filtered_df %>%
#  unnest(UUIDs)

#doof<-unested_df
##################
CharVectors<-filtered_df$Numeric

###########
#hamming amalgamate data

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


##################
#make a clustering
#reversed_doof <- doof[, ncol(doof):1]

# Convert 'ids' to a factor
  doof$Numeric<- factor(doof$Numeric)

# Convert the factor levels to integer values
 doof$Numeric <- as.integer(doof$Numeric)

write.table(doof, file = "D:/Joe_Perry_2022-2023_sperm_images/NMA_companion_ehibitdata/chumbus7.txt", sep = "\t", quote = FALSE, row.names = FALSE)
