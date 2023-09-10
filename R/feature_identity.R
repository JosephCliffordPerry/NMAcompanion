give_featureidentities<- function(rand_matrix){
rand_df<- as.data.frame(rand_matrix)
  # Create a logical matrix for each condition
  condition_1 <- rand_matrix >= 0.5 & rand_matrix <= 0.6
  condition_2 <- rand_matrix > 0.6 & rand_matrix <= 0.7
  condition_3 <- rand_matrix > 0.7

  low_confidence_grouping<- which(condition_1, arr.ind = TRUE)
  medium_confidence_grouping<- which(condition_2, arr.ind = TRUE)
  high_confidence_grouping<- which(condition_3, arr.ind = TRUE)


}




convert_groups_to_gross_phenotypes <- function(grouping, centroid_table,rand_data) {
  df<-as.data.frame(unlist(centroid_table))
   for (i in 1:length(grouping)) {
    gn <- grouping[i, 1]

    clomsters <- df %>%
      filter(startsWith(names(.), as.character(gn)))

  }

}
#convert_groups_to_gross_phenotypes(grouping = high_confidence_grouping,centroid_table = ct,rand_data =rand_data )































clusterstrings<-paste0(rand_data$Clustering_1,rand_data$Clustering_2,rand_data$Clustering_3,rand_data$Clustering_4,rand_data$Clustering_5,rand_data$Clustering_6,rand_data$Clustering_7,rand_data$Clustering_8,rand_data$Clustering_9,rand_data$Clustering_10,rand_data$Clustering_11)
cluster_identities<-cbind(data$CellID,clusterstrings)

For (i in length(cluster_identities)) {
  cluster_identities[clusterstrings]
}









df <- data.frame(UUID = character(), Numeric = character(), stringsAsFactors = FALSE)

# Iterate through rows and add UUIDs and numeric strings to the data frame
for (i in 1:nrow(cluster_identities)){
  uuid <- cluster_identities[i, 1]
  numeric_section <- gsub("[^0-9]", "", cluster_identities[i, 2])
  df <- rbind(df, data.frame(UUID = uuid, Numeric = numeric_section, stringsAsFactors = FALSE))
}

# Group UUIDs by identical numeric strings using the dplyr package
library(dplyr)

doof <- df %>%
  group_by(Numeric) %>%
  summarize(UUIDs = list(UUID))

# Print the result
print(result)

##########################

hasSingleDigitDifference <- function(num1, num2) {
  # Check if the lengths of the vectors are the same
  if (length(num1) != length(num2)) {
    return(FALSE)
  }

  # Convert the character vectors to numeric
  num1 <- as.numeric(strsplit(num1, "")[[1]])
  num2 <- as.numeric(strsplit(num2, "")[[1]])

  # Check if there is exactly one differing digit
  diff_count <- sum(num1 != num2)

  return(diff_count == 1)
}
u



hasSingleDigitDifference(num1 = uniqueNumerics[1],num2 = uniqueNumerics[5])
# Load the dplyr package for data manipulation
library(dplyr)
# Function to merge rows with a single-digit difference
mergeRows <- function(df) {
  # Create an empty dataframe to store merged rows
  merged_df <- data.frame(Numeric = character(0), UUIDs = list())

  # Iterate through each row in the dataframe
  for (i in 1:(nrow(df) - 1)) {
    for (j in (i + 1):nrow(df)) {
      num1 <- df$Numeric[i]
      num2 <- df$Numeric[j]

      # Check if the numeric values have a single-digit difference
      if (hasSingleDigitDifference(num1, num2)) {
        # Merge the UUIDs from both rows
        merged_uuids <- c(df$UUIDs[[i]], df$UUIDs[[j]])

        # Create a new merged row
        merged_row <- data.frame(Numeric = num1, UUIDs = list(merged_uuids), stringsAsFactors = FALSE)

        # Add the merged row to the merged_df
        merged_df <- bind_rows(merged_df, merged_row)

        # Print the information about the merge
        cat(i, "/", nrow(df), "\n")
      }
    }
  }

  return(merged_df)
}

# Apply the mergeRows function to your dataframe
merged_data <- mergeRows(result)

# Print the resulting dataframe with merged rows
print(merged_data)









# Function to merge rows with a single-digit difference
mergeRows <- function(doof) {
  df<-doof
  # Create an empty dataframe to store merged rows
  merged_df <- data.frame(Numeric = character(0), UUIDs = list())

  # Iterate through each row in the dataframe
  for (i in 1:(nrow(df) - 1)) {
    for (j in (i + 1):nrow(df)) {
      num1 <- df$Numeric[i]
      num2 <- df$Numeric[j]

      # Check for missing values
      if (is.na(num1) || is.na(num2)) {
        next  # Skip this iteration if there are missing values
      }

      # Check if the numeric values have a single-digit difference
      if (hasSingleDigitDifference(num1, num2)) {
        # Merge the UUIDs from both rows
        merged_uuids <- c(df$UUIDs[[i]], df$UUIDs[[j]])

        # Create a new merged row
        merged_row <- data.frame(Numeric = num1, UUIDs = list(merged_uuids), stringsAsFactors = FALSE)

        # Add the merged row to the merged_df
        merged_df <- bind_rows(merged_df, merged_row)

        # Print the information about the merge
        cat(i, "/", nrow(df), "\n")

        # Remove the merged rows from the original dataframe
        df <- df[-c(i, j), ]
      }
    }
  }

  return(list(merged_df, df))
}

# Usage
result <- mergeRows(result)
merged_data <- result[[1]]
remaining_data <- result[[2]]







df<-doof


# Function to merge rows with a single-digit difference
mergeRows <- function(doof) {
  # Create an empty dataframe to store merged rows
  merged_df <- data.frame(Numeric = character(0), UUIDs = list())
  df<-doof
  # Iterate through each row in the dataframe
  i <- 1  # Initialize i
  while (i <= nrow(df)) {
    j <- i + 1  # Initialize j to the next row

    # Ensure j is within bounds
    if (j > nrow(df)) {
      break  # No more rows to compare, exit the loop
    }

    num1 <- df$Numeric[i]
    num2 <- df$Numeric[j]

    # Check for missing values
    if (is.na(num1) || is.na(num2)) {
      i <- i + 1  # Skip this row and move to the next
      next
    }

    # Check if the numeric values have a single-digit difference
    if (hasSingleDigitDifference(num1, num2)) {
      # Merge the UUIDs from both rows
      merged_uuids <- c(df$UUIDs[[i]], df$UUIDs[[j]])

      # Create a new merged row
      merged_row <- data.frame(Numeric = num1, UUIDs = list(merged_uuids), stringsAsFactors = FALSE)

      # Add the merged row to the merged_df
      merged_df <- bind_rows(merged_df, merged_row)

      # Remove the merged rows from the original dataframe
      df <- df[-c(i, j), ]

      # Reset i to 1 to check from the beginning
      i <- 1
    } else {
      i <- i + 1  # Move to the next row
    }
  }

  return(list(merged_df, df))
}

# Usage
dorta <- mergeRows(result)
merged_data <- result[[1]]
remaining_data <- result[[2]]









# Function to merge rows with a single-digit difference
mergeRows <- function(doof) {
  df <- doof
  # Create an empty dataframe to store merged rows
  merged_df <- data.frame(Numeric = character(0), UUIDs = list())

  # Initialize index variable
  i <- 1
  Lungus<-list()
  while (i <= nrow(df)) {
    num1 <- df$Numeric[i]

    # Find rows that have a single-digit difference with num1
    similar_rows <- which(sapply(df$Numeric[-i], function(num2) hasSingleDigitDifference(num1, num2)))

    if (length(similar_rows) > 0) {
      # Merge the UUIDs from all similar rows
      merged_uuids <- c(df$UUIDs[[i]], unlist(df$UUIDs[similar_rows]))

      # Create a new merged row
      merged_row <- data.frame(Numeric = num1, UUIDs = list(merged_uuids), stringsAsFactors = FALSE)

      # Add the merged row to the merged_df
      Lungus[i] <-c(merged_row,Lungus[i])
      # Show progress
      cat(i, "/", nrow(df), "\n")
      # Remove merged rows from the original dataframe
      df <- df[-c(i, similar_rows), ]
    } else {
      # If no similar rows found, move to the next row
      i <- i + 1
    }
  }

  # Return the merged_df as a list
  return(Lungus)
}

# Usage
merged_data <- mergeRows(result)

