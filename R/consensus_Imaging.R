# Consensus imaging

library(ggplot2)

# outlinedata<- data %>% dplyr::select(starts_with("Outline_Oriented"))
#
# outlinedata1<- outlinedata[1, ]
# bungoy<-t(outlinedata1 %>% dplyr::select(starts_with("Outline_OrientedCoordinates_Y")))
# dungox<-t(outlinedata1%>% dplyr::select(starts_with("Outline_OrientedCoordinates_X")))
# result_df <- data.frame(Column1 = bungoy, Column2 = dungox)
#
#
# ggplot(result_df, aes(result_df$X1,result_df$X1.1))+geom_polygon()
#
# #######################
MakeConsensus <- function(outlinedata) {
  Outliney <- t(outlinedata %>% dplyr::select(starts_with("Outline_OrientedCoordinates_Y")))
  Outlinex <- t(outlinedata %>% dplyr::select(starts_with("Outline_OrientedCoordinates_X")))
  meanOutlineY <- rowMeans(Outliney)
  meanOutlineX <- rowMeans(Outlinex)
  polygon_df <- data.frame(Column1 = meanOutlineY, Column2 = meanOutlineX)
  consensus <- ggplot(polygon_df, aes(polygon_df$Column2, polygon_df$Column1)) +
    theme_minimal() +
    geom_polygon(fill = fill)
  return(consensus)
}
####################
MakeConsensusdf <- function(outlinedata) {
  Outliney <- t(outlinedata %>% dplyr::select(starts_with("Outline_OrientedCoordinates_Y")))
  Outlinex <- t(outlinedata %>% dplyr::select(starts_with("Outline_OrientedCoordinates_X")))
  meanOutlineY <- rowMeans(Outliney)
  meanOutlineX <- rowMeans(Outlinex)
  polygon_df <- data.frame(Column1 = meanOutlineY, Column2 = meanOutlineX)
  return(polygon_df)
}
####################
make_cluster_consensus <- function(cluster, outlinedata) {
  outline_clusters <- cbind(outlinedata, cluster$Clustering_file)


  title <- paste(names(cluster[1:(length(cluster)-1)]), collapse = " ")

  words <- str_extract_all(title, "\\b\\w+\\b")[[1]]
  # Extract words and numbers
  extracted2 <- gsub("_\\d+", "", words[1])
  words2 <- unique(gsub("_", " ", extracted2))

  numbers <- gsub("[^0-9]+", " ", title)

  # Split the numbers by space
  number_parts <- as.numeric(unlist(strsplit(numbers, " ")))

  # Determine the range of numbers
  number_range <- paste(min(number_parts, na.rm = TRUE), max(number_parts, na.rm = TRUE), sep = ":")
  if (any(grepl("\\d+:\\d+", number_range))) {
    # Create new title
    title <- paste0(words2, sep = " ", number_range, recycle0 = TRUE)

path_data_xlist<- list()
path_data_ylist<- list()
  for (i in 2:length(number_parts)) {


  # Construct the column names to select
  selected_Xcolumns <- paste0("Outline_OrientedCoordinates_X_", number_parts[i])
  selected_ycolumns <- paste0("Outline_OrientedCoordinates_Y_", number_parts[i])

  # Filter the columns based on the constructed names
  path_datax <- outlinedata[selected_Xcolumns]
  path_datay <- outlinedata[selected_ycolumns]

  path_data_xlist[[i]]<- path_datax
  path_data_ylist[[i]]<- path_datay
  }
path_dataxdf<-as.data.frame(path_data_xlist[2:length(number_parts)])
path_dataydf<-as.data.frame(path_data_ylist[2:length(number_parts)])
path_datadf<- cbind(path_dataxdf,path_dataydf)
path_clusters <- cbind(path_datadf, cluster$Clustering_file)
  a <- list()
  a2<- list()
  # Calculate the number of rows in each facet
  facet_counts <- as.data.frame(table(cluster$Clustering_file))
  facet_count_vector<-facet_counts[[2]]
  for (j in 1:max(outline_clusters$`cluster$Clustering_file`)) {
    A1 <- outline_clusters %>% filter(`cluster$Clustering_file` == j)
    A2 <- path_clusters %>% filter(`cluster$Clustering_file` == j)
     consensus_df <- MakeConsensusdf(A1)
    path_consensus_df<-MakeConsensusdf(A2)
    # Add a facet column to the consensus_df and pathdf
    consensus_df$facet <- j
    path_consensus_df$facet <- j
    # Add facet count column
    consensus_df$facet_count <- facet_count_vector[j]

    a[[j]] <- consensus_df
    a2[[j]] <- path_consensus_df
  }



  # Combine the individual consensus dataframes into one faceted dataframe
  faceted_df <- dplyr::bind_rows(a)
  faceted_path_df<- dplyr::bind_rows(a2)

  # Determine how many times you need to repeat faceted_path_df
  repeat_factor <- ceiling(nrow(faceted_df) / nrow(faceted_path_df))

  # Repeat faceted_path_df using the repeat function from base R
  faceted_path_df<- faceted_path_df[rep(seq_len(nrow(faceted_path_df)), each = repeat_factor), ]
  #make sure repeats don't overshoot
  faceted_path_df<- faceted_path_df[1:nrow(faceted_df),]

  colourfactor <- factor(faceted_df$facet)
  consensus <- ggplot(faceted_df, aes(Column2, Column1, fill = colourfactor)) +
    geom_polygon() +
    geom_text(aes(x = Inf, y = Inf, label = facet_count), vjust = 1.5,hjust = 1.5) +
    geom_path(data =faceted_path_df, aes(x = faceted_path_df$Column1,y = faceted_path_df$Column2),linewidth = 1.5)+
    facet_wrap(faceted_df$facet) +
    theme_minimal()+
    coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")

  title <- paste0(words2, sep = " ", number_range, recycle0 = TRUE)
} else {
  title <- paste0(words2, sep = " ", recycle0 = TRUE)

  a <- list()

  # Calculate the number of rows in each facet
  facet_counts <- as.data.frame(table(cluster$Clustering_file))
  facet_count_vector<-facet_counts[[2]]
  for (j in 1:max(outline_clusters$`cluster$Clustering_file`)) {
    A1 <- outline_clusters %>% filter(`cluster$Clustering_file` == j)

    consensus_df <- MakeConsensusdf(A1)

    # Add a facet column to the consensus_df
    consensus_df$facet <- j

    # Add facet count column
    consensus_df$facet_count <- facet_count_vector[j]

    a[[j]] <- consensus_df

  }



  # Combine the individual consensus dataframes into one faceted dataframe
  faceted_df <- dplyr::bind_rows(a)

  consensus <- ggplot(faceted_df, aes(Column2, Column1, fill = colourfactor)) +
    geom_polygon() +
    geom_text(aes(x = Inf, y = Inf, label = facet_count), vjust = 1.5,hjust = 1.5) +
    facet_wrap(faceted_df$facet) +
    theme_minimal()+
    coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")

}

  return(consensus)
}
#######################################
Make_hamming_consensus_images <- function(cluster, outlinedata) {
  idtitles<-unique(cluster$idtitles)
  custom_labeller <- function(variable, value) {
    return(idtitles[as.integer(value)])
  }
  outline_clusters <- cbind(outlinedata, cluster$Clustering_file)

  a <- list()
  # Calculate the number of rows in each facet
  facet_counts <- as.data.frame(table(cluster$Clustering_file))
  facet_count_vector<-facet_counts[[2]]
  for (j in 1:max(outline_clusters$`cluster$Clustering_file`)) {
    A1 <- outline_clusters %>% filter(`cluster$Clustering_file` == j)
    consensus_df <- MakeConsensusdf(A1) # Assuming this function creates the consensus dataframe

    # Add a facet column to the consensus_df
    consensus_df$facet <- j

    consensus_df$facet_count <- facet_count_vector[j]

    a[[j]] <- consensus_df
  }



  # Combine the individual consensus dataframes into one faceted dataframe
  faceted_df <- dplyr::bind_rows(a)

  colourfactor <- factor(faceted_df$facet)
  consensus <- ggplot(faceted_df, aes(Column2, Column1, fill = colourfactor)) +
    geom_polygon() +
    geom_text(aes(x = Inf, y = Inf, label = facet_count), vjust = 1.5,hjust = 1.5) +
    facet_wrap(faceted_df$facet,labeller =  custom_labeller) +
    theme_minimal()+
    coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")




  return(consensus)
}



#######################################
make_consensus_for_all_clusters <- function(clusters, outlinedata) {
  clusterconsensuses <- list()
  for (i in 1:length(clusters)) {
    graph <- make_cluster_consensus(cluster = clusters[[i]], outlinedata = outlinedata)
    clusterconsensuses[[i]] <- graph
  }
  return(clusterconsensuses)
}
