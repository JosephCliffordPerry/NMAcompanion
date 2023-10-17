#Consensus imaging

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
MakeConsensus<-function(outlinedata){
  Outliney<-t(outlinedata %>% dplyr::select(starts_with("Outline_OrientedCoordinates_Y")))
  Outlinex<-t(outlinedata%>% dplyr::select(starts_with("Outline_OrientedCoordinates_X")))
  meanOutlineY<-rowMeans(Outliney)
  meanOutlineX<-rowMeans(Outlinex)
  polygon_df <- data.frame(Column1 = meanOutlineY, Column2 =  meanOutlineX)
  consensus<-ggplot(polygon_df, aes(polygon_df$Column2,polygon_df$Column1))+theme_minimal()+geom_polygon(fill = fill)
return(consensus)}
####################
MakeConsensusdf<-function(outlinedata){
  Outliney<-t(outlinedata %>% dplyr::select(starts_with("Outline_OrientedCoordinates_Y")))
  Outlinex<-t(outlinedata%>% dplyr::select(starts_with("Outline_OrientedCoordinates_X")))
  meanOutlineY<-rowMeans(Outliney)
  meanOutlineX<-rowMeans(Outlinex)
  polygon_df <- data.frame(Column1 = meanOutlineY, Column2 =  meanOutlineX)
  return(polygon_df)}
####################
make_cluster_consensus <- function(cluster, outlinedata) {
  outline_clusters <- cbind(outlinedata, cluster$Clustering_file)
  a <- list()

  for (j in 1:max(outline_clusters$`cluster$Clustering_file`)) {
    A1 <- outline_clusters %>% filter(`cluster$Clustering_file` == j)
    consensus_df <- MakeConsensusdf(A1)  # Assuming this function creates the consensus dataframe

    # Add a facet column to the consensus_df
    consensus_df$facet <- j


    a[[j]] <- consensus_df
  }

  # Combine the individual consensus dataframes into one faceted dataframe
  faceted_df <- dplyr::bind_rows(a)
  colourfactor<- factor(faceted_df$facet)
  consensus<-ggplot(faceted_df, aes(Column2,Column1,fill = colourfactor))+geom_polygon()+facet_wrap(faceted_df$facet)+theme_minimal()
  return(consensus)
}


#######################################
make_consensus_for_all_clusters<-function(clusters,outlinedata){
  clusterconsensuses<-list()
   for (i in 1:length(clusters)) {
    graph<-make_cluster_consensus(cluster = clusters[[i]], outlinedata = outlinedata)
    clusterconsensuses[[i]]<-graph
  }
return(clusterconsensuses)
}

