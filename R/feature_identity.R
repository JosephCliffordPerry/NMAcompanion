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
