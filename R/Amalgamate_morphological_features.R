Amalgamate_morphological_features<-function(data,clusters){
# calculate rand index matrix
rand_data <- make_randindex_data(data = data, clusters = clusters)
rand_matrix <- calculate_rand_indexes(rand_data)
# calculate IDs from rand index confidence groups
confidence_groups <- give_featureidentities(rand_matrix)
ID_list <- ID_creation(confidence_groups[["high_confidence_grouping"]])
ID_list2 <- ID_creation(confidence_groups[["medium_confidence_grouping"]])
ID_list3 <- ID_creation(confidence_groups[["low_confidence_grouping"]])
full_id_list <- c(ID_list, ID_list2, ID_list3)
# make consensus images of hamming amalgamated confidence grouping ids
hamming_consensus <- hamming_amalgamate_Clustering(data = data, rand_data = rand_data, ID_list = full_id_list, outlinedata = outlinedata)
return(hamming_consensus)
}
