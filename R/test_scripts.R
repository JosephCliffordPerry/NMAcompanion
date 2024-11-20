#testsuite scripts#
~load("~data/NMA_toy_dataset.rda")
run_tests<-FALSE
if(run_tests){
a<-get_regions_of_interest(data)
if((length(a)==10)==FALSE){
  paste0("Error ROI detection")
}

b<-Cluster_ROI_list(a)
if((length(b)==10)==FALSE){
  paste0(b)
}
c<-find_contious_clusters(data)
b<-append(b,c)
d<-Amalgamate_morphological_features(data,b)
if((length(c)==5)==FALSE){
  paste0("Error in hamming amalgamation")
}
#Graph_clustered_ROIs has many errors
e<-Graph_clustered_ROIs(b,rawdata = rawdata)
}
