#testsuite scripts#
#load("~data/NMA_toy_dataset.rda")
rawdata<-data
a<-get_regions_of_interest(rawdata)
if((length(a)==10)==FALSE){
  paste0("Error ROI detection")
}

b<-Cluster_ROI_list(a)
if((length(b)==10)==FALSE){
  paste0(b)
}

c<-Amalgamate_morphological_features(rawdata,b)
if((length(c)==5)==FALSE){
  paste0("Error in hamming amalgamation")
}
#Graph_clustered_ROIs has many errors
d<-Graph_clustered_ROIs(b,rawdata = rawdata)
