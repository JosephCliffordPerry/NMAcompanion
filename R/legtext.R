Make_region_text<-function(title){
  A<-paste0("This graph shows the overall dataset ran through a umap to reduce dimensionality. The clustering from the multimodal region " , title," is mapped over it")
  B<-paste0("This graph shows the relevant region of the dataset to the cluster dataset ran through a umap to reduce dimensionality. The clustering from the multimodal region " , title," is mapped over it")
  C<-paste0("This graph show each cluster over the relevant profile region of the dataset of ",title)
  D<-paste0("Umap of just the data included in ", title, "with the clustering of ", title, " mapped onto it")
  E<-paste0("consensus images of each cluster of ",title,". Each cluster shows the shape and the number of cells present in it in the upper right corner")
f<-cbind(A,B,C,D,E)
 return(f)
}

