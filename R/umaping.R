#' Performs umaps on all of the data

Umaping <- function(originaldata, angle_data, diameter_data, radius_data) {
  numdata <- dplyr::select_if(originaldata, is.numeric)
  print("umap1 started")
  umapo <- umap(numdata, preserve.seed = TRUE)
  print("umap1 done")
  print("umap2 started")
  angleumap <- umap(angle_data, preserve.seed = TRUE)
  print("umap2 done")
  print("umap3 started")
  diameterumap <- umap(diameter_data, preserve.seed = TRUE)
  print("umap3 done")
  print("umap4 started")
  radiusumap <- umap(radius_data, preserve.seed = TRUE)
  print("umap4 done")
  umaplist <- list(umapo, angleumap, diameterumap, radiusumap)
  return(umaplist)
}
