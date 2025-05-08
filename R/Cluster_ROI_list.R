#' Cluster ROI Data from a List of Regions
#'
#' This function performs clustering on a list of Regions of Interest (ROI) using a targeted profile clustering method.
#' It optionally allows further iteration during clustering and removes any clusters that contain error messages, such as missing profiles.
#'
#' @param roi_list A list of datasets, each representing a Region of Interest (ROI), to be clustered.
#' @param allow_further_itteration Logical; if \code{TRUE}, the clustering process may perform additional iterative refinement.
#'
#' @return A list of clustered ROI data, excluding any entries that failed due to missing profiles or errors.
#'
#' @details
#' The function internally uses \code{targeted_profile_clusterer()} to cluster the input ROI datasets.
#' It checks for and removes any cluster outputs that contain error messages like "no profiles" to ensure the returned list only includes valid clusters.
#'
#' @seealso \code{\link{targeted_profile_clusterer}}
#'
#' @examples
#' \dontrun{
#' rois <- list(roi1, roi2, roi3)
#' clusters <- Cluster_ROI_list(rois, allow_further_itteration = TRUE)
#' }
#'
#' @export

Cluster_ROI_list<-function(roi_list,allow_further_itteration = FALSE){
clusters<- targeted_profile_clusterer(selected_datasets = roi_list,allow_further_itteration = allow_further_itteration)
for (i in length(clusters):1) {
  # Check if any element in clusters[[name]] contains "error"
  if (any(grepl("no profiles", clusters[[i]], ignore.case = TRUE))) {
    # Print message for section removed
    cat("Section removed due to error:", names(clusters[i]), "\n")

    # Remove the section from the list
    clusters[[i]] <- NULL
  }
}
return(clusters)
}
