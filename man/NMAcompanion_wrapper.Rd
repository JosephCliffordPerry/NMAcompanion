% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pipeline_wrapper.R
\name{NMAcompanion_wrapper}
\alias{NMAcompanion_wrapper}
\title{A NMAcompanion_wrapper that runs the entire pipeline}
\usage{
NMAcompanion_wrapper(path_to_export)
}
\arguments{
\item{path_to_export}{A string giving the file path to the tab-delimited NMA export file.}
}
\value{
A named list with six elements:
\describe{
\item{\code{data}}{Raw dataset read via \code{\link{read_NMA_export}}}
\item{\code{regions_of_interest}}{Regions of interest found using \code{\link{get_regions_of_interest}}}
\item{\code{clustered_rois}}{Clustered ROIs returned by \code{\link{Cluster_ROI_list}}, with continuous clusters appended from \code{\link{find_contious_clusters}}}
\item{\code{continuous_clusters}}{Output of \code{\link{find_contious_clusters}}}
\item{\code{morphological_features}}{Extracted using \code{\link{Amalgamate_morphological_features}}}
\item{\code{cluster_graph}}{Graph generated by \code{\link{Graph_clustered_ROIs}}}
}

The function performs the following steps:
\enumerate{
\item Loads the dataset from a path to a .txt file using \code{\link{read_NMA_export}}
\item Identifies regions of interest via \code{\link{get_regions_of_interest}}
\item Clusters regions using \code{\link{Cluster_ROI_list}} and finds continuous clusters via \code{\link{find_contious_clusters}}
\item Extracts morphological features using \code{\link{Amalgamate_morphological_features}}
\item Generates graphs with \code{\link{Graph_clustered_ROIs}}
}
}
\description{
Reads a tab-delimited nuclear morphology analysis (NMA) export file
and runs a full analysis of morphology data.
}
\examples{
\dontrun{
data <- NMAcompanion_wrapper("data/NMA_output.txt")
}

}
