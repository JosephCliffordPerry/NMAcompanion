% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Amalgamate_morphological_features.R
\name{Amalgamate_morphological_features}
\alias{Amalgamate_morphological_features}
\title{Amalgamate Morphological Features from Clustered Data}
\usage{
Amalgamate_morphological_features(data, clusters)
}
\arguments{
\item{data}{A standard NMA full profiles export}

\item{clusters}{The list of clusters provided by Cluster_ROI_list}
}
\value{
A  list of consensus images representing amalgamated morphological features
based on high, medium, and low clustering confidence groups.
}
\description{
This function processes morphological data by filtering out suspected detection errors
based on extreme angle values, calculating clustering confidence using Rand indices,
and generating consensus morphological representations based on confidence groupings.
}
\details{
The function performs the following steps:
\enumerate{
\item Detects and removes rows with suspected detection errors (angle > 280).
\item Computes Rand indices to assess clustering confidence.
\item Assigns feature based id codes made from clusters
to high, medium, and low confidence groups.
\item Constructs a consensus representation using Hamming distance between
representative feature id codes.
}
}
