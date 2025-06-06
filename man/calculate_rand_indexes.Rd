% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rand_index_calc.R
\name{calculate_rand_indexes}
\alias{calculate_rand_indexes}
\title{Calculate Pairwise Rand Index Scores Between Clusterings}
\usage{
calculate_rand_indexes(rand_data)
}
\arguments{
\item{rand_data}{A data frame returned by \code{make_randindex_data()}, where each column
(after the first) represents a set of cluster labels.}
}
\value{
A symmetric matrix of Rand Index values comparing each pair of clusterings.
}
\description{
This internal function computes the Rand Index between all pairs of clustering results
provided in the input data frame.
}
\details{
The Rand Index quantifies the similarity between two clusterings by considering all
pairs of elements and counting pairs that are assigned in the same or different clusters
in both clusterings.
}
\seealso{
\code{\link[fossil]{rand.index}} from the \pkg{fossil} package.
}
\keyword{internal}
