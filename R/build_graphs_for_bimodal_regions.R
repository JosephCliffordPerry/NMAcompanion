#' Build Graphs for Bimodal Regions and Clustered Morphological Profiles
#'
#' This function generates a set of plots to visualize clustered morphological profile data
#' (e.g., angle, diameter, or radius), including consensus images and UMAP representations
#' of identified regions of interest (ROIs).
#'
#' @param clusters A list of clustered ROI data, typically generated from a function such as
#'   \code{\link{Cluster_ROI_list}} or \code{\link{find_contious_clusters}}.
#' @param rawdata A data frame containing the raw morphological measurements, including columns
#'   prefixed with "Angle_profile_", "Diameter_profile_", "Radius_profile_", and "Outline_Oriented".
#'
#' @return A list where each element corresponds to a cluster. Each cluster contains:
#' \describe{
#'   \item{\code{graph3}}{A ggplot2 line plot showing the median profile by cluster (angle, diameter, or radius).}
#'   \item{\code{graph4}}{UMAP-based cluster visualization of multimodal ROI regions.}
#'   \item{\code{graph5}}{Consensus outline images generated for each cluster.}
#' }
#'
#' @details
#' This function:
#' \enumerate{
#'   \item Filters raw data using \code{Extreme_angle_detector} to exclude rows with suspected detection errors.
#'   \item Separates the data by measurement type (angle, diameter, radius, outline).
#'   \item Applies clustering and summarizes each cluster by its median profile.
#'   \item Constructs line plots and UMAP visualizations of these clusters.
#'   \item Builds consensus outline images for each cluster.
#' }
#'
#' The output is useful for validating and visually assessing detected morphological subtypes.
#'
#' @importFrom ggplot2 ggplot aes geom_line labs coord_fixed scale_color_discrete theme_minimal geom_polygon geom_text
#' @importFrom stringr str_extract_all
#' @import patchwork
#' @import dplyr
#' @import knitr
#' @importFrom factoextra fviz_nbclust
#' @importFrom factoextra hkmeans
#' @importFrom umap umap
#' @importFrom ggplot2 geom_path
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 scale_color_discrete
#' @importFrom ggplot2 theme
#' @importFrom dplyr filter
<<<<<<< HEAD
# @seealso \code{\link{make_miniumaps}}, \code{\link{make_miniumap_graphlist}},
#   \code{\link{make_consensus_for_all_clusters}}, \code{\link{Extreme_angle_detector}}
=======
#' @seealso \code{\link{make_miniumaps}}, \code{\link{make_miniumap_graphlist}},
#'   \code{\link{make_consensus_for_all_clusters}}, \code{\link{Extreme_angle_detector}}
>>>>>>> 0912b8c5afdc218ee42ead611652f268f142b190
#'
#' @examples
#' \dontrun{
#' graphs <- Graph_clustered_ROIs(clusters = roi_clusters, rawdata = raw_morphology_data)
#' }
#'
#' @export

Graph_clustered_ROIs <- function(clusters, rawdata) {
  #filters datasets
  error_tagged_angle_dataset <- Extreme_angle_detector(data = rawdata)
  data <- filter(error_tagged_angle_dataset, suspected_detection_error == 1)

  angle_data <- data %>% dplyr::select(starts_with("Angle_profile_"))
  diameter_data <- data %>% dplyr::select(starts_with("Diameter_profile_"))
  radius_data <- data %>% dplyr::select(starts_with("Radius_profile_"))
  outlinedata <- data %>% dplyr::select(starts_with("Outline_Oriented"))


  # make umap dataframes
  miniumaps <- make_miniumaps(clusters = clusters)
  # makes graphs of umaps of individual multimodal regions
  miniumapgraphs <- make_miniumap_graphlist(miniumaps = miniumaps, clusters = clusters)
  #
    Cluster_consensus_images <- make_consensus_for_all_clusters(clusters, outlinedata = outlinedata)


  graphs <- list()
  x5 <- list()

  for (i in 1:(length(clusters))) {
    X <- as.data.frame(names(clusters[[i]]))

    x2 <- X[grepl("Diameter_", X)]
    x3 <- X[grepl("Radius", X)]
    x4 <- X[grepl("Angle", X)]
    title <- paste(names(clusters[[i]]), collapse = " ")

    words <- str_extract_all(title, "\\b\\w+\\b")[[1]]
    # Extract words and numbers
    extracted2 <- gsub("_\\d+", "", words[1])
    words2 <- unique(gsub("_", " ", extracted2))

    numbers <- gsub("[^0-9]+", " ", title)

    # Split the numbers by space
    number_parts <- as.numeric(unlist(strsplit(numbers, " ")))

    # Determine the range of numbers
    number_range <- paste(min(number_parts), max(number_parts), sep = ":")
    if (any(grepl("\\d+:\\d+", number_range))) {
      # Create new title
      title <- paste0(words2, sep = " ", number_range, recycle0 = TRUE)
    } else {
      title <- paste0(words2, sep = " ", recycle0 = TRUE)
    }



    if (ncol(x2) > 0) {
      diameter_clusters <- cbind(diameter_data, clusters[[i]][["Clustering_file"]])
      d1 <- list()

      for (j in 1:max(clusters[[i]][["Clustering_file"]])) {
        D1 <- diameter_clusters %>% filter(`clusters[[i]][["Clustering_file"]]` == j)
        d1[[j]] <- apply(D1[1:100], 2, median)
      }

      q <- length(d1)
      d2 <- data.frame(x = 1:100, y = unlist(d1), group = rep(1:q, each = 100))

      x5[[i]] <- ggplot(d2, aes(x, y, group = group, color = as.factor(group))) +
        geom_line() +
        labs(x = "Profile Position", y = "Diameter Length", color = "Group") +
        coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")


    } else if (ncol(x3) > 0) {
      radius_clusters <- cbind(radius_data, clusters[[i]][["Clustering_file"]])
      r1 <- list()

      for (j in 1:max(clusters[[i]][["Clustering_file"]])) {
        R1 <- radius_clusters %>% filter(`clusters[[i]][["Clustering_file"]]` == j)
        r1[[j]] <- apply(R1[1:100], 2, median)
      }

      q <- length(r1)
      r2 <- data.frame(x = 1:100, y = unlist(r1), group = rep(1:q, each = 100))

      x5[[i]] <- ggplot(r2, aes(x, y, group = group, color = as.factor(group))) +
        geom_line() +
        labs(x = "Profile Position", y = "Radius Length", color = "Group") +
        coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")


    } else if (ncol(x4) > 0) {
      angle_clusters <- cbind(angle_data, clusters[[i]][["Clustering_file"]])
      a1 <- list()

      for (j in 1:max(clusters[[i]][["Clustering_file"]])) {
        A1 <- angle_clusters %>% filter(`clusters[[i]][["Clustering_file"]]` == j)
        a1[[j]] <- apply(A1[1:100], 2, median)
      }

      q <- length(a1)
      a2 <- data.frame(x = 1:100, y = unlist(a1), group = rep(1:q, each = 100))

      x5[[i]] <- ggplot(a2, aes(x, y, group = group, color = as.factor(group))) +
        geom_line() +
        labs(x = "Profile Position", y = "Angle", color = "Group") +
        coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")

    } else {
      print("No applicable profile graph")
      x5[[i]] <- "No applicable profile graph"
    }
    # Convert clusters to factor to preserve the correct order
    clusters_factor <- factor(clusters[[i]][["Clustering_file"]])



    graphs[[i]] <- list(graph3 = x5[[i]], graph4 = miniumapgraphs[[i]], graph5 = Cluster_consensus_images[[i]])
    print(paste(i, "/", length(clusters)))
  }


return(graphs)
}


###############
make.umapgraph <- function(clusters, umap, graphtype) {
  # Convert clusters to factor to preserve the correct order
  clusters_factor <- factor(clusters)

  umapcluster <- cbind(umap, clusters_factor)

  gromph1 <- ggplot(data = umapcluster, aes(V1, V2, color = clusters_factor)) +
    geom_point() +
    labs(title = title, x = paste0(graphtype, " variable 1"), y = paste0(graphtype, " variable 2"), colour = "clusters") +
    facet_wrap(clusters_factor)

  gromph2 <- ggplot(data = umapcluster, aes(V1, V2, color = clusters_factor)) +
    geom_point() +
    labs(title = title, x = paste0(graphtype, " variable 1"), y = paste0(graphtype, " variable 2"), colour = "clusters")

  graph1 <- gromph1 + gromph2
  graph1
}
############################################################

#' Build a single profile chart over a bimodal region displaying clusters
#'
#' @param profile_data the profile that will be graphed against
#' @param clusters the clustered bimodal regions to be graphed
#' @param umaplist A list of UMAP outputs
#' @param profiletype A string indicating the type of profile



Make_profile_graphs <- function(profile_data, clusters, umaplist = umaplist, profiletype) {
  umapo <- umaplist[1]
  profile_umap <- umaplist[profiletype + 1]
  profiletypes <- c("Angle", "Diameter", "Radius")

  graph1 <- make.umapgraph(clusters = clusters, umap = umapo, graphtype = "Full dataset")
  graph2 <- make.umapgraph(clusters = clusters, umap = profile_umap, graphtype = paste0(profiletypes[profiletype], " profile"))

  profile_clusters <- cbind(profile_data, clusters)

  # Initialize a list to store the matrices
  a <- list()
  angle_clusterinos <- cbind(angle_data, clusters[[i]][["Clustering_file"]])
  # Loop through clusters
  for (j in 1:max(clusters[[1]][["Clustering_file"]])) {
    A1 <- angle_clusterinos %>% filter(clusters[[i]][["Clustering_file"]] == j)
    a1 <- apply(A1[1:100], 2, median)
    a2 <- apply(A1[1:100], 2, quantile, probs = 0.25)
    a3 <- apply(A1[1:100], 2, quantile, probs = 0.75)

    # Combine the matrices for each cluster into a list
    a[[j]] <- list(median = a1, Q25 = a2, Q75 = a3)
  }

  # Create a long-format data frame for plotting
  a_long <- lapply(a, function(cluster) {
    data.frame(Position = 1:100, Angle = cluster$median, Q25 = cluster$Q25, Q75 = cluster$Q75)
  }) %>%
    bind_rows(.id = "Cluster")

  # Create the ggplot plot
  x5 <- ggplot(a_long, aes(x = Position, y = Angle, group = Cluster, color = Cluster)) +
    geom_line(linewidth = 1.2) +
    # geom_ribbon(aes(ymin = Q25, ymax = Q75), alpha = 0.05) +
    labs(title, x = "Profile Position", y = paste0(profiletypes[profiletype]), color = "Cluster") +
    # facet_wrap(~Cluster)+
    theme_minimal() +
    scale_color_discrete()
  x51 <- ggplot(a_long, aes(x = Position, y = Angle, group = Cluster, color = Cluster)) +
    geom_line(linewidth = 1.2) +
    # geom_ribbon(aes(ymin = Q25, ymax = Q75), alpha = 0.05) +
    labs(title, x = "Profile Position", y = paste0(profiletypes[profiletype]), color = "Cluster") +
    # facet_wrap(~Cluster)+
    theme_minimal() +
    scale_color_discrete()
  graph3 <- x5 + x51
}
