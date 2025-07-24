#' Generate a Consensus Polygon from Outline Data
#'
#' This function computes a mean outline (consensus shape) from coordinate data.
#' If `groups` is provided, it computes one consensus outline per group.
#'
#' @param data A data frame imported from NMA
#' @param groups Optional. A vector indicating group membership for each outline.
#'
#' @return A ggplot2 object showing the consensus outline(s)
#'
#'@importFrom purrr map_dfr
#'@export
MakeNMAConsensus <- function(data, groups = NULL) {
  # Extract outlines
  Outliney <- t(data %>% select(starts_with("Outline_OrientedCoordinates_Y")))
  Outlinex <- t(data %>% select(starts_with("Outline_OrientedCoordinates_X")))

  if (!is.null(groups)) {
    # Add grouping information
    if (length(groups) != ncol(Outliney)) {
      stop("Length of groups must match the number of outlines.")
    }

    # Calculate mean outline per group using purrr::map_dfr
    plot_data <- purrr::map_dfr(unique(groups), function(g) {
      group_idx <- which(groups == g)
      group_y <- rowMeans(Outliney[, group_idx, drop = FALSE])
      group_x <- rowMeans(Outlinex[, group_idx, drop = FALSE])

      data.frame(
        x = group_x,
        y = group_y,
        group = g
      )
    })

    # Plot grouped consensus polygons
    consensus <- ggplot(plot_data, aes(x = x, y = y, fill = group, group = group)) +
      geom_polygon(alpha = 0.5, color = "black", linewidth = 1) +
      theme(legend.position = "none") +
      ggplot(plot_data, aes(x = x, y = y, fill = group, group = group)) +
      geom_polygon(alpha = 0.6, color = "black", linewidth = 1) +
      facet_wrap(~ group) +
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
      ) +
      labs(title = "Stitched Consensus Polygons")

  } else {
    # Calculate overall mean outline
    meanOutlineY <- rowMeans(Outliney)
    meanOutlineX <- rowMeans(Outlinex)

    polygon_df <- data.frame(
      x = meanOutlineX,
      y = meanOutlineY
    )

    # Plot single consensus polygon
    consensus <- ggplot(polygon_df, aes(x = x, y = y)) +
      geom_polygon(alpha = 0.7) +
      theme_minimal()
  }

  return(consensus)
}
#' Generate Profile Graphs (Angle, Radius, Diameter) Faceted or Single
#'
#' @param data Data frame containing profile data
#' @param groups Optional vector of cluster assignments (same length as nrow(data)), or NULL
#' @param positions Number of profile positions (default 100)
#' @param profile_type Optional character, one of "Angle", "Radius", "Diameter", or NULL (default NULL = all)
#'
#' @return ggplot object faceted by profile type or single plot
#' @export
make_NMA_profile_graphs <- function(data,
                                    groups = NULL,
                                    positions = 100,
                                    profile_type = NULL) {

  profile_types <- c("Angle", "Radius", "Diameter")

  # Validate or set profile type
  if (!is.null(profile_type)) {
    if (!profile_type %in% profile_types) {
      stop("profile_type must be one of 'Angle', 'Radius', 'Diameter', or NULL.")
    }
    profile_types <- profile_type
  }

  all_profiles <- lapply(profile_types, function(pt) {
    prof_data <- data %>%
      dplyr::select(starts_with(paste0(pt, "_profile_")))

    if (!is.null(groups)) {
      if (length(groups) != nrow(prof_data)) {
        stop("Length of groups must match the number of rows in the profile data.")
      }

      combined <- cbind(prof_data, cluster = groups)

      profiles <- lapply(unique(groups), function(g) {
        cluster_subset <- combined %>% filter(cluster == g)

        median_profile <- apply(cluster_subset[1:positions], 2, median)
        q1_profile <- apply(cluster_subset[1:positions], 2, quantile, probs = 0.25)
        q3_profile <- apply(cluster_subset[1:positions], 2, quantile, probs = 0.75)

        data.frame(
          x = 1:positions,
          y = median_profile,
          ymin = q1_profile,
          ymax = q3_profile,
          group = as.factor(g),
          type = pt
        )
      })

      bind_rows(profiles)

    } else {
      # No groups — single median profile
      median_profile <- apply(prof_data[1:positions, , drop = FALSE], 2, median)
      q1_profile <- apply(prof_data[1:positions, , drop = FALSE], 2, quantile, probs = 0.25)
      q3_profile <- apply(prof_data[1:positions, , drop = FALSE], 2, quantile, probs = 0.75)

      data.frame(
        x = 1:positions,
        y = median_profile,
        ymin = q1_profile,
        ymax = q3_profile,
        group = "All",
        type = pt
      )
    }
  })

  profile_df <- bind_rows(all_profiles)

  # Plot with IQR ribbons
  p <- ggplot(profile_df, aes(x = x, y = y, color = group, fill = group)) +
    geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.2, color = NA) +
    geom_line(linewidth = 1.2) +
    labs(
      x = "Profile Position",
      y = "Value",
      color = ifelse(is.null(groups), "", "Cluster"),
      fill = ifelse(is.null(groups), "", "Cluster")
    ) +
    theme_minimal()

  # Facet if multiple profile types
  if (length(profile_types) > 1) {
    p <- p + facet_wrap(~type, scales = "free_y")
  }

  return(p)
}



# make a umap graph
#' Generate UMAP Plot (with optional grouping)
#'
#' @param data A numeric matrix or data frame to perform UMAP on (e.g. profile data)
#' @param groups Optional vector of group/cluster assignments (same length as nrow(data)), or NULL
#' @param title Optional character string for plot title
#' @param n_neighbors Number of neighbors for UMAP (default: 15)
#' @param min_dist Minimum distance for UMAP (default: 0.1)
#' @param metric Distance metric (default: "euclidean")
#'
#' @return A ggplot object (with or without faceting)
#' @export
UmapNMAdata <- function(data,
                            groups = NULL,
                            title = NULL,
                            n_neighbors = 15,
                            min_dist = 0.1,
                            metric = "euclidean") {
    # Validate data
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("'data' must be a data frame or matrix")
  }

  # Run UMAP
  umap_result <- umap::umap(data,
                            config = umap::umap.defaults,
                            n_neighbors = n_neighbors,
                            min_dist = min_dist,
                            metric = metric)

  umap_df <- as.data.frame(umap_result$layout)
  colnames(umap_df) <- c("UMAP1", "UMAP2")

  if (!is.null(groups)) {
    if (length(groups) != nrow(data)) {
      stop("Length of 'groups' must match number of rows in 'data'")
    }
    umap_df$group <- factor(groups)

    # Full colored plot
    p_full <- ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = group)) +
      geom_point(size = 1.2, alpha = 0.8) +
      theme(legend.position = "none", axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())+
      labs(
        title = title %||% "UMAP of Groups",
        x = NULL, y = NULL, color = "Group"
      ) +
      theme_minimal() +
      coord_fixed()

    # Faceted plot by group
    p_facet <- ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = group)) +
      geom_point(size = 1.2, alpha = 0.8) +
      facet_wrap(~group) +
      theme_minimal() +
      labs(x = NULL, y = NULL) +
      coord_fixed()

    return(p_full + p_facet)  # requires patchwork

  } else {
    # Plot without grouping
    p <- ggplot(umap_df, aes(x = UMAP1, y = UMAP2)) +
      geom_point(size = 1.2, alpha = 0.8) +
      labs(
        title = title %||% "UMAP Projection",
        x = NULL, y = NULL
      ) +
      theme_minimal() +
      coord_fixed()

    return(p)
  }
}

