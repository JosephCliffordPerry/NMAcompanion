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
  Outliney <- t(data %>% dplyr::select(starts_with("Outline_OrientedCoordinates_Y")))
  Outlinex <- t(data %>% dplyr::select(starts_with("Outline_OrientedCoordinates_X")))

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
      geom_polygon(alpha = 0.5, color = "black") +
      theme_minimal()

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
make_profile_graphs <- function(data,
                                groups = NULL,
                                positions = 100,
                                profile_type = NULL) {

  profile_types <- c("Angle", "Radius", "Diameter")

  # If profile_type given, validate it; else set to all
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
        data.frame(
          x = 1:positions,
          y = median_profile,
          group = as.factor(g),
          type = pt
        )
      })
      bind_rows(profiles)

    } else {
      # No groups — single median profile across all rows
      median_profile <- apply(prof_data[1:positions, , drop = FALSE], 2, median)
      data.frame(
        x = 1:positions,
        y = median_profile,
        group = "All",
        type = pt
      )
    }
  })

  profile_df <- bind_rows(all_profiles)

  p <- ggplot(profile_df, aes(x = x, y = y, color = group)) +
    geom_line(linewidth = 1.2) +
    labs(
      x = "Profile Position",
      y = "Value",
      color = ifelse(is.null(groups), "", "Cluster")
    ) +
    theme_minimal()

  # Facet if multiple profile types
  if (length(profile_types) > 1) {
    p <- p + facet_wrap(~type, scales = "free_y")
  }

  return(p)
}


# make a umap graph

