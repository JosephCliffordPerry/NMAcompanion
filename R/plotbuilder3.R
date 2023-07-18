plotbuilder3 <- function(clusters, originaldata, angle_data, diameter_data, radius_data){
  numdata <- dplyr::select_if(originaldata, is.numeric)
  umapo <- umap(numdata, preserve.seed = TRUE)
  print("umap1 done")
  angleumap <- umap(angle_data, preserve.seed = TRUE)
  print("umap2 done")
  umapodata <- as.data.frame(umapo$layout)
  angleumapdata <- as.data.frame(angleumap$layout)
  graphs <- list()
  x5 <- list()

  for (i in 1:length(clusters)) {
    X <- as.data.frame(names(clusters[[i]]))

    x2 <- X[grepl("Diameter_", X)]
    x3 <- X[grepl("Radius", X)]
    x4 <- X[grepl("Angle", X)]
    title <- paste(names(selected_datasets[[i]]), collapse = " ")

    words <- str_extract_all(title, "\\b\\w+\\b")[[1]]
    # Extract words and numbers
    extracted2 <- gsub("_\\d+", "", words[1])
    words2 <- unique(gsub("_", " ",extracted2 ))

    numbers <- gsub("[^0-9]+", " ", title)

    # Split the numbers by space
    number_parts <- as.numeric(unlist(strsplit(numbers, " ")))

    # Determine the range of numbers
    number_range <- paste(min(number_parts, na.rm = TRUE), max(number_parts, na.rm = TRUE), sep = ":")
    if (any(grepl("\\d+:\\d+", number_range))) {

      # Create new title
      title <- paste0(words2, sep = " ",number_range, recycle0 = TRUE)}
    else {
      title <- paste0(words2, sep = " ", recycle0 = TRUE)}

    # Convert clusters to factor to preserve the correct order
    clusters_factor <- factor(clusters[[i]][["Clustering_file"]])
    umapo_cluster<-cbind(umapodata,clusters_factor)
    angleumapcluster <-cbind(angleumapdata,clusters_factor)
    gromph1 <- ggplot(data = umapo_cluster, aes(V1, V2, color = clusters_factor)) +
      geom_point() + labs(title = title,colour = "clusters") +
      facet_wrap(clusters_factor)

    gromph2 <- ggplot(data = umapo_cluster, aes(V1, V2, color = clusters_factor)) +
      geom_point() + labs(title = title, colour = "clusters")

    graph1 <- gromph1 + gromph2

    gromph3 <- ggplot(data = angleumapcluster, aes(V1, V2, color = clusters_factor)) +
      geom_point() + labs(title = title,colour = "clusters") +
      facet_wrap(clusters_factor)

    gromph4 <- ggplot(data = angleumapcluster, aes(V1, V2, color = clusters_factor)) +
      geom_point() + labs(title = title,colour = "clusters")

    graph2 <- gromph3 + gromph4



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
        labs(x = "X", y = "Y", color = "Group")

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
        labs(x = "X", y = "Y", color = "Group")

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
        labs(x = "X", y = "Y", color = "Group")

    } else {
      print("No applicable profile graph")

    }
    graphs[[i]] <- list(graph1 = graph1, graph2 = graph2, graph3 = x5[i])
    print(paste(i,"/",length(clusters)))}
  return(graphs)
}
