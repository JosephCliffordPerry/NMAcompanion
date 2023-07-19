#' @importFrom factoextra fviz_nbclust
#' @importFrom factoextra hkmeans
#' @importFrom umap umap
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 geom_line
#' @importFrom dplyr filter
#' @importFrom shiny shinyUI
#' @importFrom shiny fluidPage
#' @importFrom shiny titlePanel
#' @importFrom shiny mainPanel
#' @importFrom shiny selectInput
#' @importFrom shiny plotOutput
#' @importFrom shiny sidebarLayout
#' @importFrom shiny sidebarPanel
#' @importFrom shiny verbatimTextOutput
#' @importFrom shiny shinyServer
#' @importFrom shiny observe
#' @importFrom shiny renderPlot
#' @importFrom shiny renderPrint
#' @importFrom shiny shinyApp
#' @importFrom stringr str_extract_all
#' @importFrom dplyr %>%
#' @importFrom dplyr starts_with
#' @importFrom dplyr select

targeted_profile_analysis <- function(rawdata){


  Extreme_angle_detector <- function(data){

    #cutting dataset into different portions based on content
    dataset<- data %>% dplyr::select(starts_with("Angle_profile_"))

    # Iterate through the dataset row by row
    for (i in 1:nrow(dataset)) {
      # Check if any value in the row is over 280
      if (any(dataset[i, ] > 280)) {
        # Tag the row with 2 in "suspected detection error" column
        data$suspected_detection_error[i] <- 2
      } else {
        # Tag the row with 1 in "suspected detection error" column
        data$suspected_detection_error[i] <- 1
      }
    }
    return(data)}

  graphviewerbuilder <- function(testgraphlist){
    # Define the UI
    ui <- shinyUI(
      fluidPage(
        titlePanel("Graph Viewer"),
        mainPanel(
          selectInput("section", "Select Section", choices = 1:length(testgraphlist)),
          plotOutput("graph1"),
          plotOutput("graph2"),
          plotOutput("graph3"),
          sidebarLayout(
            sidebarPanel(
              # Input controls if needed
            ),
            mainPanel(
              verbatimTextOutput("output")
            )
          )
        ) )
    )


    # Define the server
    server <- shinyServer(function(input, output) {
      observe({
        section <- input$section
        graphs_section <- testgraphlist[[as.numeric(section)]]

        output$graph1 <- renderPlot({
          graphs_section[["graph1"]]
        })

        output$graph2 <- renderPlot({
          graphs_section[["graph2"]]
        })
        output$graph3 <- renderPlot({
          graphs_section[["graph3"]]
        })
        output$output <- renderPrint({
          # Print the variables
          cat("percent bad edge detected:", percent_bad_edge_detected, "\n")
          cat("number of cells used:", number_of_cells_used, "\n")
        })
      })
    })

    # Run the Shiny app
    graphviewer <- shinyApp(ui, server)
    return(graphviewer)
  }


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
      gromph1 <- ggplot(data = umapo_cluster, aes(umapo_cluster$V1, umapo_cluster$V2, color = clusters_factor)) +
        geom_point() + labs(title = title,colour = "clusters") +
        facet_wrap(clusters_factor)

      gromph2 <- ggplot(data = umapo_cluster, aes(umapo_cluster$V1, umapo_cluster$V2, color = clusters_factor)) +
        geom_point() + labs(title = title, colour = "clusters")

      graph1 <- gromph1 + gromph2

      gromph3 <- ggplot(data = angleumapcluster, aes(angleumapcluster$V1, angleumapcluster$V2, color = clusters_factor)) +
        geom_point() + labs(title = title,colour = "clusters") +
        facet_wrap(clusters_factor)

      gromph4 <- ggplot(data = angleumapcluster, aes(angleumapcluster$V1, angleumapcluster$V2, color = clusters_factor)) +
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

        x5[[i]] <- ggplot(d2, aes(d2$x, d2$y, group = group, color = as.factor(group))) +
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


fulldatasetclustergrapher <- function(data){

    dataset <- dplyr::select_if(data[7:327], is.numeric)

    #function that calculates the number of clusters from the WSS cluster number calculator
    WSSNclustercalc <- function(clusterWSS){
      x <- clusterWSS[["data"]][["y"]]
      x2 <- diff(x)
      x3 <- which(x2 > x2[1]/5)
      return(min(x3))}


    # Scale the selected columns
    scaleddata <- scale(dataset)
    #sample out the maximum amount of data that can be processed with  fviz_nbclust
    sampled_data <- as.data.frame(scaleddata[sample(nrow(dataset),1000), ])
    # Perform clustering and determine the optimal number of clusters
    clustersSil <- fviz_nbclust(sampled_data, FUNcluster = kmeans, method = "silhouette")
    #clusterNBC <- NbClust(sampled_data, distance = "euclidean",
    #         min.nc = 2, max.nc = 9,
    #        method = "complete", index ="all")
    #   n <- fviz_nbclust(clusterNBC)
    #clusterNBCN <- as.numeric(gsub("\\D", "", n[["labels"]][["title"]]))
    clusterWSS <- fviz_nbclust(sampled_data, FUNcluster = kmeans, method = "wss")

    clustergap <- fviz_nbclust(sampled_data, FUNcluster = kmeans, method = "gap")
    gapNclusters <- which.max(clustergap[["data"]][["gap"]])
    SilNclusters <- which.max(clustersSil[["data"]][["y"]])

    WSSNclusters <- WSSNclustercalc(clusterWSS)

    Nclusters <- round(mean(c(WSSNclusters,SilNclusters,gapNclusters)))#add clusterNBCN if needed
    #clusters dataset
    clusters <- hkmeans(scaleddata, k = Nclusters)

    # Extract the clustering results
    Clustering_file <- clusters$cluster

    umapo <- umap(dataset, preserve.seed = TRUE)
    print("umap1 done")
    angleumap <- umap(angle_data, preserve.seed = TRUE)
    print("umap2 done")
    umapodata <- as.data.frame(umapo$layout)
    angleumapdata <- as.data.frame(angleumap$layout)

    title <- "Whole dataset clustered"

    # Convert clusters to factor to preserve the correct order
    clusters_factor <- factor(Clustering_file)
    umapo_cluster<-cbind(umapodata,clusters_factor)
    angleumapcluster <-cbind(angleumapdata,clusters_factor)
    gromph1 <- ggplot(data = umapo_cluster, aes(umapo_cluster$V1, umapo_cluster$V2, color = clusters_factor)) +
      geom_point() + labs(title = title) +
      facet_wrap(clusters_factor)

    gromph2 <- ggplot(data = umapo_cluster, aes(umapo_cluster$V1, umapo_cluster$V2, color = clusters_factor)) +
      geom_point() + labs(title = title)

    graph1 <- gromph1 + gromph2

    gromph3 <- ggplot(data = angleumapcluster, aes(angleumapcluster$V1, angleumapcluster$V2, color = clusters_factor)) +
      geom_point() + labs(title = title) +
      facet_wrap(clusters_factor)

    gromph4 <- ggplot(data = angleumapcluster, aes(angleumapcluster$V1, angleumapcluster$V2, color = clusters_factor)) +
      geom_point() + labs(title = title)

    graph2 <- gromph3 + gromph4

    graphs <- list(graph1 = graph1, graph2 = graph2, graph3 = NULL)
    diameter_clusters <- cbind(diameter_data, Clustering_file)
    d1 <- list()

    for (j in 1:max(Clustering_file)) {
      D1 <- diameter_clusters %>% filter(Clustering_file == j)
      d1[[j]] <- apply(D1[1:100], 2, median)
    }

    q <- length(d1)
    d2 <- data.frame(x = 1:100, y = unlist(d1), group = rep(1:q, each = 100))

    d3 <- ggplot(d2, aes(x, y, group = group, color = as.factor(group))) +
      geom_line() +
      labs(x = "X", y = "Y", color = "Group",title = "Diameter profiles")
    radius_clusters <- cbind(radius_data, Clustering_file)
    r1 <- list()

    for (j in 1:max(Clustering_file)) {
      R1 <- radius_clusters %>% filter(Clustering_file == j)
      r1[[j]] <- apply(R1[1:100], 2, median)
    }

    q <- length(r1)
    r2 <- data.frame(x = 1:100, y = unlist(r1), group = rep(1:q, each = 100))

    r3 <- ggplot(r2, aes(x, y, group = group, color = as.factor(group))) +
      geom_line() +
      labs(x = "X", y = "Y", color = "Group",title = "radius profiles")

    angle_clusters <- cbind(angle_data, Clustering_file)
    a1 <- list()

    for (j in 1:max(Clustering_file)) {
      A1 <- angle_clusters %>% filter(Clustering_file == j)
      a1[[j]] <- apply(A1[1:100], 2, median)
    }

    q <- length(a1)
    a2 <- data.frame(x = 1:100, y = unlist(a1), group = rep(1:q, each = 100))

    a3 <- ggplot(a2, aes(x, y, group = group, color = as.factor(group))) +
      geom_line() +
      labs(x = "X", y = "Y", color = "Group",title = "angle profiles")

    graph3 <- d3+a3+r3

    graphs <- list(graph1 = graph1, graph2 = graph2, graph3 = graph3)

    return(graphs)}
error_tagged_angle_dataset<-Extreme_angle_detector(data = rawdata)
data <- filter(error_tagged_angle_dataset, suspected_detection_error == 1)
error <- filter(error_tagged_angle_dataset, suspected_detection_error == 2)

bad_edge_detected <- nrow(error)/nrow(data)
percent_bad_edge_detected <-round(bad_edge_detected*100,digits = 3)
number_of_cells_used<-nrow(data)
#cutting dataset into different portions based on content
angle_data<- data %>% dplyr::select(starts_with("Angle_profile_"))
diameter_data<- data %>% dplyr::select(starts_with("Diameter_profile_"))
radius_data<- data %>% dplyr::select(starts_with("Radius_profile_"))
other_data <- dplyr::select_if(data[7:27], is.numeric)


#makes list for bimodality check
selected_datasets <- list()
#checks all portions for bimodality
selected_datasets <- hartigansdipper(angle_data,selected_datasets)
selected_datasets <- hartigansdipper(diameter_data,selected_datasets)
selected_datasets <- hartigansdipper(radius_data,selected_datasets)
selected_datasets <- monohartigansdipper(other_data,selected_datasets)
#clusters data
clusters <- list()
clusters <-targeted_profile_clusterer(selected_datasets = selected_datasets)

#creates list of graphs
testgraphlist2<-plotbuilder3(clusters = clusters,originaldata = data,angle_data = angle_data,diameter_data = diameter_data,radius_data = radius_data )
testgraphlist2[length(testgraphlist2)+1][[1]] <- fulldatasetclustergrapher(data = data)

#creates a popout to view the graphs
graphview<-graphviewerbuilder(testgraphlist = testgraphlist2)

return(graphview)}
