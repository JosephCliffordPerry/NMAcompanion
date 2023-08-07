#' Shiny Ui builder

graphviewerbuilder <- function(testgraphlist){
  ui <- shinyUI(
    fluidPage(
      titlePanel("Graph Viewer"),
      tabsetPanel(
        # First tab - Graphs
        tabPanel("Multimodal Regions",
                 mainPanel(
                   selectInput("section", "Select Section", choices = 1:(length(testgraphlist)-1)),
                   plotOutput("graph1"),
                   plotOutput("graph2"),
                   plotOutput("graph3")
                 )
        ),

        # Second tab - Output
        tabPanel("Overall Dataset",
                 mainPanel(
                   verbatimTextOutput("output"),
                   plotOutput("graphA"),
                   plotOutput("graphB"),
                   plotOutput("graphC"),
                   plotOutput("graphD"),
                   plotOutput("graphE"),
                   plotOutput("graphF"),
                   plotOutput("graphG")
                 )
        )
      )
    )
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

      # Get the last part of graphlist
      last_graphs <- testgraphlist[[length(graphlist)]]

      output$graphA <- renderPlot({
        last_graphs[["graphA"]]
      })

      output$graphB <- renderPlot({
        last_graphs[["graphB"]]
      })

      output$graphC <- renderPlot({
        last_graphs[["graphC"]]
      })

      output$graphD <- renderPlot({
        last_graphs[["graphD"]]
      })

      output$graphE <- renderPlot({
        last_graphs[["graphE"]]
      })

      output$graphF <- renderPlot({
        last_graphs[["graphF"]]
      })

      output$graphG <- renderPlot({
        last_graphs[["graphG"]]
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
