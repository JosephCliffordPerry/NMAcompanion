#' @importFrom Shiny shinyUI
#' @importFrom Shiny fluidPage
#' @importFrom shiny
#' @importFrom shiny
#' @importFrom shiny

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
