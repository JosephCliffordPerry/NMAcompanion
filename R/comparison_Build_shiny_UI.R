# comparison Shiny UI builder
comparisongraphviewerbuilder <- function(testgraphlist, clusters, data, hamming_consensus) {
  ui <- shinyUI(
    fluidPage(
      titlePanel("Graph Viewer"),
      tabsetPanel(
        # First tab - Graphs
        tabPanel(
          "Multimodal Regions",
          mainPanel(
            selectInput("section", "Select Section", choices = 1:(length(testgraphlist) - 1)),
            plotOutput("graph1"),
            verbatimTextOutput("text1"),
            plotOutput("graph2"),
            verbatimTextOutput("text2"),
            plotOutput("graph3"),
            verbatimTextOutput("text3"),
            plotOutput("graph4"),
            verbatimTextOutput("text4"),
            plotOutput("graph5"),
            verbatimTextOutput("text5"),
            dataTableOutput("table1")
          )
        ),

        # Second tab - Output
        tabPanel(
          "Overall Dataset",
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
        ),

        # Third tab - Morphotypes
        tabPanel(
          "Morphotypes",
          mainPanel(
            selectInput("morphoSection", "Select Morphotype Section", choices = 1:(length(hamming_consensus))),
            plotOutput("morphoGraph1")
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
      output$text1 <- renderPrint({
        graphs_section[["text1"]]
      })
      output$graph2 <- renderPlot({
        graphs_section[["graph2"]]
      })
      output$text2 <- renderPrint({
        graphs_section[["text2"]]
      })

      output$graph3 <- renderPlot({
        graphs_section[["graph3"]]
      })
      output$text3 <- renderPrint({
        graphs_section[["text3"]]
      })
      output$graph4 <- renderPlot({
        graphs_section[["graph4"]]
      })
      output$text4 <- renderPrint({
        graphs_section[["text4"]]
      })
      output$graph5 <- renderPlot({
        graphs_section[["graph5"]]
      })
      output$text5 <- renderPrint({
        graphs_section[["text5"]]
      })
      output$table1 <- renderTable({
        graphs_section[["table1"]]
      })

      # Get the last part of graphlist
      last_graphs <- testgraphlist[[length(testgraphlist)]]

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

    observe({
      morphoSection <- input$morphoSection
      morpho_graphs_section <- hamming_consensus[[as.numeric(morphoSection)]]

      output$morphoGraph1 <- renderPlot({
        morpho_graphs_section[["graph1"]]
      })
    })
  })

  # Run the Shiny app
  graphviewer <- shinyApp(ui, server)
  return(graphviewer)
}
