library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(visNetwork)
library(tidyverse)
library(DT)
library(readxl)

options(shiny.maxRequestSize = 10 * 1024^2)

# Define UI
ui <- dashboardPage(skin = "purple",
  header = dashboardHeader(title = "CONNECT"),
  sidebar = dashboardSidebar(
    fileInput("file", "Upload Data (.csv, .xls, or .txt)",
                          multiple = TRUE,
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv",
                                     ".xls",
                                     ".xlsx",
                                     ".txt")),
    br(),
    numericInput(inputId = "selectRowNum", label = "Transcript Row Number: ", value = 1),
    selectInput("selectRowColor", "Transcript Row Color: ", choices = c("lightskyblue" ,"yellow", "lightcoral", "green" ))
  ),
  body = dashboardBody(
    tags$head(
      tags$style(HTML("
      .box {
        overflow: scroll;
      }")
                 )
    ),
    # box(title = "Node Table", status = "primary", width = 6, collapsible = TRUE,
    #   dataTableOutput("nodeTable")
    #     ),
    # box(title = "Edge Table", status = "primary", width = 6, collapsible = TRUE,
    #   dataTableOutput("edgeTable")
    #     ),
    box(title = "Network Diagram", 
        status = "primary",
        collapsible = TRUE,
        visNetworkOutput("mynetwork")
        ),
    box(
      title = "Transcript Data",
      status = "primary", 
      width = 6,
      collapsible = TRUE,
      DT::dataTableOutput("transcriptTable")
    ),
    ),
  footer =  dashboardFooter( tagList(
                               tags$p("Developed by the IDEA Lab, Clemson University, Clemson, SC, USA. 2023"
                               ),
                               tags$p("Project is supported by the National Science Foundation (ECR-2024965, DRL-2031175) and the Office of the Associate Dean for Research in the College of Education at Clemson University."
                               )
  )
                              
  )
)


# Define server
server <- function(input, output, session) {
 
  ####TRANSCRIPT####
  
  # read transcript input file
  
  data_input <- reactive({
    req(input$file)
    
    tryCatch(
      {

        if (tolower(tools::file_ext(input$file$datapath)) == "xlsx") {
          clean_data <- read_excel(input$file$datapath)
        }

        else {
          clean_data <- read.csv(input$file$datapath)
        }

        names(clean_data) <- make.names(names(clean_data),unique = TRUE)

        clean_data

      },

      error = function(e) {
        stop(safeError(e))
      }
    )
    
  })
  
  # transcript data table 
  output$transcriptTable <- DT::renderDataTable({
    
    data_input() %>%
      datatable(
        # filter = "bottom",
        # server = TRUE, 
          #rownames = FALSE,
          class = "cell-border stripe",
          options = list(
            paging = TRUE,
            searchHighlight = TRUE,
            filter = "top",
            autoWidth = TRUE,
            pageLength = 10,
            #scrollX = "200px",
            scrollY = "400px"
            ),
      ) %>%
      # Update color background of a row in the table based on row and color selected from input
       formatStyle(0, 
                   target = "row", 
                   backgroundColor = styleEqual( input$selectRowNum, c(input$selectRowColor))
                  )
  })
  
  
    ###NETWORK###
  
  # Initialize nodes and edges as reactive values
  
  graph_data <-  reactiveValues(
    nodes = data.frame(id = character(0), 
                       label = character(0),
                       stringsAsFactors = FALSE),
    edges = data.frame(id = character(0), 
                       to = character(0),
                       from = character(0),
                       stringsAsFactors = FALSE)
  ) 
 
  
  
  # Render the network diagram
  
  output$mynetwork <- renderVisNetwork({
    visNetwork(graph_data$nodes, graph_data$edges) %>%
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1, algorithm = "hierarchical"), manipulation = list(enabled = TRUE,
                                                                                                                      addNodeCols = c("id", "label", "group"
                                                                                                                      ),
                                                                                                                      addEdgeCols = c("id","label", "width"),
                                                                                                                      editEdgeCols = c("label", "width"), 
                                                                                                                      editNodeCols = c("label", "group"
                                                                                                                                       ),
                                                                                                                      deleteNode = TRUE,
                                                                                                                      deleteEdge = TRUE
                                                                                                                      )
                 #selectedBy = "group"
      ) 
  })
  
  # Get node information from network dynamically using network proxy function
  
  # observe({
  #   input$getNodes
  #   visNetworkProxy("mynetwork") %>%
  #   visGetNodes()
  #  })
  # 

  # Render node table
  
  # output$nodeTable <- DT::renderDataTable({
  #   graph_data$nodes
  # })
  
  # Render edge table
  
  # output$edgeTable <- DT::renderDataTable({
  #   graph_data$edges
  # })
  
}

# Run the app
shinyApp(ui = ui, server = server)
