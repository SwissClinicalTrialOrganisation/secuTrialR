library(shiny)

# Define UI for data upload app ----
  ui <- fluidPage(

    # App title ----
    titlePanel("Uploading Files"),

    # Sidebar layout with input and output definitions ----
    sidebarLayout(

      # Sidebar panel for inputs ----
      sidebarPanel(

        # Input: Select a file ----
        fileInput("file1", "Choose secuTrial export",
                  multiple = FALSE,
                  accept = c("zip",
                             "ziparchive",
                             ".zip")),

        # Input: Select number of rows to display ----
        radioButtons("disp", "Display:",
                     choices = c(export_options = "export options",
                                 export_overview = "export overview",
                                 completeness = "completeness",
                                 recruitment_plot = "recruitment plot",
                                 scores = "score variables",
                                 linkage = "CDMA linkage"),
                     selected = "export options")

      ),

      # Main panel for displaying outputs ----
      mainPanel(

        # Output: Data file ----
        tableOutput("contents"),
        plotOutput(outputId = "plot")

      )

    )
  )

# Define server logic to read selected file ----
server <- function(input, output) {

  output$contents <- renderTable({

    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    req(input$file1)

    tryCatch(
      {
        library(secuTrialR)
        sT_export <- read_secuTrial(input$file1$datapath)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )

    if (input$disp == "export options") {
      print(sT_export$export_options)
    } else if (input$disp == "export overview") {
      print(sT_export)
    } else if (input$disp == "completeness") {
      form_status_summary(sT_export)
    } else if (input$disp == "score variables") {
      return_scores(sT_export)
    } else if (input$disp == "CDMA linkage") {
      links_secuTrial(sT_export)
    }

  })

  output$plot <- renderPlot({

    req(input$file1)

    tryCatch(
      {
        library(secuTrialR)
        sT_export <- read_secuTrial(input$file1$datapath)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    if (input$disp == "recruitment plot") {
      plot_recruitment(sT_export)
    }

  })

}

# Create Shiny app ----
shinyApp(ui, server)
