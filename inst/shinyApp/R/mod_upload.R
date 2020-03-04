# Upload module UI function
mod_upload_UI <- function(id, label){
  ns <- NS(id)
  tabItem(tabName = label,
          tags$head(tags$style(".progress-bar{background-color:#dd4b39;}")),
          fileInput(inputId = ns("secuTrial_export_file"),
                    label = "Choose secuTrial export zip",
                    multiple = FALSE,
                    accept = c("zip", "ziparchive", ".zip"),
                    width = 700),
          textOutput(ns("read_sT_data")),
          hr(),
          actionButton(inputId = ns("use_example_data"), label = "Use example data", icon("lightbulb")),
          hr(),
          textOutput(ns("example_sT_data"))
  )
}

# Upload module server function
mod_upload <- function(input, output, session, sT_export){
  # read upload data
  observeEvent(input$secuTrial_export_file$datapath, {
    curr_export <- read_secuTrial(input$secuTrial_export_file$datapath)
    sT_export(curr_export)
  })

  output$read_sT_data <- renderText({
    # catch exception
    if (is.null(input$secuTrial_export_file$datapath)) {
      print("Please upload file.")
    } else {
      # select centre dropdown for monitoring cases
      ctr <- sT_export()[[sT_export()$export_options$meta_names$centres]]
      updateSelectInput(session, inputId = "centre",
                        choices = c("all", ctr$mnpctrname)
      )

      if (length(sT_export())) {
        print("Upload and reading of data successful.")
      } else {
        print("Error: Data could not be read.")
      }
    }
  })

  # use example data
  observeEvent(input$use_example_data, {
    path <- system.file("extdata", "sT_exports", "longnames",
                        "s_export_CSV-xls_CTU05_long_ref_miss_en_utf8.zip",
                        package = "secuTrialR")
    curr_export <- read_secuTrial(path)
    sT_export(curr_export)
  })

  observeEvent(input$use_example_data, {
    sendSweetAlert(
      session = session,
      title = "Example data loaded.",
      text = icon("lightbulb"),
      btn_colors = "#dd4b39",
      btn_labels = "OK"
      #type = "success"
    )
  })
}
