# Casenode monitoring module UI function
mod_monitorcn_UI <- function(id, label) {
  ns <- NS(id)
  tabItem(tabName = label,
          h2("Return random monitoring cases"),
          selectInput(inputId = ns("centre"), label = "Specify centre",
                      choices = c("all")),
          dateInput(inputId = ns("dateafter"), label = "Return cases after date",
                    value = "1900-01-01", width = 190),
          numericInput(inputId = ns("seednumber"), label = "Seed", value = 1, width = 100),
          setSliderColor(c("#dd4b39"), c(1)),
          sliderInput(inputId = ns("percentage"), label = "Specify percentage of cases",
                      min = 1, max = 99, value = 10, width = 400),
          hr(),
          actionButton(inputId = ns("create_mon_table"), label = "Submit configuration",
                       icon("paper-plane")),
          downloadButton(ns("download_monitoring_cases_csv"), "Cases"),
          downloadButton(ns("download_monitoring_config_csv"), "Config"),
          hr(),
          box(
            tableOutput(ns("monitoring_cases")),
            width = 4
          )
  )
}

# Casenode monitoring module server function
mod_monitorcn <- function(input, output, session, sT_export) {
  # reactive button
  rdm_cases <- eventReactive(input$create_mon_table, {
    perc <- input$percentage / 100
    rand_participants <- return_random_participants(sT_export(), seed = input$seednumber,
                                                    centres = input$centre,
                                                    date = input$dateafter,
                                                    percent = perc)
    rand_participants$participants
  })

  output$monitoring_cases <- renderTable({
    rdm_cases()
  })

  output$download_monitoring_cases_csv <- downloadHandler(

    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      "monitoring_cases.csv"
    },

    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      # Write to a file specified by the 'file' argument
      write.csv(rdm_cases(), file = file, row.names = FALSE, quote = FALSE)
    }
  )

  output$download_monitoring_config_csv <- downloadHandler(

    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      "monitoring_cases_config.csv"
    },

    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      write(
        paste(c(
          paste0("percentage: ", input$percentage),
          paste0("seed: ", input$seednumber),
          paste0("centre(s): ", input$centre),
          paste0("after date: ", as.character.Date(input$dateafter)))),
        sep = ",", file = file, append = TRUE
      )
    }
  )
}
