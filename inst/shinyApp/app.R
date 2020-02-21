## app.R ##
library(shinydashboard)
library(shiny)
library(shinyWidgets) # setSliderColor
library(secuTrialR)

source("R/mod_upload.R")
source("R/mod_recruitplot.R")
source("R/mod_recruittable.R")
source("R/mod_formcomplete.R")
source("R/mod_visitplan.R")
source("R/mod_monitorcn.R")
source("R/mod_codebook.R")
source("R/mod_export.R")

mod <- list(
  upload = "mod_upload",
  recruitplot = "mod_recruitplot",
  recruittable = "mod_recruittable",
  formcomplete = "mod_formcomplete",
  visitplan = "mod_visitplan",
  monitorcn = "mod_monitorcn",
  codebook = "mod_codebook",
  export = "mod_export"
)

ui <- dashboardPage(skin = "red",
                    dashboardHeader(title = "SCTO - secuTrialR"),
                    dashboardSidebar(
                      sidebarMenu(
                        # set icon colors
                        tags$style(".fa-upload {color:#dd4b39}"),
                        tags$style(".fa-signal {color:#dd4b39}"),
                        tags$style(".fa-table {color:#dd4b39}"),
                        tags$style(".fa-percent {color:#dd4b39}"),
                        tags$style(".fa-calendar-alt {color:#dd4b39}"),
                        tags$style(".fa-dice {color:#dd4b39}"),
                        tags$style(".fa-book {color:#dd4b39}"),
                        tags$style(".fa-download {color:#dd4b39}"),
                        tags$style(".fa-paper-plane {color:#dd4b39}"),
                        tags$style(".fa-lightbulb {color:#dd4b39}"),
                        # define sidebar menu items
                        menuItem("Upload", tabName = mod$upload, icon = icon("upload")),
                        menuItem("Recruitment plot", tabName = mod$recruitplot, icon = icon("signal")),
                        menuItem("Recruitment table", tabName = mod$recruittable, icon = icon("table")),
                        menuItem("Form completeness", tabName = mod$formcomplete, icon = icon("percent")),

                        menuItem("Visit plan", tabName = "visitstructure", icon = icon("calendar-alt")),
                        menuItem("Monitoring cases", tabName = "moncases", icon = icon("dice")),
                        menuItem("Codebook", tabName = "codebook", icon = icon("book")),
                        menuItem("STATA - SAS - SPSS", tabName = "download", icon = icon("download"))
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        mod_upload_UI(mod$upload, label = mod$upload),
                        mod_recruitplot_UI(mod$recruitplot, label = mod$recruitplot),
                        mod_recruittable_UI(mod$recruittable, label = mod$recruittable),
                        mod_formcomplete_UI(mod$formcomplete, label = mod$formcomplete),

                        # Fifth tab content
                        tabItem(tabName = "visitstructure",
                                h2("Visit plan"),
                                box(plotOutput("visit_structure", height = 500, width = 900), width = 1000)
                        ),
                        # Sixth tab monitoring
                        tabItem(tabName = "moncases",
                                h2("Return random monitoring cases"),
                                selectInput(inputId = "centre", label = "Specify centre",
                                            choices = c("all")),
                                dateInput(inputId = "dateafter", label = "Return cases after date",
                                          value = "1900-01-01", width = 190),
                                numericInput(inputId = "seednumber", label = "Seed", value = 1, width = 100),
                                setSliderColor(c("#dd4b39"), c(1)),
                                sliderInput(inputId = "percentage", label = "Specify percentage of cases",
                                            min = 1, max = 99, value = 10, width = 400),
                                hr(),
                                actionButton(inputId = "create_mon_table", label = "Submit configuration",
                                             icon("paper-plane")),
                                downloadButton("download_monitoring_cases_csv", "Cases"),
                                downloadButton("download_monitoring_config_csv", "Config"),
                                hr(),
                                box(tableOutput("monitoring_cases"), width = 4)
                        ),

                        # seventh tab codebook
                        tabItem(tabName = "codebook",
                                h2("Codebook"),
                                tags$style(HTML(".tabbable > .nav > li > a                  {background-color: #444;  color:white}
                                                 .tabbable > .nav > li[class=active]    > a {background-color: #dd4b39; color:white}")),
                                tabsetPanel(
                                  tabPanel("Forms", tableOutput("forms")),
                                  tabPanel("Questions", tableOutput("questions")),
                                  tabPanel("Items", tableOutput("items")),
                                  tabPanel("Centres", tableOutput("centres")),
                                  tabPanel("Cases", tableOutput("cases")),
                                  tabPanel("Visitplan", tableOutput("visitplan"))
                                )


                        ),

                        # Last tab content / Download
                        tabItem(tabName = "download",
                                h2("Download data conversion archives"),
                                br(),
                                h4("Download STATA"),
                                downloadButton("downloadDataStata", "Download dta zip archive"),
                                br(),
                                br(),
                                h4("Download SAS"),
                                downloadButton("downloadDataSas", "Download sas7bdat zip archive"),
                                br(),
                                br(),
                                downloadButton("downloadDataXpt", "Download xpt v8 zip archive"),
                                br(),
                                br(),
                                h4("Download SPSS"),
                                downloadButton("downloadDataSav", "Download sav zip archive")
                        )
                      )
                    )
)

server <- function(input, output, session) {
  # init the sT export reactive Val
  sT_export <- reactiveVal()
  callModule(mod_upload, mod$upload, sT_export)
  callModule(mod_recruitplot, mod$recruitplot, sT_export)
  callModule(mod_recruittable, mod$recruittable, sT_export)
  callModule(mod_formcomplete, mod$formcomplete, sT_export)

  # start codebook
  output$forms <- renderTable({
    sT_export()[[sT_export()$export_options$meta_names$forms]]
  })

  output$questions <- renderTable({
    sT_export()[[sT_export()$export_options$meta_names$questions]]
  })

  output$items <- renderTable({
    #cols <- c("ffcolname", "itemtype", "fflabel", "unit", "lookuptable")
    sT_export()[[sT_export()$export_options$meta_names$items]]#[cols]
  })

  output$centres <- renderTable({
    sT_export()[[sT_export()$export_options$meta_names$centres]]
  })

  output$cases <- renderTable({
    #cols <- c("mnpaid", "mnpvisstartdate")
    sT_export()[[sT_export()$export_options$meta_names$casenodes]]#[cols]
  })

  output$visitplan <- renderTable({
    sT_export()[[sT_export()$export_options$meta_names$visitplan]]
  })
  # end codebook


  output$visit_structure <- renderPlot({
    plot(visit_structure(sT_export()))
  })

  # reactive button
  rdm_cases <- eventReactive(input$create_mon_table, {
    perc <- input$percentage / 100
    return_random_cases(sT_export(), seed = input$seednumber,
                        centres = input$centre,
                        date = input$dateafter,
                        percent = perc)$cases
  })

  output$monitoring_cases <- renderTable({
    rdm_cases()
  })


  output$downloadDataStata <- downloadHandler(
    tdir <- tempdir(),
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      "stata.zip"
    },
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      # Write to a file specified by the 'file' argument
      write_secuTrial(sT_export(), format = "dta", path = tdir)
      dta_loc <- list.files(path = tdir, full.names = TRUE, pattern = "dta$")
      # -j prevents keeping directory structure
      zip(zipfile = file, files = dta_loc, flags = "-j")
    }
  )

  output$downloadDataSas <- downloadHandler(
    tdir <- tempdir(),
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      "sas7bdat.zip"
    },
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      # Write to a file specified by the 'file' argument
      write_secuTrial(sT_export(), format = "sas", path = tdir)
      dta_loc <- list.files(path = tdir, full.names = TRUE, pattern = "sas7bdat$")
      # -j prevents keeping directory structure
      zip(zipfile = file, files = dta_loc, flags = "-j")
    }
  )

  output$downloadDataXpt <- downloadHandler(
    tdir <- tempdir(),
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      "xpt.zip"
    },
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      # Write to a file specified by the 'file' argument
      write_secuTrial(sT_export(), format = "xpt", path = tdir)
      dta_loc <- list.files(path = tdir, full.names = TRUE, pattern = "xpt$")
      # -j prevents keeping directory structure
      zip(zipfile = file, files = dta_loc, flags = "-j")
    }
  )

  output$downloadDataSav <- downloadHandler(
    tdir <- tempdir(),
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      "sav.zip"
    },
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      # Write to a file specified by the 'file' argument
      write_secuTrial(sT_export(), format = "sav", path = tdir)
      dta_loc <- list.files(path = tdir, full.names = TRUE, pattern = "sav$")
      # -j prevents keeping directory structure
      zip(zipfile = file, files = dta_loc, flags = "-j")
    }
  )

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

shinyApp(ui, server)
