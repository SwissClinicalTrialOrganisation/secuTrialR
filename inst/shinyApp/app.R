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
                        menuItem("Visit plan", tabName = mod$visitplan, icon = icon("calendar-alt")),
                        menuItem("Monitoring cases", tabName = mod$monitorcn, icon = icon("dice")),
                        menuItem("Codebook", tabName = mod$codebook, icon = icon("book")),

                        menuItem("STATA - SAS - SPSS", tabName = "download", icon = icon("download"))
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        mod_upload_UI(mod$upload, label = mod$upload),
                        mod_recruitplot_UI(mod$recruitplot, label = mod$recruitplot),
                        mod_recruittable_UI(mod$recruittable, label = mod$recruittable),
                        mod_formcomplete_UI(mod$formcomplete, label = mod$formcomplete),
                        mod_visitplan_UI(mod$visitplan, label = mod$visitplan),
                        mod_monitorcn_UI(mod$monitorcn, label = mod$monitorcn),
                        mod_codebook_UI(mod$codebook, label = mod$codebook),


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
  callModule(mod_visitplan, mod$visitplan, sT_export)
  callModule(mod_monitorcn, mod$monitorcn, sT_export)
  callModule(mod_codebook, mod$codebook, sT_export)






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

}

shinyApp(ui, server)
