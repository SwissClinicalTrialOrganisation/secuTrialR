## app.R ##
library(shinydashboard)
library(shiny)
library(shinyWidgets) # setSliderColor
library(secuTrialR)

# load all module functions
source("R/mod_upload.R")
source("R/mod_recruitplot.R")
source("R/mod_recruittable.R")
source("R/mod_formcomplete.R")
source("R/mod_visitplan.R")
source("R/mod_monitorcn.R")
source("R/mod_codebook.R")
source("R/mod_export.R")

# a list of all module names
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
                        menuItem("STATA - SAS - SPSS", tabName = mod$export, icon = icon("download"))
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        # fill dashboard body contents with module UI functions
                        mod_upload_UI(mod$upload, label = mod$upload),
                        mod_recruitplot_UI(mod$recruitplot, label = mod$recruitplot),
                        mod_recruittable_UI(mod$recruittable, label = mod$recruittable),
                        mod_formcomplete_UI(mod$formcomplete, label = mod$formcomplete),
                        mod_visitplan_UI(mod$visitplan, label = mod$visitplan),
                        mod_monitorcn_UI(mod$monitorcn, label = mod$monitorcn),
                        mod_codebook_UI(mod$codebook, label = mod$codebook),
                        mod_export_UI(mod$export, label = mod$export)
                      )
                    )
)

server <- function(input, output, session) {
  # init the sT export reactive Val
  sT_export <- reactiveVal()
  # call all server modules
  callModule(mod_upload, mod$upload, sT_export)
  callModule(mod_recruitplot, mod$recruitplot, sT_export)
  callModule(mod_recruittable, mod$recruittable, sT_export)
  callModule(mod_formcomplete, mod$formcomplete, sT_export)
  callModule(mod_visitplan, mod$visitplan, sT_export)
  callModule(mod_monitorcn, mod$monitorcn, sT_export)
  callModule(mod_codebook, mod$codebook, sT_export)
  callModule(mod_export, mod$export, sT_export)
}

shinyApp(ui, server)
