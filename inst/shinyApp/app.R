## app.R ##
library(shinydashboard)
library(shiny)
library(shinyWidgets) # setSliderColor
library(secuTrialR)

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
                        menuItem("Upload", tabName = "upload", icon = icon("upload")),
                        menuItem("Recruitment plot", tabName = "recruitmentplot", icon = icon("signal")),
                        menuItem("Recruitment table", tabName = "recruitmenttable", icon = icon("table")),
                        menuItem("Form completeness", tabName = "formcompleteness", icon = icon("percent")),
                        menuItem("Visit plan", tabName = "visitstructure", icon = icon("calendar-alt")),
                        menuItem("Monitoring cases", tabName = "moncases", icon = icon("dice")),
                        menuItem("Codebook", tabName = "codebook", icon = icon("book")),
                        menuItem("Download", tabName = "download", icon = icon("download"))
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        # First tab content
                        tabItem(tabName = "upload",
                                fileInput(inputId = "secuTrial_export_file",
                                          label = "Choose secuTrial export zip",
                                          multiple = FALSE,
                                          accept = c("zip",
                                                     "ziparchive",
                                                     ".zip"), width = 700),
                                textOutput("read_sT_data"),
                                hr(),
                                actionButton(inputId = "use_example_data", label = "Use example data",
                                             icon("lightbulb")),
                                hr(),
                                textOutput("example_sT_data")

                        ),

                        # Second tab content
                        tabItem(tabName = "recruitmentplot",
                                h2("Study recruitment"),
                                box(plotOutput("recruitment_plot", height = 500, width = 900), width = 1000)
                        ),
                        # Third tab content
                        tabItem(tabName = "recruitmenttable",
                                h2("Study recruitment"),
                                box(tableOutput("annual_recruitment"))
                        ),
                        # Fourth tab content
                        tabItem(tabName = "formcompleteness",
                                h2("Form completeness"),
                                checkboxInput(inputId = "percent", label = "Percent", value = TRUE),
                                checkboxInput(inputId = "counts", label = "Counts", value = TRUE),
                                checkboxInput(inputId = "rmat", label = "Remove at forms"),
                                box(tableOutput("form_completeness_perc"), width = 300),
                                box(tableOutput("form_completeness_count"), width = 250)
                        ),
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
                                downloadButton('download_monitoring_cases_csv', 'Cases'),
                                downloadButton('download_monitoring_config_csv', 'Config'),
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
                                h2("Download files"),
                                h4("Download recruitment plot"),
                                downloadButton('downloadDataRecruitmentPlot', 'Download'),
                                h4("Download Stata conversion"),
                                downloadButton('downloadDataStata', 'Download')
                                #h4("Download plots"),
                                #downloadButton('downloadData', 'Download')
                        )
                      )
                    )
)

server <- function(input, output, session) {

  # init the sT export reactive Val
  sT_export <- reactiveVal()

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

  output$recruitment_plot <- renderPlot({
    plot_recruitment(sT_export())
  })

  output$annual_recruitment <- renderTable({
    annual_recruitment(sT_export())
  })

  output$form_completeness_perc <- renderTable({
    if(input$percent) {
      table <- form_status_summary(sT_export())
      names <- names(table)
      names_perc <- names[grepl(names, pattern = ".percent")]
      names_perc <- c("form_name", names_perc)
      if (input$rmat) {
        table <- table[which(! grepl(table$form_name, pattern = "^at")), ]
        table %>% select(names_perc)
      } else {
        table %>% select(names_perc)
      }
    }
  })

  output$form_completeness_count <- renderTable({
    if(input$counts) {
      table <- form_status_summary(sT_export())
      names <- names(table)
      names_count <- names[! grepl(names, pattern = ".percent")]
      if (input$rmat) {
        table <- table[which(! grepl(table$form_name, pattern = "^at")), ]
        table %>% select(names_count)
      } else {
        table %>% select(names_count)
      }
    }
  })

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

  output$downloadDataRecruitmentPlot <- downloadHandler(

    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      "recruitment_plot.pdf"
    },

    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      # Write to a file specified by the 'file' argument
      pdf(file = file)
      plot_recruitment(sT_export())
      dev.off()
    }
  )

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
