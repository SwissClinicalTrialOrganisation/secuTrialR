## mod_codebook.R

mod_codebook_UI <- function(id, label) {
  ns <- NS(id)
  # seventh tab codebook
  tabItem(tabName = label,
          h2("Codebook"),
          tags$style(HTML(".tabbable > .nav > li > a                  {background-color: #444;  color:white}
                                                 .tabbable > .nav > li[class=active]    > a {background-color: #dd4b39; color:white}")),
          tabsetPanel(
            tabPanel("Forms", tableOutput(ns("forms"))),
            tabPanel("Questions", tableOutput(ns("questions"))),
            tabPanel("Items", tableOutput(ns("items"))),
            tabPanel("Centres", tableOutput(ns("centres"))),
            tabPanel("Cases", tableOutput(ns("cases"))),
            tabPanel("Visitplan", tableOutput(ns("visitplan")))
          )


  )
}

mod_codebook <- function(input, output, session, sT_export) {
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

}
