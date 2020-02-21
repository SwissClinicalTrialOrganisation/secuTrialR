## mod_visitplan.R

mod_visitplan_UI <- function(id, label) {
  ns <- NS(id)

  tabItem(tabName = label,
          h2("Visit plan"),
          box(
            plotOutput(ns("visit_structure"), height = 500, width = 900),
            width = 1000
          )
  )
}

mod_visitplan <- function(input, output, session, sT_export) {

  output$visit_structure <- renderPlot({
    plot(visit_structure(sT_export()))
  })

}
