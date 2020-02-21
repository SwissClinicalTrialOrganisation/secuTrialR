## mod_recruittable.R
mod_recruittable_UI <- function(id, label) {
  ns <- NS(id)

  # Third tab content
  tabItem(tabName = label,
          h2(id = ns("title"), "Study recruitment"),
          box(
            tableOutput(ns("annual_recruitment"))
          )
  )
}

mod_recruittable <- function(input, output, session, sT_export) {

  output$annual_recruitment <- renderTable({
    annual_recruitment(sT_export())
  })
}
