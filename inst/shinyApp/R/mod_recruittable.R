## mod_recruittable.R
# Recruitment table module UI function
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

# Recruitment table module server function
mod_recruittable <- function(input, output, session, sT_export) {

  output$annual_recruitment <- renderTable({
    annual_recruitment(sT_export())
  })
}
