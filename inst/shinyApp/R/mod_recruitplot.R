## mod_recruitplot.R

mod_recruitplot_UI <- function(id, label) {
  ns <- NS(id)

  tabItem(tabName = label,
          h2(id = ns("title"), "Study recruitment"),
          box(
            plotOutput(ns("recruitment_plot"), height = 500, width = 900), width = 1000
          ),
          downloadButton(ns("downloadDataRecruitmentPlot"), "Download pdf")
  )
}

mod_recruitplot <- function(input, output, session, sT_export) {

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

  output$recruitment_plot <- renderPlot({
    plot_recruitment(sT_export())
  })

}
