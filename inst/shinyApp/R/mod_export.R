# Export module UI function
mod_export_UI <- function(id, label) {
  ns <- NS(id)
  # Last tab content / Download
  tabItem(tabName = label,
          h2("Download data conversion archives"),
          br(),
          h4("Download STATA"),
          downloadButton(ns("downloadDataStata"), "Download dta zip archive"),
          br(),
          br(),
          h4("Download SAS"),
          downloadButton(ns("downloadDataSas"), "Download sas7bdat zip archive"),
          br(),
          br(),
          downloadButton(ns("downloadDataXpt"), "Download xpt v8 zip archive"),
          br(),
          br(),
          h4("Download SPSS"),
          downloadButton(ns("downloadDataSav"), "Download sav zip archive")
  )
}

# Export module server function
mod_export <- function(input, output, session, sT_export) {

  output$downloadDataStata <- downloader(file_name = "stata.zip", format = "dta", sT_export)
  output$downloadDataSas <- downloader(file_name = "sas7bdat.zip", format = "sas", sT_export)
  output$downloadDataXpt <- downloader(file_name = "xpt.zip", format = "xpt", sT_export)
  output$downloadDataSav <- downloader(file_name = "sav.zip", format = "sav", sT_export)

}


downloader <- function(file_name, format, sT_export) {

  downloadHandler(
    tdir <- tempdir(),
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      file_name
    },
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      # Write to a file specified by the 'file' argument
      write_secuTrial(sT_export(), format = format, path = tdir)
      # exception handling for sas7bdat
      if (format == "sas") format <- "sas7bdat"
      dta_loc <- list.files(path = tdir, full.names = TRUE, pattern = paste0(format, "$"))
      # -j prevents keeping directory structure
      zip(zipfile = file, files = dta_loc, flags = "-j")
    }
  )

}
