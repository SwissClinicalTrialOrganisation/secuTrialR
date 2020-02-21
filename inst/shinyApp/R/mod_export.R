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
