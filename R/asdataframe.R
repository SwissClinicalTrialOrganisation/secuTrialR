#' as.data.frame method for secuTrialdata objects
#'
#' @param object secuTrialdata object
#' @param meta logical should metadata be returned
#' @param rm_regex regex syntax to remove from names
#' @param envir environment in which to put the data
#'
#' @return each data.frame on the secuTrialdata object is saved to it's own data.frame in the environment
#' @export
#'
#' @examples
# prepare path to example export
#' export_location <- system.file("extdata",
#'                                "s_export_CSV-xls_CTU05_longnames_sep_ref.zip",
#'                                package = "secuTrialR")
#' # load all export data
#' sT_export <- read_secuTrial_export(data_dir = export_location)
#' # add files to global environment
#' as.data.frame(sT_export)
#' # add files to global environment, removing the project name from the file names
#' as.data.frame(sT_export, rm_regex = "ctu05")
as.data.frame.secuTrialdata <- function(object, meta = TRUE, rm_regex = NULL, envir = .GlobalEnv) {

  datanames <- object$export_options$data_names
  datanames <- as.character(datanames)
  if (!meta) {
    datanames <- datanames[!datanames %in% unlist(object$export_options$meta_names)]
  }
  datanames2 <- datanames
  if (!is.null(rm_regex)) datanames2 <- gsub(rm_regex, "", datanames)

  invisible(
    mapply(
      function(orig_name, new_name){
        assign(new_name, object[[orig_name]], envir = envir)
      },
  datanames, datanames2)
  )

}

