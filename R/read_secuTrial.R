#' Read secuTrial export
#' @description Convenience wrapper for \code{read_secuTrial_raw}, \code{label_secuTrial},
#'              \code{factorize_secuTrial} and \code{dates_secuTrial}.
#' @param data_dir string - location of the export
#' @param labels logical - add labels to variables and table
#' @param factor logical - convert categorical variables to factor variables
#'               (ignored when reference values are not in a separate table)
#' @param dates  logical - convert date variables
#' @param ...    options passed to \code{read_secuTrial_raw}. Can be used for passing options to read.table
#'
#' @return \code{secuTrialdata} object - a list with one data.frame for each file on the export
#'         and a list containing the export options
#' @export
#'
#' @examples
#' export_location <- system.file("extdata", "sT_exports", "lnames",
#'                                "s_export_CSV-xls_CTU05_long_ref_miss_en_utf8.zip",
#'                                package = "secuTrialR")
#' # read all export data
#' sT_export <- read_secuTrial(data_dir = export_location)
#'
read_secuTrial <- function(data_dir,
                           labels = TRUE,
                           factor = TRUE,
                           dates = TRUE,
                           ...) {

  # check for file existence
  if (! file.exists(data_dir)) {
    stop(paste0("There is no file '", data_dir, "'"))
  }

  # read raw export
  tryCatch(
    expr = {
      d <- read_secuTrial_raw(data_dir = data_dir, ...)
      message("Read export successfully.")
      check_export_options(d)
    },
    error = function(e) {
      message("Input to read_secuTrial() appears to be incompatible. It should be in CSV format and not be rectangular.\n  ", e)
    }
  )
  # label
  tryCatch(
    expr = {
      if (labels) d <- label_secuTrial(d)
    },
    error = function(e) {
      message("label_secuTrial() failed. Proceeding without labelling.")
    }
  )
  # factorize
  tryCatch(
    expr = {
      if (factor & d$export_options$refvals_separate) d <- factorize_secuTrial(d)
    },
    error = function(e) {
      message("factorize_secuTrial() failed. Proceeding without factorization.")
    }
  )
  # parse dates
  tryCatch(
    expr = {
      if (dates) d <- dates_secuTrial(d)
    },
    error = function(e) {
      message("dates_secuTrial() failed. Proceeding without date parsing.")
    }
  )
  # return output
  tryCatch(
    expr = {
      return(d)
    },
    error = function(e) {
      message("read_secuTrial() failed.")
    }
  )
}
