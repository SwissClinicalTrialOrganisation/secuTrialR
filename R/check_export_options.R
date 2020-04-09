#' Returns deviations from suggested export options
#'
#' Given a \code{secuTrialdata} object, this function returns information on deviations
#' from suggested export options.
#'
#' @param dat \code{secuTrialdata} object
#' @details While the package strives to allow loading of as many types of secuTrial data exports
#'          as possible, there are certain export options which are less likely to cause issues.
#'          If possible it is suggested to export data which adheres to a suggested option set.
#'          This function points out deviations from the suggested set of options which are: \cr
#'          is_zip == TRUE \cr
#'          refvals_separate == TRUE \cr
#'          add_id == TRUE \cr
#'          duplicate_meta == FALSE \cr
#'          encoding == "UTF-8" \cr
#'          form_status == TRUE \cr
#'          centre_info == TRUE \cr
#'          proj_setup == TRUE \cr
#'          dict_items$lang == "en" \cr
#'          hidden_fields == FALSE \cr
#'          structure == TRUE
#'
#' @examples
#' path <- system.file("extdata", "sT_exports", "exp_opt",
#'                     "s_export_CSV-xls_CTU05_only_column_names.zip",
#'                     package = "secuTrialR")
#' sT_export <- read_secuTrial_raw(path)
#'
#' secuTrialR:::check_export_options(sT_export)
check_export_options <- function(dat) {
  if (class(dat) != "secuTrialdata") {
    stop("check_export_options requires objects of the class 'secuTrialdata' as input.")
  }
  eo <- dat$export_options
  warn_components <- ""
  if (! eo$is_zip) {
    warn_components <- paste0(warn_components, "Export is not zipped.\n")
  }
  if (! eo$refvals_separate) {
    warn_components <- paste0(warn_components, "Reference values are not stored in separate table.\n")
  }
  if (! eo$add_id) {
    warn_components <- paste0(warn_components, "Add-ID is not part of the export.\n")
  }
  if (eo$duplicate_meta) {
    warn_components <- paste0(warn_components, "Meta data is duplicated into all tables.\n")
  }
  if (eo$encoding != "UTF-8") {
    warn_components <- paste0(warn_components, "Character encoding is not set to UTF-8.\n")
  }
  if (! eo$form_status) {
    warn_components <- paste0(warn_components, "Form status is not part of the export.\n")
  }
  if (! eo$centre_info) {
    warn_components <- paste0(warn_components, "Centre information is not part of the export.\n")
  }
  if (! eo$proj_setup) {
    warn_components <- paste0(warn_components, "Project setup is not part of the export.\n")
  }
  if (eo$dict_items$lang != "en") {
    warn_components <- paste0(warn_components, "Language is not English.\n")
  }
  if (eo$hidden_fields) {
    warn_components <- paste0(warn_components, "Data from hidden fields is part of the export.\n")
  }
  if (! eo$structure) {
    warn_components <- paste0(warn_components, "Structure information is not part of the export.\n")
  }

  if (str_length(warn_components)) {
    warn_start <- "The following export options deviate from the suggested specifications:\n"
    warn <- paste0(warn_start, warn_components)
    message(warn)
  }
}
