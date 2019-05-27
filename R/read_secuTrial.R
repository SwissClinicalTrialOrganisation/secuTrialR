#' Load secutrial export
#' @description Convenience wrapper for \code{read_secuTrial_export}, \code{label_secuTrial}, \code{factorize_secuTrial} and \code{dates_secuTrial}.
#' @param data_dir string - location of the export
#' @param labels logical - add labels to variables and table
#' @param factor logical - convert categorical variables to factor variables (ignored when reference values are not in a separate table)
#' @param dates  logical - convert date variables
#' @param add_id_name string This needs to be specified if your Add-ID name has
#'                    been changed in the AdminTool Design setting.
#'
#' @return secuTrialdata object - a list with one data.frame for each file on the export.
#' @export
#'
#' @examples
#' path <- system.file("extdata",
#'                     "s_export_CSV-xls_CTU05_longnames_sep_ref.zip",
#'                     package = "secuTrialR")
#' d <- read_secuTrial(path)

read_secuTrial <- function(data_dir,
                           labels = TRUE,
                           factor = TRUE,
                           dates = TRUE,
                           add_id_name = NULL){

  d <- read_secuTrial_export(data_dir = data_dir, add_id_name = add_id_name)
  if (labels) d <- label_secuTrial(d)
  if (factor & d$export_options$refvals_separate) d <- factorize_secuTrial(d)
  if (dates) d <- dates_secuTrial(d)

  return(d)

}
