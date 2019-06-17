#' Load secuTrial export
#' @description Convenience wrapper for \code{\link{read_secuTrial_export}}, \code{\link{label_secuTrial}}, \code{\link{factorize_secuTrial}}, \code{\link{dates_secuTrial}} and \code{\link{durations_secuTrial}}.
#' @param data_dir string - location of the export
#' @param labels logical - add labels to variables and table
#' @param factor logical - convert categorical variables to factor variables (ignored when reference values are not in a separate table)
#' @param dates  logical - convert date variables
#' @param format_durations  logical - format duration variables
#'
#' @return secuTrialdata object - a list with one data.frame for each file on the export.
#' @seealso \code{\link{read_secuTrial_export}}, \code{\link{label_secuTrial}}, \code{\link{factorize_secuTrial}}, \code{\link{dates_secuTrial}}, \code{\link{durations_secuTrial}}
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
                           format_durations = TRUE) {

  d <- read_secuTrial_export(data_dir = data_dir)
  if (labels) d <- label_secuTrial(d)
  if (factor & d$export_options$refvals_separate) d <- factorize_secuTrial(d)
  if (dates) d <- dates_secuTrial(d)
  if (format_durations) d <- durations_secuTrial(d)

  return(d)

}
