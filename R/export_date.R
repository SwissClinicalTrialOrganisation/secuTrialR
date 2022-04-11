#' Return the date and time that an export was performed
#'
#' @param x secuTrial export
#' @rdname export_date
#' @return \code{export_date} returns a date object, \code{export_datetime} returns a string
#' @export
#'
#' @examples
#' export_location <- system.file("extdata", "sT_exports", "lnames",
#'                                "s_export_CSV-xls_CTU05_long_ref_miss_en_utf8.zip",
#'                                package = "secuTrialR")
#' # read all export data
#' sT_export <- read_secuTrial(data_dir = export_location)
#' # get date/time
#' export_datetime(sT_export)
#' export_date(sT_export)
export_datetime <- function(x){
  if (class(x)[1] != "secuTrialdata") stop("'secuTrialdata object required'")

  x$export_options$time_of_export
}
#' @rdname export_date
#' @export
export_date <- function(x){
  if (class(x)[1] != "secuTrialdata") stop("'secuTrialdata object required'")

  as.Date(substr(x$export_options$time_of_export, 1, 10), "%d.%m.%Y")
}


