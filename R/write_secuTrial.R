#' Write secuTrial exports to other formats
#' @description Convert the export prepared in R and export it to
#'              SPSS (sav), Stata (dta) or SAS (sas, xpt version 8)
#'              using the haven package.
#' @name write_secuTrial
#' @param object \code{secuTrialdata} object
#' @param ... further parameters
#' @details Due to variable naming limitations in other packages, date variables are
#'          appended with _d (rather than _date), datetime/POSIX variables are appended
#'          with _dt (rather than _datetime) and factors with _f (rather than _factor).
#'          Further variable names may be altered in the conversion process.
#'          For details please refer to the \code{haven} documentation.
#' @return a list of filenames
#' @export
#' @import haven
#' @examples
#' # prepare path to example export
#' export_location <- system.file("extdata", "sT_exports", "lnames",
#'                                "s_export_CSV-xls_CTU05_long_ref_miss_en_utf8.zip",
#'                                package = "secuTrialR")
#' # load all export data
#' sT_export <- read_secuTrial(data_dir = export_location)
#' tdir <- tempdir()
#' write_secuTrial(sT_export, format = "dta", path = tdir)
#' list.files(tdir)
#'
write_secuTrial <- function(object, ...) UseMethod("write_secuTrial", object)
#' @export
#' @name write_secuTrial
#' @param format format in which to save the export (one of "dta", "sas", "sav", "xpt")
#' @param metadata if TRUE then metadate files will also be written
write_secuTrial.secuTrialdata <- function(object, format = "dta", metadata = FALSE, ...) {

  if (! format %in% c("dta", "sas", "sav", "xpt")) {
    stop(paste0("format must be one of 'dta', 'sas', 'sav', 'xpt'. You specified: ", format))
  }
  x <- object$export_options$data_names
  names(x) <- NULL
  if (!metadata) x <- x[!x %in% object$export_options$meta_names]

  lapply(x, function(obs) {
    tmp <- object[[obs]]
    write_secuTrial(tmp, filename = obs, format = format, ...)
  })
}

#' @name write_secuTrial
#' @param df a data.frame
#' @param filename file name
#' @param path directory where the files should be saved
write_secuTrial.data.frame <- function(df, filename, path = "", format = "dta", ...) {
  df <- convertnames(df, format)
  format2 <- format
  if (format == "sas") format2 <- "sas7bdat"
  out <- file.path(path, paste0(filename, ".", format2))
  if (format == "dta") haven::write_dta(df, out, ...)
  if (format == "sav") haven::write_sav(df, out, ...)
  if (format == "sas") haven::write_sas(df, out, ...)
  if (format == "xpt") haven::write_xpt(df, out, version = 8, ...)
  paste("Saved to", out)
}
