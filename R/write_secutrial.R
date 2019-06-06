#' Write secuTrial exports to other formats
#' @description Convert the export prepared in R and export it SPSS, Stata or SAS using the haven package.
#' @name write_secuTrial
#' @param object secuTrialdata object
#' @param path directory where the files should be saved
#' @param format format in which to save the export (one of "dta", "sas", "sav", "xpt")
#' @details Due to variable naming limitations in other packages, date variables are appended with _d (rather than _date), datetime/POSIX variables are appended with _dt (rather than _datetime) and factors with _f (rather than _factor).
#' @return a list of filenames
#' @export
#' @examples
#' # prepare path to example export
#' export_location <- system.file("extdata",
#'                                "s_export_CSV-xls_CTU05_longnames_sep_ref.zip",
#'                                package = "secuTrialR")
#' # load all export data
#' sT_export <- read_secuTrial(data_dir = export_location)
#' write_secuTrial(sT_export, format = "dta")
#'
write_secuTrial <- function(object, ...) UseMethod("write_secuTrial", object)
write_secuTrial.secuTrialdata <- function(object, ...){

  x <- object$export_options$data_names
  names(x) <- NULL
  x <- x[!x %in% object$export_options$meta_names]

  lapply(x, function(obs){
    tmp <- object[[obs]]
    write_secuTrial(tmp, filename = obs, ...)
  })

}


write_secuTrial.data.frame <- function(df, filename, path = "", format = "dta", ...){
  df <- secuTrialR:::convertnames(df, format)
  format2 <- format
  if (format == "sas") format2 <- "sas7bdat"
  out <- file.path(path, paste0(filename, ".", format2))
  if (format == "dta") haven::write_dta(df, out, ...)
  if (format == "sav") haven::write_sav(df, out, ...)
  if (format == "sas") haven::write_sas(df, out, ...)
  if (format == "xpt") haven::write_xpt(df, out, ...)
  paste("Saved to", out)
}


convertnames <- function(df, format){
  name <- names(df)
  # if (format %in% c("dta", "sav")){
    name <- gsub("\\.datetime", "_dt", name)
    name <- gsub("\\.date", "_d", name)
    name <- gsub("\\.factor", "_f", name)
  # }
  names(df) <- name
  return(df)
}


