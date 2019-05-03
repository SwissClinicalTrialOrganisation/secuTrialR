#' Get variable labels for secuTrialdata objects
#'
#' @param object \code{secuTrialdata} object
#' @param form which form (string)
#' @details regular expressions are used with \code{form} (specifically, it is appended with \code{$} to identify the form). Consequently, if \code{form} matches multiple forms (because the beginning is different), multiple forms may be returned. You could be more specific with the regular expression, remembering that it is appended by \code{$}.
#' @return named vector
#' @export
#'
#' @examples
#' # prepare path to example export
#' export_location <- system.file("extdata", "s_export_CSV-xls_BMD.zip",
#'                                package = "secuTrialR")
#' # load all export data
#' sT_export <- load_secuTrial_export(data_dir = export_location)
#' # get labels
#' labels <- labels_secuTrial(sT_export)
#' labels[["age"]]
labels_secuTrial <- function(object, form = NULL){
  it <- object[[object$export_options$meta_names$items]]
  if(!is.null(form)){
    qs <- object[[object$export_options$meta_names$questions]]
    itqs <- merge(it, qs, by = "fgid")
    it <- itqs[grepl(paste0(form, "$"), itqs$formtablename), ]
  }
  it <- it[!grepl("Dummy", as.character(it$itemtype)), ]
  it <- it[, c("ffcolname", "fflabel")]
  it <- unique(it)
  it2 <- as.character(it$fflabel)
  names(it2) <- it$ffcolname
  it2
}
