#' Data dictionary for secuTrialdata objects
#'
#' @param x \code{secuTrialdata} object
#'
#' @return data frame with info on variables
#' @export
#'
#' @examples
#' export_location <- system.file("extdata", "sT_exports", "lnames",
#'                                "s_export_CSV-xls_CTU05_long_ref_miss_en_utf8.zip",
#'                                package = "secuTrialR")
#' # read all export data
#' sT_export <- read_secuTrial(data_dir = export_location)
#' # get dictionary
#' dictionary_secuTrial(sT_export)
dictionary_secuTrial <- function(x){
  if (class(x)[1] != "secuTrialdata") stop("'secuTrialdata object required'")
  tmp <- merge(unique(x[[x$export_options$meta_names$items]][, c("fgid", "ffcolname", "itemtype", "fflabel", "unit")]),
               unique(x[[x$export_options$meta_names$questions]][, c("fgid", "formtablename", "formname")]),
               all.x = TRUE, by = "fgid")
  dict <- merge(unique(tmp[, c("formtablename", "formname", "ffcolname", "itemtype", "fflabel", "unit")]),
                unique(x[[x$export_options$meta_names$forms]][, c("formtablename", "formname", "formfamily")]),
                by = c("formtablename", "formname"))
  return(dict)
}
