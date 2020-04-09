#' Returns hidden items (variables) from \code{secuTrialdata} objects
#' @description Sometimes, during a study, certain fields may be hidden because data should
#'              no longer be entered into them. If this is the case and the data of these
#'              fields is part of your export is likely good to know about it.
#'
#' @param x a \code{secuTrialdata} object
#' @export
#' @return a data.frame (columns: name, itemtype, label) that pinpoints which items are hidden
#'
#' @examples
#' # export location
#' expot_loc <- system.file("extdata", "sT_exports", "lnames",
#'                          "s_export_CSV-xls_CTU05_long_ref_miss_en_utf8.zip",
#'                          package = "secuTrialR")
#' # read export
#' sT_export <- read_secuTrial(expot_loc)
#'
#' # return scores
#' return_hidden_items(sT_export)
#'
return_hidden_items <- function(x) {
  if (class(x) == "secuTrialdata") {

    if (! x$export_options$hidden_fields) {
      stop("Form data of hidden fields was not exported and will thus not be in this export.")
    }

    curr_items <- x[[x$export_options$meta_names$items]]
    hidden_vars <- curr_items[which(curr_items$hidden == "yes"), ]
    hidden_vars_relevant <- hidden_vars[, c("ffcolname", "itemtype", "fflabel")]
    names(hidden_vars_relevant) <- c("name", "itemtype", "label")
    rownames(hidden_vars_relevant) <- NULL
    hidden_vars_relevant <- unique(hidden_vars_relevant)
    return(hidden_vars_relevant)
  } else {
    stop("return_hidden_items requires objects of the class 'secuTrialdata' as input.")
  }
}
