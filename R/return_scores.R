#' Returns the score (calculated) items from \code{secuTrialdata} objects
#' @description secuTrial allows to set up calculated fields (i.e. scores) that depend on other items. It is not
#'              suggested to use the scores calculated by secuTrial to perform reliable analyses. To this end,
#'              calling \code{return_scores} will return all items in the secuTrial export which are scores and should
#'              be manually recalculated before data analysis.
#' @param x a \code{secuTrialdata} object
#' @export
#' @return a data.frame (columns: name, itemtype, label) that pinpoints which items are scores/calculated.
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
#' return_scores(sT_export)
#'
return_scores <- function(x) {
  if (class(x) == "secuTrialdata") {
    curr_items <- x[[x$export_options$meta_names$items]]
    scores <- curr_items[grep("calculated|berechnet", curr_items$itemtype), ]
    scores_relevant_col <- scores[, c("ffcolname", "itemtype", "fflabel")]
    names(scores_relevant_col) <- c("name", "itemtype", "label")
    rownames(scores_relevant_col) <- NULL
    scores_relevant_col <- unique(scores_relevant_col)
    return(scores_relevant_col)
  } else {
    stop("return_scores requires objects of the class 'secuTrialdata' as input.")
  }
}
