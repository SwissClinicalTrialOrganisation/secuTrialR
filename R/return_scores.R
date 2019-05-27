#' Returns the score (calculated) items from secuTrialdata objects
#' @description secuTrial allows to set up calculated fields (i.e. scores) that depend on other items. It is not
#'              suggested to use the scores calculated by secuTrial to perform reliable analyses. To this end,
#'              calling \code{return_scores} will return all items in the secuTrial export which are scores and should
#'              be manually recalculated before data analysis.
#' @param x a \code{secuTrialdata} object
#' @export
#' @details return_scores will produce a data.frame that pinpoints which items are scores/calculated.
#'
#' @examples
#' # export location
#' expot_loc <- system.file("extdata",
#'                          "s_export_CSV-xls_CTU05_longnames_sep_ref.zip",
#'                          package = "secuTrialR")
#' # read export
#' sT_export <- read_secuTrial(expot_loc)
#'
#' # return scores
#' return_scores(sT_export)
#'
return_scores <- function(x) {
  if (class(x) == "secuTrialdata") {
    curr_items <- data.frame(x[x$export_options$meta_names$items])
    itemtype_col <- grep("itemtype", names(curr_items))
    scores <- curr_items[grep("calculated", curr_items[, itemtype_col]), ]
    relevant_cols <- grep("ffcolname|itemtype|fflabel", names(scores))
    scores_relevant_col <- scores[, relevant_cols]
    names(scores_relevant_col) <- c("name", "itemtype", "label")
    rownames(scores_relevant_col) <- NULL
    scores_relevant_col <- unique(scores_relevant_col)
    return(scores_relevant_col)
  } else {
    stop("return_scores requires objects of the class 'secuTrialdata' as input.")
  }
}
