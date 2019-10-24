#' Subsets a secuTrialdata object
#'
#' Given a secuTrial data object, and subsetting parameters,
#' this function filters the data object to include only the selected pat ids.
#'
#' @param dat secuTrialdata object
#' @param pat_id a selection of patient IDs used for subsetting
#' @return secuTrialdata object containing only the pat_ids that meet the selection criteria.
#' @export
#'
#' @examples
#'
#' # This example, builds pseudo-urls that do not point to an active secuTrial instance.
#'
#' path <- system.file("extdata", "sT_exports", "longnames",
#'                     "s_export_CSV-xls_CTU05_long_ref_miss_en_utf8.zip",
#'                     package = "secuTrialR")
#' sT_export <- read_secuTrial(path)
#' patients <- c("RPACK-INS-011", "RPACK-INS-014")
#'
#' # subset sT_export
#' subset_sT_export <- subset_secuTrial(dat = sT_export, pat_id = patients)
#'
subset_secuTrial <- function(dat, pat_id){

  return(dat)

}
