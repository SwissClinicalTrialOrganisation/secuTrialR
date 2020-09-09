#'
#' This function loads a multi-page secuTrial 'Validation Overview' report into an R tibble.
#'
#' @export read_validation_overview
#'
#' @param data_dir Path to the Validation Overview (must be an *.xlsx file).
#' @param skip Equivalent parameter in read_excel().
#'             The validation overview xlsx files contain some information in the first
#'             few lines of each sheet which need to be skipped in order to produce the
#'             correct header in R. Prior to reading the validation overview with read_validation_overview()
#'             it is likely a good idea to check how many lines need to be skipped. This
#'             parameter has been added because the amount of lines can differ between different
#'             versions of secuTrial.
#'
#' @return tibble with the 'Validation Overview' data
#' @export read_validation_overview
#' @importFrom readxl read_excel
#' @importFrom readxl excel_sheets
#' @importFrom dplyr bind_rows
#' @examples
#' val_ovv_location <- system.file("extdata", "sT_exports", "BMD",
#'                                 "bmd_validation_overview.xlsx",
#'                                 package = "secuTrialR")
#' val_ovv <- read_validation_overview(data_dir = val_ovv_location)
#'
read_validation_overview <- function(data_dir, skip = 7) {

  last_four <- substr(data_dir, start = (nchar(data_dir) - 3), stop = nchar(data_dir))
  if (! last_four == "xlsx") {
    stop("Your Validation Overview file does not appear to be an *.xlsx. Please supply an *.xlsx.")
  }
  # init
  full_val_rep <- c()
  # load sheet names
  val_rep_sheet_names <- excel_sheets(path = data_dir)
  for (name in val_rep_sheet_names) {
    # first lines are not informative and can be skipped
    val_rep_sheet <- read_excel(path = data_dir, skip = skip, sheet = name)
    # first line is empty and can be removed
    val_rep_sheet <- val_rep_sheet[-1, ]
    full_val_rep <- bind_rows(full_val_rep, val_rep_sheet)
  }
  return(full_val_rep)
}
