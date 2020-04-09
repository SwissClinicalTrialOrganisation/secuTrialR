#' Retrieves participants present in \code{secuTrialdata}
#'
#' Given a \code{secuTrialdata} object, this function simply returns a list of participants.
#' Information included are participant IDs and corresponding study centre information,
#' if available.
#'
#' @param dat \code{secuTrialdata} object containing participant IDs and centre information
#' @return data.frame containing participants present in dat
#' @importFrom dplyr left_join
#' @export
#'
#' @examples
#'
#' path <- system.file("extdata", "sT_exports", "exp_opt",
#'                     "s_export_CSV-xls_CTU05_all_info.zip",
#'                     package = "secuTrialR")
#' sT_export <- read_secuTrial(path)
#'
#' # show participants
#' participants <- get_participants(sT_export)
#'
get_participants <- function(dat) {
  if (class(dat) != "secuTrialdata") {
    stop("get_participants requires objects of the class 'secuTrialdata' as input.")
  }

  meta <- unlist(dat$export_options$meta_names)

  participants <- dat[[meta["casenodes"]]]
  if (dat$export_options$centre_info) {
    centres <- dat[[meta["centres"]]]
    participants <- suppressMessages(left_join(participants, centres))
  }
  select_columns <- c("mnppid", "mnpaid", "mnpctrid", "mnpctrname")
  participants <- participants[, which(names(participants) %in% select_columns), drop = FALSE]

  return(participants)
}
