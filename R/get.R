#' Retrieves participants present in secuTrialdata
#'
#' Given a secuTrialdata object, this function simply returns a list of participants.
#' Information included are participant IDs and corresponding study centre information,
#' if available.
#'
#' @param dat secuTrialdata object containing participant IDs and centre information
#' @return data frame containing a list of participants present in dat
#' @export
#'
#' @examples
#'
#' path <- system.file("extdata", "sT_exports", "export_options",
#'                     "s_export_CSV-xls_CTU05_20191003-144349_all_info.zip",
#'                     package = "secuTrialR")
#' sT_export <- read_secuTrial(path)
#'
#' # show participants
#' participants <- get_participants(sT_export)
#'
get_participants <- function(dat){
  if (class(dat) != "secuTrialdata"){
    stop("get_participants requires objects of the class 'secuTrialdata' as input.")
  }

  meta <- unlist(dat$export_options$meta_names)

  if (dat$export_options$add_id & dat$export_options$centre_info){
    participants <- dat[[meta["casenodes"]]][, c("mnppid", "mnpaid", "mnpctrid")]
    centres <- dat[[meta["centres"]]][, c("mnpctrid", "mnpctrname")]
    participants <- suppressMessages(left_join(participants, centres))
  } else if (!dat$export_options$add_id & dat$export_options$centre_info){
    participants <- dat[[meta["casenodes"]]][, c("mnppid", "mnpctrid")]
    centres <- dat[[meta["centres"]]][, c("mnpctrid", "mnpctrname")]
    participants <- suppressMessages(left_join(participants, centres))
  } else if (dat$export_options$add_id & !dat$export_options$centre_info){
    participants <- dat[[meta["casenodes"]]][, c("mnppid", "mnpaid")]
  }
  else{
    participants <- dat[[meta["casenodes"]]][, "mnppid", drop = FALSE]
  }

  return(participants)
}
