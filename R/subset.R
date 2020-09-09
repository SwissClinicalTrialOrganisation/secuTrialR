#' Subsets a \code{secuTrialdata} object
#'
#' Given a \code{secuTrialdata} object, and subsetting parameters,
#' this function filters the data object to include only the desired study participants.
#' Subsetting is possible based on participants and based on centres. In order to subset
#' based on participants, participant IDs (mnpaid) musst be present in the export.
#' In order to subset based on centres, centre information must be included in the export.
#'
#' Subsetting based on participants only, centers only, or based on both is possible. The value of parameter exclude
#' determines whether the output will include participants that meet selection criteria (when exclude = FALSE),
#' or exclude them (when exclude = TRUE). When selecting based on both participants and centres,
#' exclude = FALSE will include the intersection of participants meeting the selection criteria.
#' If exclude = TRUE, a complement of union of participant and centre sets is returned.
#'
#' @param dat \code{secuTrialdata} object containing participant IDs and centre information
#' @param participant character vector with a selection of participant IDs (mnpaid) used for subsetting
#' @param centre character vector with a selection of centre names (mnpctrname) used for subsetting
#' @param exclude boolean which if true excludes participants and centres from dat
#' @return \code{secuTrialdata} object containing only those participants that meet the selection criteria.
#' @importFrom purrr modify_if
#' @export
#'
#' @examples
#'
#' path <- system.file("extdata", "sT_exports", "exp_opt",
#'                     "s_export_CSV-xls_CTU05_all_info.zip",
#'                     package = "secuTrialR")
#' sT <- read_secuTrial(path)
#' participants <- c("RPACK-INS-011", "RPACK-INS-014", "RPACK-INS-015")
#' centres <- c("Inselspital Bern (RPACK)", "Universitätsspital Basel (RPACK)")
#'
#' # show all participants
#' get_participants(sT)
#'
#' # subset sT_export
#' sT_subset1 <- subset_secuTrial(dat = sT, participant = participants)
#' get_participants(sT_subset1)
#' sT_subset2 <- subset_secuTrial(dat = sT, participant = participants, exclude = TRUE)
#' get_participants(sT_subset2)
#' sT_subset3 <- subset_secuTrial(dat = sT, centre = centres, exclude = TRUE)
#' get_participants(sT_subset3)
#' sT_subset4 <- subset_secuTrial(dat = sT, participant = participants,
#'                                centre = centres, exclude = FALSE)
#' get_participants(sT_subset4)
#' sT_subset5 <- subset_secuTrial(dat = sT, participant = participants,
#'                                centre = centres[2], exclude = FALSE)
#' get_participants(sT_subset5)
#'
subset_secuTrial <- function(dat, participant = NULL, centre = NULL, exclude = FALSE) {
  if (class(dat) != "secuTrialdata") {
    stop("subset_secuTrial() requires objects of the class 'secuTrialdata' as input parameter 'dat'.")
  }
  if (!is.null(participant) & !dat$export_options$add_id) {
    stop("No subsetting based on participants possible. Re-export your data with the Add-ID option.")
  }
  if (!is.null(centre) & !dat$export_options$centre_info) {
    stop("No subsetting based on centres possible. Re-export your data with centre info.")
  }
  if (is.null(centre) & is.null(participant)) {
    return(dat)
  }
  meta <- unlist(dat$export_options$meta_names)
  forms <- dat$export_options$data_names[!dat$export_options$data_names %in% meta]
  new_dat <- dat

  if (!is.null(centre)) {
    if (exclude) {
      new_dat[[meta["centres"]]] <- new_dat[[meta["centres"]]][!new_dat[[meta["centres"]]][["mnpctrname"]] %in% centre, ]
      new_dat[[meta["casenodes"]]] <- new_dat[[meta["casenodes"]]][new_dat[[meta["casenodes"]]][["mnpctrid"]] %in%
                                                                     new_dat[[meta["centres"]]][["mnpctrid"]], ]
    } else {
      new_dat[[meta["centres"]]] <- new_dat[[meta["centres"]]][new_dat[[meta["centres"]]][["mnpctrname"]] %in% centre, ]
      new_dat[[meta["casenodes"]]] <- new_dat[[meta["casenodes"]]][new_dat[[meta["casenodes"]]][["mnpctrid"]] %in%
                                                                     new_dat[[meta["centres"]]][["mnpctrid"]], ]
    }
  }

  if (!is.null(participant)) {
    if (exclude) {
      new_dat[[meta["casenodes"]]] <- new_dat[[meta["casenodes"]]][!new_dat[[meta["casenodes"]]][["mnpaid"]]
                                                                   %in% participant, ]
    } else {
      new_dat[[meta["casenodes"]]] <- new_dat[[meta["casenodes"]]][new_dat[[meta["casenodes"]]][["mnpaid"]]
                                                                   %in% participant, ]
    }
  }

  participant_sel <- new_dat[[meta["casenodes"]]][["mnppid"]]

  for (tab in names(new_dat)) {
    if (class(new_dat[[tab]]) != "data.frame") {
      next
    }
    if ("mnppid" %in% names(new_dat[[tab]])) {
      new_dat[[tab]] <- new_dat[[tab]][new_dat[[tab]][["mnppid"]] %in% participant_sel, ]
    } else {
      new_dat[[tab]] <- new_dat[[tab]]
    }
    # make adaptation necessary for subsets based on centres to match exports made without
    # a centre to begin with. Adaptation of object class and attributes needed.
    if (!is.null(centre) & "centre" %in% names(new_dat[[tab]])) {
      new_dat[[tab]] <- modify_if(new_dat[[tab]],
                                  function(x) {
                                    all(is.na(x)) & !any(class(x) %in% c("Date", "POSIXct", "POSIXt", "Datetime", "factor"))
                                  },
                                  as.logical)
      new_dat[[tab]][["centre"]] <- factor(new_dat[[tab]][["centre"]],
                                           levels = new_dat[[meta["centres"]]][["mnpctrname"]])
      new_dat[[tab]][["pat_id"]] <- as.character(new_dat[[tab]][["pat_id"]])
    }
    if ("visit_name" %in% names(new_dat[[tab]])) {
      new_dat[[tab]][["visit_name"]] <- as.character(new_dat[[tab]][["visit_name"]])
    }
    if (nrow(new_dat[[tab]]) > 0) {
      row.names(new_dat[[tab]]) <- 1:nrow(new_dat[[tab]])
    }
  }
  # if according to the export_options it is labelled we relabel
  # we also suppress the warning here because it is not appropriate
  if(new_dat$export_options$labelled) {
    message("If you changed any labels in the secuTrialdata object manually these will be reset to their original state.")
    new_dat <- suppressWarnings(label_secuTrial(new_dat))
  }
  return(new_dat)
}
