#' Subsets a secuTrialdata object
#'
#' Given a secuTrial data object, and subsetting parameters,
#' this function filters the data object to include only the selected pat ids.
#'
#' @param dat secuTrialdata object
#' @param patient character vector with a selection of patient IDs (mnpaid) used for subsetting
#' @param centre character vector with a selection of centre IDs (mnpctrid) used for subsetting
#' @param exclude boolean which if true excludes given pat_id and centre from dat
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
#' patient <- c("RPACK-INS-011", "RPACK-INS-014")
#' centre <- c("Inselspital Bern (RPACK)")
#'
#' # subset sT_export
#' subset_sT_export <- subset_secuTrial(dat = sT_export, pat_id = patient)
#'
subset_secuTrial <- function(dat, patient = NULL, centre = NULL, exclude = FALSE){

  if(!is.null(patient) & !dat$export_options$add_id){
    stop("No subsetting based on patient ids possible. Re-export your data with the Add-ID option.")
  }
  if(is.null(centre) & is.null(patient)){
    return(dat)
  }
  meta <- unlist(dat$export_options$meta_names)
  forms <- dat$export_options$data_names[!dat$export_options$data_names %in% meta]
  new_dat <- dat

  if(!is.null(centre)){
    if(exclude){
      new_dat[[meta["centres"]]] <- new_dat[[meta["centres"]]][!new_dat[[meta["centres"]]][["mnpctrid"]] %in% centre, ]
      new_dat[[meta["casenodes"]]] <- new_dat[[meta["casenodes"]]][!new_dat[[meta["casenodes"]]][["mnpctrid"]] %in% centre, ]
    } else {
      new_dat[[meta["centres"]]] <- new_dat[[meta["centres"]]][new_dat[[meta["centres"]]][["mnpctrid"]] %in% centre, ]
      new_dat[[meta["casenodes"]]] <- new_dat[[meta["casenodes"]]][new_dat[[meta["casenodes"]]][["mnpctrid"]] %in% centre, ]
    }
  }

  if(!is.null(patient)){
    if (exclude) {
      new_dat[[meta["casenodes"]]] <- new_dat[[meta["casenodes"]]][!new_dat[[meta["casenodes"]]][["mnpaid"]] %in% patient, ]
    } else {
      new_dat[[meta["casenodes"]]] <- new_dat[[meta["casenodes"]]][new_dat[[meta["casenodes"]]][["mnpaid"]] %in% patient, ]
    }
    #centres_present <- unique(new_dat[[meta[["casenodes"]]]][["mnpctrid"]])
    #new_dat[[meta["centres"]]] <- new_dat[[meta["centres"]]][new_dat[[meta["centres"]]][["mnpctrid"]] %in% centres_present, ]
  }

  patient_sel <- new_dat[[meta["casenodes"]]][["mnppid"]]

  for(tab in names(new_dat)){
    if (class(new_dat[[tab]]) != "data.frame"){
      next
    }
    if("mnppid" %in% names(new_dat[[tab]])){
      new_dat[[tab]] <- new_dat[[tab]][new_dat[[tab]][["mnppid"]] %in% patient_sel, ]
    } else {
      new_dat[[tab]] <- new_dat[[tab]]
    }
    ## make adaptation necessary to match exports without a centre (object class, attributes)
    if(!is.null(centre) & "centre" %in% names(new_dat[[tab]])){
      new_dat[[tab]] <- modify_if(new_dat[[tab]],
                                  function(x) all(is.na(x)) & !any(class(x) %in% c("Date", "POSIXct", "POSIXt", "Datetime", "factor")),
                                  as.logical)
      new_dat[[tab]][["centre"]] <- factor(new_dat[[tab]][["centre"]],
                                              levels = new_dat[[meta["centres"]]][["mnpctrname"]])
      new_dat[[tab]][["pat_id"]] <- as.character(new_dat[[tab]][["pat_id"]])
    }
    if("visit_name" %in% names(new_dat[[tab]])){
      new_dat[[tab]][["visit_name"]] <- as.character(new_dat[[tab]][["visit_name"]])
    }
    if(nrow(new_dat[[tab]]) > 0){
      row.names(new_dat[[tab]]) <- 1:nrow(new_dat[[tab]])
    }
  }
  return(new_dat)
}
