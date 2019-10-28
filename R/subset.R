#' Subsets a secuTrialdata object
#'
#' Given a secuTrial data object, and subsetting parameters,
#' this function filters the data object to include only the selected pat ids.
#'
#' @param dat secuTrialdata object
#' @param pat_id character vector with a selection of patient IDs (mnpaid) used for subsetting
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
#' patients <- c("RPACK-INS-011", "RPACK-INS-014")
#' centre <- c("Inselspital Bern (RPACK)")
#'
#' # subset sT_export
#' subset_sT_export <- subset_secuTrial(dat = sT_export, pat_id = patients)
#'
subset_secuTrial <- function(dat, ...,  exclude = FALSE){
  arg <- list(...)
  if(!all(names(arg) %in% c("pat_id", "centre"))){
    stop("subsetting parameters must be either 'pat_id', or 'centre'")
  }

  meta <- unlist(dat$export_options$meta_names)
  forms <- dat$export_options$data_names[!dat$export_options$data_names %in% meta]

  patients <- arg$pat_id
  centre <- arg$centre

  new_dat <- vector("list", length(dat))
  names(new_dat) <- names(dat)

  if(is.null(centre) & is.null(patients)){
    return(dat)
  }

  if(!is.null(centre)){
    if(exclude){
      new_dat[[meta["centres"]]] <- dat[[meta["centres"]]][which(!dat[[meta["centres"]]][["mnpctrid"]] %in% centre), ]
      new_dat[[meta["casenodes"]]] <- dat[[meta["casenodes"]]][which(!dat[[meta["casenodes"]]][["mnpctrid"]] %in% centre), ]
    } else {
      new_dat[[meta["centres"]]] <- dat[[meta["centres"]]][which(dat[[meta["centres"]]][["mnpctrid"]] %in% centre), ]
      new_dat[[meta["casenodes"]]] <- dat[[meta["casenodes"]]][which(dat[[meta["casenodes"]]][["mnpctrid"]] %in% centre), ]
    }
  }

  if(!is.null(patients)){
    if (exclude) {
      new_dat[[meta["casenodes"]]] <- dat[[meta["casenodes"]]][which(!dat[[meta["casenodes"]]][["mnpaid"]] %in% patients), ]
    } else {
      new_dat[[meta["casenodes"]]] <- dat[[meta["casenodes"]]][which(dat[[meta["casenodes"]]][["mnpaid"]] %in% patients), ]
    }
  }

  patients_sel <- new_dat[[meta["casenodes"]]][["mnppid"]]

  for(tab in names(new_dat)){
    if("mnppid" %in% names(dat[[tab]])){
      new_dat[[tab]] <- dat[[tab]][which(dat[[tab]][["mnppid"]] %in% patients_sel), ]
    } else {
      new_dat[[tab]] <- dat[[tab]]
    }
  }

  new_dat$export_options <- dat$export_options

  return(new_dat)
}
