#'
#' Asses completeness of data for each variable in a secuTrial export
#'
#' @description
#'
#' NOTE: This is not exported currently since it is not generic enough.
#'       It can still be used but in depth knowledge of the function is required
#'       to evaluate if the result is correct.
#'       As a rule of thumb: rigid CDMA setups with low complexity will likely
#'                           work as expected.
#'
#' Variable completeness is defined as percentage of data entered for a variable
#' when considering all occurrences of the variable throughout the visit plan and
#' all participants registered in the secuTrial data base. The completeness can
#' be assessed for 'allforms', which will also take unsaved forms into account, and
#' for 'savedforms' which will only consider forms that have been actively saved.
#' In order to asses variable completeness the function requires a loaded secuTrial
#' validation overview and a loaded secuTrial standard export. Both the validation
#' overview and the data export should be from the same time (not more than minutes
#' apart if possible) to ensure data integrity.
#' Variable completeness is considered based on rules in the secuTrial setup. Thus,
#' if a variable is not marked as necessary for completeness, this variable will
#' always be considered as 100 percent complete if only 'savedforms' are assessed.
#' Please note that variable completeness for repetition forms should only be
#' assessed for saved forms.
#'
# #' @export assess_form_variable_completeness
#'
#' @param form data.frame Form for which to assess variable completeness
#'             (i.e. a list element returned by read_secuTrial_raw).
#' @param casenodes_table data.frame The central casenodes record file
#'                      (i.e. 'casenodes' from the list returned by read_secuTrial_raw).
#' @param validation_overview tibble returned by read_validation_overview.
#' @param completeness string Specifies if completeness is assessed for all forms ('allforms')
#'                     or only for saved forms ('savedforms').
#' @param occ_in_vp integer Specifies how often the form occurs in the visit plan
#'                  (only relevant if completeness = 'allforms' is set).
#' @param omit_mnp boolean Removes variable names from the result table that start with mnp.
#' @return data.frame showing percent completeness per variable.
#' @importFrom tibble rownames_to_column
#' @examples
#' # prepare path to example export
#' export_location <- system.file("extdata", "sT_exports", "BMD",
#'                                "s_export_CSV-xls_BMD_short_en_utf8.zip",
#'                                package = "secuTrialR")
#' # read all export data
#' sT_export <- read_secuTrial_raw(data_dir = export_location)
#'
#' # read validation overview
#' val_ovv_location <- system.file("extdata", "sT_exports", "BMD",
#'                                 "bmd_validation_overview.xlsx",
#'                                 package = "secuTrialR")
#' val_ovv <- read_validation_overview(data_dir = val_ovv_location)
#'
#' secuTrialR:::assess_form_variable_completeness(form = sT_export$bmd,
#'                                                casenodes_table = sT_export$cn,
#'                                                validation_overview = val_ovv,
#'                                                completeness = "allforms",
#'                                                occ_in_vp = 5)
#'
#' @seealso read_validation_overview, read_secuTrial_raw
#'
assess_form_variable_completeness <- function(form,
                                              casenodes_table,
                                              validation_overview,
                                              completeness = "allforms",
                                              occ_in_vp = 1,
                                              omit_mnp = TRUE) {
  # form must appear at least once in visit plan
  if (occ_in_vp < 1) {
    stop(paste("occ_in_vp is set to", occ_in_vp, "but can not be smaller than 1."))
  }
  # occ_in_vp is obsolete if "savedforms" is specified
  if (occ_in_vp > 1 & completeness == "savedforms") {
    warning(paste("occ_in_vp set to", occ_in_vp, "ignored because completeness has been set to 'savedforms'."))
  }
  # check if c("Completion status", "Column") are available in the validation overview they contain
  # the key data to make the assessment and are not checked by default in secuTrial
  if (! all(c("Completion status", "Column") %in% names(validation_overview))) {
    stop(paste("'Completion status' and 'Column' are not available in the supplied validation overview but are needed.",
               "Reexport your validation overview in secuTrial with these two columns displayed.'"))
  }
  # check for "allforms" and a position column in the form
  if (any("position" %in% names(form)) & completeness == "allforms") {
    stop(paste("The 'position' column in your form indicates a repetition form.",
         "You should set the completeness parameter to 'savedforms'."))
  }

  if (completeness == "allforms") {
    # count participants (all registered ids)
    forms_per_participant_count <- length(casenodes_table[, 1])
    # multiply forms_per_participant_count with form occurence in
    # visit plan to adjust for repeating forms
    forms_per_participant_count <- forms_per_participant_count * occ_in_vp
    # count form entries
    saved_form_count <- length(form[, 1])
    # count empty forms - frozen forms appear as "rule-based freeze"
    # and are thus not counted as missing
    empty_form_count <- forms_per_participant_count - saved_form_count
    # consistency check, empty_form_count cannot be negative
    if (empty_form_count < 0) {
      stop(paste0("You have likely not specified occ_in_vp correctly. You specified: ", occ_in_vp))
    }
    # form table
    form_table <- table(names(form))
    # init empty counts
    form_table[names(form_table)] <- empty_form_count
    # init table_missing_counts
    table_missing_counts <- form_table
    # validation overview as table
    validation_overview_table <- table(validation_overview$Column)
    if (length(validation_overview_table) != 0) {
      # missing count per variable
      table_missing_counts[names(validation_overview_table)] <-
        table_missing_counts[names(validation_overview_table)] +
        validation_overview_table[names(validation_overview_table)]
    }
    # change to dataframe
    table_missing_counts <- as.data.frame(as.matrix(table_missing_counts))
    # add completeness
    table_missing_counts$completeness <- 1 - (table_missing_counts$V1 / forms_per_participant_count)
    # table names and formatting
    table_missing_counts <- rownames_to_column(table_missing_counts)
    names(table_missing_counts) <- c("variable", "timesmissing", "completeness")
    # validation report also has entries for other forms
    # which return double NA and can be removed
    non_na_idx <- which(! (is.na(table_missing_counts$timesmissing) &
                           is.na(table_missing_counts$completeness))
                       )
    table_missing_counts <- table_missing_counts[non_na_idx, ]
    # add timesentered column
    table_missing_counts$timesentered <- as.vector(colSums(! is.na(form[, table_missing_counts$variable])))
    table_missing_counts <- table_missing_counts[, c("variable", "timesentered", "timesmissing", "completeness")]
  } else if (completeness == "savedforms") {

    # count form entries
    saved_form_count <- length(form[, 1])
    # validation overview as table
    validation_overview_table <- table(validation_overview$Column)
    if (length(validation_overview_table) == 0) {
      return("100% completeness. Your Validation overview is empty.")
    }
    # missing count per variable
    table_missing_counts <- na.omit(as.data.frame(validation_overview_table[names(form)]))
    # add completeness
    table_missing_counts$completeness <- 1 - (table_missing_counts$Freq / saved_form_count)
    # table names and formatting
    names(table_missing_counts) <- c("variable", "timesmissing", "completeness")
    # add timesentered column
    table_missing_counts$timesentered <- as.vector(colSums(! is.na(form[, as.character(table_missing_counts$variable)])))
    table_missing_counts <- table_missing_counts[, c("variable", "timesentered", "timesmissing", "completeness")]
  } else {
    stop(paste0("completeness argument must be 'allforms' or 'savedforms'. You specified: ", "'", completeness, "'"))
  }

  # remove variables starting with mnp
  if (omit_mnp) {
    mnp_idx <- grep("^mnp", table_missing_counts$variable)
    if (length(mnp_idx) > 1) {
        table_missing_counts <- table_missing_counts[-mnp_idx, ]
    }
  }
  # for "allforms" remove "centre", "pat_id", "sigstatus", "visit_name"
  if (completeness == "allforms") {
    omit_idx <- which(table_missing_counts$variable %in% c("centre", "pat_id", "sigstatus", "visit_name"))
    table_missing_counts <- table_missing_counts[-omit_idx, ]
  }
  # sort
  table_missing_counts$variable <- as.character(table_missing_counts$variable)
  table_missing_counts <- table_missing_counts[order(table_missing_counts$variable), ]
  row.names(table_missing_counts) <- NULL
  return(table_missing_counts)
}
