#' A function to assess the status of forms
#'
#' This function returns a data.frame informing on the status of
#' forms per patient. There is a line for every combination of
#' form type and patient id. The numbers are occurence counts.
#'
#' @param object secuTrialdata object
#' @keywords form status, completeness
#' @export
#' @examples
#' # prepare path to example export
#' export_location <- system.file("extdata",
#'                                "s_export_CSV-xls_BMD.zip",
#'                                package = "secuTrialR")
#' # load all export data
#' sT_export <- read_secuTrial_export(data_dir = export_location)
#'
#' # get form status
#' form_status_summary(sT_export)
#'
form_status_counts <- function(object) {

  if (! object$export_options$form_status) {
    stop("Please reexport with the Form status selected for this function to work.")
  }
  if (class(object) != "secuTrialdata") {
    stop("return_scores requires objects of the class 'secuTrialdata' as input.")
  }

  # init output
  form_status_summary_table <- data.frame(pat_id = character(),
                                          form_name = character(),
                                          completely_filled = integer(),
                                          partly_filled = integer(),
                                          empty = integer(),
                                          with_warnings = integer(),
                                          with_errors = integer())

  all_forms <- as.vector(object$export_options$data_names)
  for (form in all_forms) {
    curr_form <- object[[form]]

    # condition 1
    # mnpfcs0/1/2 contain the vital information
    # if there are no mnpfcs varibles then form status can not be assessed
    # condition 2
    # some files may be empty and must be skipped (e.g. audit trail (at))
    if (length(grep("mnpfcs", names(curr_form))) & nrow(curr_form) & ("pat_id" %in% names(curr_form))) {

      # do something with the data
      form_status_table <- curr_form[, c("pat_id", "mnpfcs0", "mnpfcs1", "mnpfcs2")]

      # translate to number encoding
      if (! object$export_options$refvals_separate) {
        lang <- object$export_options$dict_items$lang
        dict <- .get_dict("dict_form_status_mnpfcs.csv")[, c("vname", "code", lang)]
        for (name in c("mnpfcs0", "mnpfcs1", "mnpfcs2")) {
          # mgsub is from the qdap package - potentially a spot for a different solution
          form_status_table[, name] <- mgsub(form_status_table[, name],
                                            pattern = dict[which(dict$vname == name), ][, lang],
                                            replacement = dict[which(dict$vname == name), ][, "code"])
        }
      }

      status_summary0 <- form_status_table %>% count(pat_id, mnpfcs0)
      status_summary1 <- form_status_table %>% count(pat_id, mnpfcs1)
      status_summary2 <- form_status_table %>% count(pat_id, mnpfcs2)

      spread_summary0 <- spread(status_summary0, key = mnpfcs0, value = n)
      # catch exceptions
      if (! "0" %in% names(spread_summary0)) {
        spread_summary0$"0" <- NA
      }
      if (! "2" %in% names(spread_summary0)) {
        spread_summary0$"2" <- NA
      }
      if (! "4" %in% names(spread_summary0)) {
        spread_summary0$"4" <- NA
      }

      spread_summary1 <- spread(status_summary1, key = mnpfcs1, value = n)
      # catch exceptions
      if (! "1" %in% names(spread_summary1)) {
        spread_summary1$"1" <- NA
      }

      spread_summary2 <- spread(status_summary2, key = mnpfcs2, value = n)
      # catch exceptions
      if (! "1" %in% names(spread_summary2)) {
        spread_summary2$"1" <- NA
      }

      # col names
      names(spread_summary0) <- mgsub(names(spread_summary0),
                                      pattern = c("0", "2", "4"),
                                      replacement = c("empty", "partly_filled", "completely_filled"))
      names(spread_summary1) <- gsub(names(spread_summary1),
                                     pattern = "1",
                                     replacement = "with_errors")
      names(spread_summary2) <- gsub(names(spread_summary2),
                                     pattern = "1",
                                     replacement = "with_warnings")

      # merge the tables on pat_id
      status_summary_table <- merge(spread_summary0, spread_summary1, by = "pat_id") %>%
                                merge(spread_summary2, by = "pat_id")
      # form name
      status_summary_table$form_name <- form

      # only retain relevant columns
      form_status_summary_table <- rbind(
        form_status_summary_table,
        subset(status_summary_table, select = c("pat_id", "form_name", "completely_filled", "partly_filled",
                                                "empty", "with_warnings", "with_errors"))
      )
    }
  }
  # NA values here are really 0 counts
  form_status_summary_table[is.na(form_status_summary_table)] <- 0
  form_status_summary_table
}
