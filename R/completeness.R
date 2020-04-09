#' A function to assess the status of forms
#'
#' This function returns a data.frame informing on the status of saved
#' forms per participant. There is a line for every combination of
#' form type and participant id. The numbers are occurrence counts.
#'
#' @param object \code{secuTrialdata} object
#' @param ... further parameters
#' @keywords form status completeness
#' @export
#' @return data.frame informing on the status of saved forms per participant
#' @importFrom tidyr spread
#' @importFrom dplyr count recode
#' @examples
#' # prepare path to example export
#' export_location <- system.file("extdata", "sT_exports", "snames",
#'                                "s_export_CSV-xls_CTU05_short_ref_miss_en_utf8.zip",
#'                                package = "secuTrialR")
#' # load all export data
#' sT_export <- read_secuTrial(data_dir = export_location)
#'
#' # get form status
#' form_status_counts(sT_export)
#'
form_status_counts <- function(object, ...) UseMethod("form_status_counts", object)

#' @export
form_status_counts.secuTrialdata <- function(object, ...) {

  if (! object$export_options$form_status) {
    stop("Please reexport with the Form status selected for this function to work.")
  }

  # the function only works with factorized exports
  if (! object$export_options$factorized) {
    stop("Please use a factorized (see factorize_secuTrial()) secuTrialdata object for this function to work.")
  }

  # check for add_if
  if (! object$export_options$add_id) {
    stop("Please reexport with the Add-ID selected for this function to work.")
  }

  # get dict
  mnpfcs_dict <- .get_dict("dict_form_status_mnpfcs.csv")
  curr_lang_and_en_dict <- mnpfcs_dict[, c("en", object$export_options$dict_items$lang)]

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
    # condition 3
    # there must be a pat_id
    if (length(grep("mnpfcs", names(curr_form))) & nrow(curr_form) & ("pat_id" %in% names(curr_form))) {

      # override to english if language differs from en
      if (object$export_options$dict_items$lang != "en") {
        curr_form$mnpfcs0.factor <- recode(curr_form$mnpfcs0.factor, !!! setNames(curr_lang_and_en_dict[, 1],
                                                                                  curr_lang_and_en_dict[, 2]))
        curr_form$mnpfcs1.factor <- recode(curr_form$mnpfcs1.factor, !!! setNames(curr_lang_and_en_dict[, 1],
                                                                                  curr_lang_and_en_dict[, 2]))
        curr_form$mnpfcs2.factor <- recode(curr_form$mnpfcs2.factor, !!! setNames(curr_lang_and_en_dict[, 1],
                                                                                  curr_lang_and_en_dict[, 2]))
      }

      # select relevant columns
      form_status_table <- curr_form[, c("pat_id",
                                         "mnpfcs0.factor",
                                         "mnpfcs1.factor",
                                         "mnpfcs2.factor")]

      # the warnings here are uninformative
      status_summary0 <- suppressWarnings(form_status_table %>% count(form_status_table$pat_id,
                                                                      form_status_table$mnpfcs0.factor))
      status_summary1 <- suppressWarnings(form_status_table %>% count(form_status_table$pat_id,
                                                                      form_status_table$mnpfcs1.factor))
      status_summary2 <- suppressWarnings(form_status_table %>% count(form_status_table$pat_id,
                                                                      form_status_table$mnpfcs2.factor))

      # change names
      names(status_summary0) <- c("pat_id", "mnpfcs0.factor", "n")
      names(status_summary1) <- c("pat_id", "mnpfcs1.factor", "n")
      names(status_summary2) <- c("pat_id", "mnpfcs2.factor", "n")

      spread_summary0 <- spread(status_summary0, key = "mnpfcs0.factor", value = "n")
      # catch exceptions
      if (! "empty" %in% names(spread_summary0)) {
        spread_summary0$"empty" <- NA
      }
      if (! "partly filled" %in% names(spread_summary0)) {
        spread_summary0$"partly filled" <- NA
      }
      if (! "completely filled" %in% names(spread_summary0)) {
        spread_summary0$"completely filled" <- NA
      }

      spread_summary1 <- spread(status_summary1, key = "mnpfcs1.factor", value = "n")
      # catch exceptions
      if (! "with errors" %in% names(spread_summary1)) {
        spread_summary1$"with errors" <- NA
      }

      spread_summary2 <- spread(status_summary2, key = "mnpfcs2.factor", value = "n")
      # catch exceptions
      if (! "with warnings" %in% names(spread_summary2)) {
        spread_summary2$"with warnings" <- NA
      }

      # merge the tables on pat_id
      status_summary_table <- merge(spread_summary0, spread_summary1, by = "pat_id") %>%
                                merge(spread_summary2, by = "pat_id")
      # form name
      status_summary_table$form_name <- form

      # only retain relevant columns
      form_status_summary_table <- rbind(
        form_status_summary_table,
        subset(status_summary_table, select = c("pat_id", "form_name", "completely filled", "partly filled",
                                                "empty", "with warnings", "with errors"))
      )
    }
  }
  # NA values here are really 0 counts
  form_status_summary_table[is.na(form_status_summary_table)] <- 0
  names(form_status_summary_table) <- gsub(names(form_status_summary_table), pattern = " ", replacement = "_")
  form_status_summary_table
}

#' A function to show summary statistics for form statuses
#'
#' This function warps form_status_counts and returns a
#' data.frame summarizing the statuses for each form.
#' Only saved forms are considered for the statistic.
#'
#' @param object \code{secuTrialdata} object
#' @param ... further parameters
#' @keywords form status completeness
#' @importFrom dplyr group_by summarise mutate_all
#' @export
#' @return data.frame summarizing the statuses for each form
#' @examples
#' # prepare path to example export
#' export_location <- system.file("extdata","sT_exports", "snames",
#'                                "s_export_CSV-xls_CTU05_short_ref_miss_en_utf8.zip",
#'                                package = "secuTrialR")
#' # load all export data
#' sT_export <- read_secuTrial(data_dir = export_location)
#'
#' # get form status
#' form_status_summary(sT_export)
#'
form_status_summary <- function(object, ...) UseMethod("form_status_summary", object)

#' @importFrom rlang .data
#' @export
form_status_summary.secuTrialdata <- function(object, ...) {

  status_counts <- form_status_counts(object)
  status_summary <- status_counts %>%
    # .data helps with the "no visible binding for global variable"
    # NOTE produced by "R CMD check" for this to word we also need
    # the @importFrom rlang .data over the function
    group_by(.data$form_name) %>%
    summarise("partly_filled" = sum(.data$partly_filled),
              "completely_filled" = sum(.data$completely_filled),
              "empty" = sum(.data$empty),
              "with_warnings" = sum(.data$with_warnings),
              "with_errors" = sum(.data$with_errors))
  # the sum of "partly filled", "completely filled", "empty" is the total count of
  # registered forms for each form type (form_name)
  form_count <- rowSums(subset(status_summary, select = c("partly_filled",
                                                          "completely_filled",
                                                          "empty")))
  # omit form_name and divide all count columns by form_count
  percentage <- status_summary[, 2:ncol(status_summary)] %>%
                  mutate_all(list(~. / form_count))
  # label the percent columns
  names(percentage) <- paste0(names(percentage), ".percent")
  # append all columns together
  status_summary_return <- cbind(status_summary, percentage)
  status_summary_return$form_count <- form_count

  return(status_summary_return)
}
