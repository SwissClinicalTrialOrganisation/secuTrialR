#' Methods to handle date(times)s in secuTrial exports
#' @description Converts dates and datetime variables to \code{Date} or \code{POSIXct} class, as appropriate.
#' @name dates_secuTrial
#' @rdname dates_secuTrial
#' @param object \code{secuTrialdata} object
#' @param ... further parameters
#' @details New variables are created appended with \code{.date} or \code{.datetime}.
#'          This is a safety mechanism in case NAs are inadvertently introduced.
#' @return same as the original object with date variables converted to \code{Date}s.
#' @export
#' @examples
#' # prepare path to example export
#' export_location <- system.file("extdata", "sT_exports", "lnames",
#'                                "s_export_CSV-xls_CTU05_long_ref_miss_en_utf8.zip",
#'                                package = "secuTrialR")
#' # load all export data
#' sT_export <- read_secuTrial_raw(data_dir = export_location)
#' # prepare dates
#' sT_export_dates <- dates_secuTrial(sT_export)
#'
#' # show parsed datetime example
#' sT_export_dates$ctu05baseline$hiv_date.datetime[1]
#' # [1] "2019-03-05 23:56:00 CET"
#' # show parsed date example
#' sT_export_dates$ctu05baseline$paracetamol_start.date[1]
#' # [1] "2019-03-05"

dates_secuTrial <- function(object, ...) UseMethod("dates_secuTrial", object)
datetimes_secuTrial <- function(object, ...) UseMethod("datetimes_secuTrial", object)

#' @rdname dates_secuTrial
#' @export
dates_secuTrial.secuTrialdata <- function(object, ...) {
  if (object$export_options$dated) warning("dates already added")
  table_names <- object$export_options$data_names
  names(table_names) <- NULL
  # get language and internationalization dictionary for items table
  dict <- object$export_options$dict_items
  obs <- lapply(table_names, .convert_dates, object = object, dict = dict, ...)

  object[table_names] <- obs
  object$export_options$dated <- TRUE
  object

}

# @rdname dates_secuTrial
# @param data data.frame
# @param datevars string consisting of variables with dates
# @param format format of dates (typically taken from \code{object$export_options$date_format})
dates_secuTrial.data.frame <- function(data, datevars, timevars, dateformat, datetimeformat, form, warn = FALSE) {
  warn_msg <- ""
  datevars <- datevars[datevars %in% names(data)]
  timevars <- timevars[timevars %in% names(data)]
  if (length(datevars) > 0) {
    for (x in datevars) {
      newdatecol <- dates_secuTrial(data[, x], dateformat)
      # check for conversion of all else warn
      if (length(which(is.na(newdatecol))) > length(which(is.na(data[, x])))) {
        warn_msg <- paste0(warn_msg, "Not all dates were converted for\n",
                           "  variable: '", x,
                           "'\n  in form: '", form,
                           "'\n  This is likely due to incomplete date entries.\n")
      }
      data[, paste0(x, ".date")] <- newdatecol
      data <- .move_column_after(data, paste0(x, ".date"), x)
    }
  }
  data
  if (length(timevars) > 0) {
    for (x in timevars) {
      newdatecol <- datetimes_secuTrial(data[, x], datetimeformat)
      # check for conversion of all else warn
      if (length(which(is.na(newdatecol))) > length(which(is.na(data[, x])))) {
        warn_msg <- paste0(warn_msg, "Not all dates were converted for\n",
                           "  variable: '", x,
                           "'\n  in form: '", form,
                           "'\n  This is likely due to incomplete date entries.\n")
      }
      data[, paste0(x, ".datetime")] <- newdatecol
      data <- .move_column_after(data, paste0(x, ".datetime"), x)
    }
  }
  # only if there is at least one warning queued
  if (str_length(warn_msg)) {
    warning(warn_msg)
  }
  data
}

# @rdname dates_secuTrial
# @param var date variable to be converted
dates_secuTrial.character <- function(var, format) {
  # some export types probably return strings
  d <- as.Date(var, format = format)
  if (!is.null(label(var))) label(d) <- label(var)
  if (!is.null(units(var))) units(d) <- units(var)
  d
}
datetimes_secuTrial.character <- function(var, format) {
  # some export types probably return strings
  d <- as.POSIXct(var, format = format)
  if (!is.null(label(var))) label(d) <- label(var)
  if (!is.null(units(var))) units(d) <- units(var)
  d
}
# @rdname dates_secuTrial
dates_secuTrial.factor <- function(var, format) {
  # depending on options, strings might be converted to factors
  # convert to string
  d <- dates_secuTrial(as.character(var), format)
  if (!is.null(label(var))) label(d) <- label(var)
  if (!is.null(units(var))) units(d) <- units(var)
  d
}
datetimes_secuTrial.factor <- function(var, format) {
  # depending on options, strings might be converted to factors
  # convert to string
  d <- datetimes_secuTrial(as.character(var), format)
  if (!is.null(label(var))) label(d) <- label(var)
  if (!is.null(units(var))) units(d) <- units(var)
  d
}
# @rdname dates_secuTrial
dates_secuTrial.integer <- function(var, format) {
  # this is the default type
  # convert to string
  d <- dates_secuTrial(as.character(var), format)
  if (!is.null(label(var))) label(d) <- label(var)
  if (!is.null(units(var))) units(d) <- units(var)
  d
}
datetimes_secuTrial.integer <- function(var, format) {
  # this is the default type
  # convert to string
  d <- datetimes_secuTrial(as.character(var), format)
  if (!is.null(label(var))) label(d) <- label(var)
  if (!is.null(units(var))) units(d) <- units(var)
  d
}
# @rdname dates_secuTrial
dates_secuTrial.numeric <- function(var, format) {
  # this is the default type
  # convert to string
  d <- dates_secuTrial(as.character(var), format)
  if (!is.null(label(var))) label(d) <- label(var)
  if (!is.null(units(var))) units(d) <- units(var)
  d
}
datetimes_secuTrial.numeric <- function(var, format) {
  # this is the default type
  # convert to string
  d <- datetimes_secuTrial(as.character(var), format)
  if (!is.null(label(var))) label(d) <- label(var)
  if (!is.null(units(var))) units(d) <- units(var)
  d
}
# @rdname dates_secuTrial
dates_secuTrial.logical <- function(var, format) {
  # this happens when the variable is empty
  # convert to string to get (empty) Date object
  d <- dates_secuTrial(as.character(var), format)
  if (!is.null(label(var))) label(d) <- label(var)
  if (!is.null(units(var))) units(d) <- units(var)
  d
}
datetimes_secuTrial.logical <- function(var, format) {
  # this happens when the variable is empty
  # convert to string to get (empty) Date object
  d <- datetimes_secuTrial(as.character(var), format)
  if (!is.null(label(var))) label(d) <- label(var)
  if (!is.null(units(var))) units(d) <- units(var)
  d
}
# @rdname dates_secuTrial
dates_secuTrial.Date <- function(var, format) {
  # in case a variable is already a date
  warning(var, " is already a Date")
  var
}
datetimes_secuTrial.POSIXct <- function(var, format) {
  # in case a variable is already a date
  warning(var, " is already a POSIXct")
  var
}
