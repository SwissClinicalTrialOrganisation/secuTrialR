#' Methods to handle dates in SecuTrial exports
#' @name dates_secuTrial
#' @rdname dates_secuTrial
#' @param object secuTrialdata object
#' @details New variables are created appended with \code{.date}. This is a safety mechanism incase NAs are inadvertantly introduced.
#' Methods exist for secuTrialdata objects, data.frames, character, factor, integer and logical classes.
#' @return same as the original object with date variables converted to Dates.
#' @export
#' @examples
#' # TODO
dates_secuTrial <- function(x, ...) UseMethod("dates_secuTrial", x)

#' @rdname dates_secuTrial
#' @export
dates_secuTrial.secuTrialdata <- function(object){

  x <- object$export_options$data_names
  names(x) <- NULL
  x <- x[!x %in% object$export_options$meta_names]

  obs <- lapply(x, function(obj){
    # print(obj)
    # find date variables
    it <- object[[object$export_options$meta_names$items]]
    qu <- object[[object$export_options$meta_names$questions]]
    itqu <- merge(it, qu, by = "fgid")
    itqu <- itqu[grepl(obj, as.character(itqu$formtablename)), ]
    itqu$itemtype <- as.character(itqu$itemtype)
    itqu$ffcolname <- as.character(itqu$ffcolname)
    itqu <- itqu[grepl("checked date", itqu$itemtype, ignore.case = TRUE), ]
    # remove year
    itqu <- itqu[!grepl("\\(yyyy\\)", itqu$itemtype, ignore.case = TRUE), ]
    datevars <- unique(itqu$ffcolname)
    # date format
    format <- object$export_options$date.format
    # print(obj)
    tmp <- object[[obj]]
    tmp <- dates_secuTrial(tmp, datevars, format)
    tmp
  })
  # print(obs)
  object[x] <- obs
  object

}

#' @rdname dates_secuTrial
#' @param data data.frame
#' @param datevars string consisting of variables with dates
#' @param format format of dates (taken from \code{object$export_options$date.format})
dates_secuTrial.data.frame <- function(data, datevars, format){
  datevars <- datevars[datevars %in% names(data)]
  # print(datevars)
  if(length(datevars)==1){
    newdatecol <- dates_secuTrial(data[, datevars], format)
    # print(head(newdatecol))
    data[, paste0(datevars, ".date")] <- newdatecol
  }
  if(length(datevars) > 1){
    newdatecols <- lapply(data[, datevars], function(x) dates_secuTrial(x, format))
    newdatecols <- do.call("cbind", newdatecols)
    colnames(newdatecols) <- paste0(datevars, ".date")
    # print(head(newdatecols))
    data <- cbind(data, newdatecols)
  }
  # print(head(data))
  data
}

#' @rdname dates_secuTrial
#' @param var date variable to be converted
#' @param format date format
dates_secuTrial.character <- function(var, format){
  # some export types probably return strings
  as.Date(var, format = format)
}
dates_secuTrial.factor <- function(var, format){
  # depending on options, strings might be converted to factors
  # convert to string
  dates_secuTrial(as.character(var), format)
}
dates_secuTrial.integer <- function(var, format){
  # this is the default type
  # convert to string
  dates_secuTrial(as.character(var), format)
}
dates_secuTrial.numeric <- function(var, format){
  # this is the default type
  # convert to string
  dates_secuTrial(as.character(var), format)
}
dates_secuTrial.logical <- function(var, format){
  # this happens when the variable is empty
  var
}













