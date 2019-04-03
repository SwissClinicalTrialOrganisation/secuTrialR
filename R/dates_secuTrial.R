

dates_secuTrial <- function(x, ...) UseMethod("dates_secuTrial", x)


#' @rdname dates_secuTrial
#' @param object secuTrialdata object
dates_secuTrial.secuTrialdata <- function(object){

  x <- object$export_options$data_names
  names(x) <- NULL
  x <- x[!x %in% object$export_options$meta_names]

  obs <- lapply(x, function(obj){
    # find date variables
    it <- object[[object$export_options$meta_names$items]]
    qu <- object[[object$export_options$meta_names$questions]]
    itqu <- merge(it, qu, by = "fgid")
    itqu <- itqu[grepl(x, as.character(itqu$formtablename)), ]
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
  obs
  object[x] <- obs
  object

}

#' @rdname dates_secuTrial
#' @param data data.frame
#' @param datevars string consisting of variables with dates
#' @param format format of dates (taken from \code{object$export_options$date.format})
dates_secuTrial.data.frame <- function(data, datevars, format){
  datevars <- datevars[datevars %in% names(data)]
  if(length(datevars)==1){
    newdatecol <- dates_secuTrial(data[, datevars], format)
    data[, paste0(datevars, ".date")] <- newdatecol
  }
  if(length(datevars) > 1){
    newdatecols <- lapply(data[, datevars], function(x) dates_secuTrial(x, format))
    newdatecols <- do.call("cbind", newdatecols)
    names(newdatecols) <- paste0(datevars, ".date")
    data <- cbind(date, newdatecols)
  }
  data
}

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
dates_secuTrial.logical <- function(var, format){
  # this happens when the variable is empty
  var
}













