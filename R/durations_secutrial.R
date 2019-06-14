#' Methods to handle durations and clock time variable in secuTrial exports
#' @description Converts durations to the smallest unit (e.g. minutes for variables recorded as hours:minutes) and formats clock times as HH:MM strings rather than (H)HMM integers, as appropriate.
#' @param object secuTrialdata object
#' @param warn logical, return warnings
#' @param days_in_month numeric, number of days in month for conversion to minutes, defaults to 30.4
#' @param days_in_year numeric, number of days in year for conversion to minutes, defaults to 365




#' @export
durations_secuTrial <- function(x, ...) UseMethod("durations_secuTrial", x)
#' @export
durations_secuTrial.secuTrialdata <- function(object){
  print("sT")
  if (object$export_options$dated) warning("dates already added")

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
    if (nrow(itqu) > 0){ # particularly relevant for audit trail
      itqu$itemtype <- as.character(itqu$itemtype)
      itqu$ffcolname <- as.character(itqu$ffcolname)

      intervals <- itqu[grepl("Interval", itqu$itemtype), ]
      intervals <- unique(intervals[, c("ffcolname",
                                        "itemtype",
                                        "fflabel",
                                        "formtablename")])
      if (nrow(intervals) > 0){
        intervals$length <- NA
        intervals$format <- NA
        g <- function(x) grep(x, intervals$itemtype)
        intervals$length[g("Time .* h-m ")] <- 4
        intervals$format[g("Time .* h-m ")] <- "h-m"
        intervals$length[g(" y ")] <- 1 # can ignore?
        intervals$format[g(" y ")] <- "y"
        intervals$length[g("Date .* y-m-d-h-m ")] <- 10 # 99-11-30-23-59
        intervals$format[g("Date .* y-m-d-h-m ")] <- "y-m-d-h-m"
      }

      g <- function(x) grep(x, times$itemtype)
      times <- itqu[grepl("Time", itqu$itemtype) &
                      !grepl("Interval", itqu$itemtype), ]
      times <- unique(times[, c("ffcolname",
                                "itemtype",
                                "fflabel",
                                "formtablename")])
      if (nrow(times) > 0) {
        times$length <- NA
        times$format <- NA
        times$length[g("\\(hh:mm\\)")] <- 4
        times$format[g("\\(hh:mm\\)")] <- "hh:mm"
      }

      tmp <- object[[obj]]
      tmp <- durations_secuTrial(tmp, intervals, times)
      out <- tmp
    } else {
      out <- object[[obj]]
    }

    out
  })
  object[x] <- obs
  object$export_options$dated <- TRUE
  object

}


durations_secuTrial.data.frame <- function(data,
                                           intervals,
                                           times,
                                           warn = FALSE,
                                           ...){
  # print(df)
  intervalvars <- intervals[intervals$ffcolname %in% names(data),]
  timevars <- times[times$ffcolname %in% names(data),]
  if (nrow(intervalvars) > 0) {
    # print("interval")
    for (x in 1:nrow(intervalvars)) {
      v <- intervalvars$ffcolname[x]
      # print(v)
      l <- intervalvars$length[x]
      f <- intervalvars$format[x]
      newcol <- durations_secuTrial(data[, v], l, f, ...)
      nv <- paste0(v, ".dur")
      data[, nv] <- newcol
      label(data[, nv]) <- paste0(label(data[, x]), " (smallest unit ", f, ")")
      data <- .move_column_after(data, nv, v)
    }
  } else {
    if (warn) warning(paste("no durations detected in", get("obj", envir = parent.frame())))
  }
  if (nrow(timevars) > 0) {
    # print("time")
    for (x in 1:nrow(timevars)) {
      v <- timevars$ffcolname[x]
      # print(v)
      l <- timevars$length[x]
      f <- timevars$format[x]
      newcol <- times_secuTrial(data[, v], l, f)
      nv <- paste0(v, ".time")
      data[, nv] <- newcol
      label(data[, nv]) <- paste0(label(data[, x]), " (clock time)")
      data <- .move_column_after(data, nv, v)
    }
  } else {
    if (warn) warning(paste("no times detected in", get("obj", envir = parent.frame())))
  }
  data
}





durations_secuTrial.integer <- function(var,
                                        length,
                                        format,
                                        days_in_month = 30.4,
                                        days_in_year = 365){
  # print("integer")
  # print(length)
  # print(format)
  if (length > 1){
    # pad
    z <- sprintf(paste0("%0", length, ".0f"), var)
    z[is.na(var)] <- NA
    l <- lapply(z, function(x){
      s <- substring(x, seq(1, length, 2), seq(2, length, 2))
      as.numeric(s)
    })
    if (format == "h-m") multiplier <- c(60, 1)
    if (format == "y-m-d-h-m") multiplier <- c(days_in_year*24*60, # years -> mins
                                               days_in_month*24*60, # months -> mins
                                               24*60, # days -> mins
                                               60, # hours -> mins
                                               1) # mins
    out <- sapply(l, function(x) sum(x * multiplier))
  } else {
    out <- var
  }
  out
}


times_secuTrial <- function(var,
                            length,
                            format){
  # print("times")
  if (length(var) > 0) {
    z <- sprintf(paste0("%0", length, ".0f"), var)
    z[is.na(var)] <- NA
    l <- lapply(z, function(x){
      s <- substring(x, seq(1, length, 2), seq(2, length, 2))
    })
    # need others?
    out <- sapply(l, paste0, collapse = ":")
  } else {
    out <- var
  }
  out
}
