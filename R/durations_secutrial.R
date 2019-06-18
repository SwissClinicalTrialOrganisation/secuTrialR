#' Methods to handle durations and clock time variable in secuTrial exports
#' @description Converts durations to the smallest unit (e.g. minutes for variables recorded as hours:minutes) and formats clock times as HH:MM strings rather than (H)HMM integers, as appropriate.
#' @param object secuTrialdata object
#' @param warn logical, return warnings
#' @param days_in_year numeric, number of days in year for conversion to minutes, defaults to 365
#' @seealso \code{\link{read_secuTrial}}
#' @export
durations_secuTrial <- function(x, ...) UseMethod("durations_secuTrial", x)
#' @export
durations_secuTrial.secuTrialdata <- function(object){
  if (object$export_options$durations) warning("durations already added")

  x <- object$export_options$data_names
  names(x) <- NULL
  x <- x[!x %in% object$export_options$meta_names]

  obs <- lapply(x, function(obj){
    # find date variables
    it <- object[[object$export_options$meta_names$items]]
    qu <- object[[object$export_options$meta_names$questions]]
    itqu <- merge(it, qu, by = "fgid")
    itqu <- itqu[grepl(obj, as.character(itqu$formtablename)), ]
    if (nrow(itqu) > 0) { # particularly relevant for audit trail
      itqu$itemtype <- as.character(itqu$itemtype)
      itqu$ffcolname <- as.character(itqu$ffcolname)

      intervals <- itqu[grepl("calculated only", itqu$itemtype), ] # USE "CALCULATED ONLY"
      intervals <- intervals[!grepl("Score", intervals$itemtype), ] # filter out scores
      intervals <- unique(intervals[, c("ffcolname",
                                        "itemtype",
                                        "fflabel",
                                        "formtablename")])
      if (nrow(intervals) > 0){
        intervals$length <- NA
        intervals$format <- NA
        g <- function(x) grep(x, intervals$itemtype)
        intervals$format[g(" m-s ")] <- "m-s"
        intervals$format[g(" h-m-s ")] <- "h-m-s"
        intervals$format[g(" h-m ")] <- "h-m"
        intervals$format[g(" d-h-m ")] <- "d-h-m"
        intervals$format[g(" y-m-d-h-m ")] <- "y-m-d-h-m"
        intervals$format[g(" y-m-d ")] <- "y-m-d"
        intervals$format[g(" y-m ")] <- "y-m"
        intervals$format[g(" y ")] <- "y" # could probably drop them
        intervals <- intervals[!g(" y "), ] # drop year only variables - can use them as they are
      }

      g <- function(x) grep(x, times$itemtype)
      times <- itqu[grepl("Time", itqu$itemtype) &
                      !grepl("Interval", itqu$itemtype), ]
      times <- unique(times[, c("ffcolname",
                                "itemtype",
                                "fflabel",
                                "formtablename")])
      if (nrow(times) > 0) {
        times$format <- NA
        times$format[g("\\(hh:mm\\)")] <- "hh:mm"
        times$format[g("\\(mm:ss\\)")] <- "mm:ss"
      }

      tmp <- object[[obj]]
      lab <- label(tmp)
      tmp <- durations_secuTrial(tmp, intervals = intervals, times = times)
      label(tmp) <- lab
      out <- tmp
    } else {
      out <- object[[obj]]
    }

    out
  })
  object[x] <- obs
  object$export_options$durations <- TRUE
  object

}


durations_secuTrial.data.frame <- function(data,
                                           intervals,
                                           times,
                                           warn = FALSE,
                                           ...){
  intervalvars <- intervals[intervals$ffcolname %in% names(data), ]
  timevars <- times[times$ffcolname %in% names(data), ]
  if (nrow(intervalvars) > 0) {
    for (x in 1:nrow(intervalvars)) {
      v <- intervalvars$ffcolname[x]
      l <- intervalvars$length[x]
      f <- intervalvars$format[x]
      newcol <- durations_secuTrial(data[, v], f, ...)
      nv <- paste0(v, ".dur")
      data[, nv] <- newcol
      label(data[, nv]) <- paste0(label(data[, v]),
                                  " (smallest unit ", f, ")")
      data <- .move_column_after(data, nv, v)
    }
  } else {
    if (warn) warning(paste("no durations detected in",
                            get("obj", envir = parent.frame())))
  }
  if (nrow(timevars) > 0) {
    # print("time")
    for (x in 1:nrow(timevars)) {
      v <- timevars$ffcolname[x]
      l <- timevars$length[x]
      f <- timevars$format[x]
      newcol <- times_secuTrial(data[, v], f)
      nv <- paste0(v, ".time")
      data[, nv] <- newcol
      label(data[, nv]) <- paste0(label(data[, v]), " (clock time)")
      data <- .move_column_after(data, nv, v)
    }
  } else {
    if (warn) warning(paste("no times detected in",
                            get("obj", envir = parent.frame())))
  }
  data
}





durations_secuTrial.numeric <- function(var,
                                        format,
                                        ...){
  f2l <- format2length(format, ...)
  if (f2l$length > 1){
    # pad
    z <- sprintf(paste0("%0", f2l$length, ".0f"), var)
    z[is.na(var)] <- NA
    l <- lapply(z, function(x){
      s <- substring(x, f2l$cp1, f2l$cp2)
      as.numeric(s)
    })
    out <- sapply(l, function(x) sum(x * f2l$multiplier))
  } else {
    out <- var
  }
  out
}


times_secuTrial <- function(var,
                            format){
  f2l <- format2length(format)
  if (length(var) > 0) {
    z <- sprintf(paste0("%0", f2l$length, ".0f"), var)
    z[is.na(var)] <- NA
    l <- lapply(z, function(x){
      s <- substring(x, seq(1, f2l$length, 2), seq(2, f2l$length, 2))
    })
    # need others?
    out <- sapply(l, paste0, collapse = ":")
    out[is.na(var)] <- NA
  } else {
    out <- var
  }
  out
}


format2length <- function(x, days_in_year = 365){
  days_in_month <- days_in_year / 12
  fs <- data.frame(format = c("m-s", "h-m-s", "h-m", "d-h-m",
                              "y-m-d-h-m", "y-m-d", "y-m", "y", "hh:mm",
                              "mm:ss"),
                   length = c(4, 6, 4, 6, 12, 8, 6, 4, 4, 4),
                   cut1 = c(2, 2, 2, 2, 4, 4, 4, 4, 2, 2),
                   stringsAsFactors = FALSE)
  w <- match(x, fs$format)
  fs <- fs[w, ]
  if (fs$cut1 < fs$length){
    cp1 <- c(1, seq(fs$cut1+1, fs$length, 2))
    cp2 <- c(fs$cut1, seq(fs$cut1+2, fs$length, 2))
  } else {
    cp1 <- 1
    cp2 <- fs$cut1
  }
  # multipliers
  if (x == "m-s") multiplier <- c(60, 1)
  if (x == "h-m-s") multiplier <- c(60^2, 60, 1)
  if (x == "h-m") multiplier <- c(60, 1)
  if (x == "d-h-m") multiplier <- c(24*60, 60, 1)
  if (x == "y-m-d-h-m") multiplier <- c(days_in_year*24*60,
                                        days_in_month*24*60, 24*60, 60, 1)
  if (x == "y-m-d") multiplier <- c(days_in_year, days_in_month, 1)
  if (x == "y-m") multiplier <- c(12, 1)
  if (x == "y") multiplier <- c(1)
  if (x == "hh:mm") multiplier <- NA
  if (x == "mm:ss") multiplier <- NA
  return(list(format = fs$format,
              length = fs$length,
              cut1 = fs$cut1,
              cp1 = cp1,
              cp2 = cp2,
              multiplier = multiplier))
}


