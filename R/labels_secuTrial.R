#' Get variable labels for secuTrialdata objects
#'
#' @rdname labels_secuTrial
#' @name labels_secuTrial
#' @description Variable labels are important for understanding the contents of a variable. \code{secuTrialR} offers two main methods to get those labels. \code{labels_secuTrial} returns a named list of labels. \code{label_secuTrial} adds labels and units to variables (and data.frames) which can then be queried via \code{label} or \code{units}.
#' @param object \code{secuTrialdata} object
#' @param form which form (string)
#' @details For \code{labels_secuTrial}, regular expressions are used with \code{form} (specifically, it is inserted between \code{(} and \code{)$} to identify the form). Consequently, if \code{form} matches multiple forms (because the beginning is different), multiple forms may be returned. You could be more specific with the regular expression, remembering that it is inserted between \code{(} and \code{)$}.
#' @note The \code{label_secuTrial}/\code{label} syntax is similar to that used in Hmisc, with the advantage that it does not change data types (Hmisc coerces everything to labelled integer). Similar to Hmisc, however, most operations will remove the labels.
#' @return \code{labels_secuTrial} returns a named vector
#' \code{label_secuTrial} returns the same object as \code{object}, but with labels added to variables and data.frames
#' \code{label} and \code{units} return strings with the appropriate labels
#' @export
#'
#' @examples
#' # prepare path to example export
#' export_location <- system.file("extdata", "s_export_CSV-xls_BMD.zip",
#'                                package = "secuTrialR")
#' # load all export data
#' sT_export <- load_secuTrial_export(data_dir = export_location)
#' # get labels
#' labels <- labels_secuTrial(sT_export)
#' labels[["age"]]
labels_secuTrial <- function(object, form = NULL) {
  it <- object[[object$export_options$meta_names$items]]
  if (!is.null(form)) {
    qs <- object[[object$export_options$meta_names$questions]]
    itqs <- merge(it, qs, by = "fgid")
    it <- itqs[grepl(paste0("(", paste0(form, collapse = "|"), ")$"), itqs$formtablename), ]
  }
  it <- it[!grepl("Dummy", as.character(it$itemtype)), ]
  it <- it[, c("ffcolname", "fflabel")]
  it <- unique(it)
  it2 <- as.character(it$fflabel)
  names(it2) <- it$ffcolname
  it2
}




#' @rdname labels_secuTrial
#' @export
#' @examples
#'
#' # ALTERNATIVE APPROACH
#' # load secuTrial export with separate reference table
#' sT_export <- load_secuTrial_export(system.file("extdata", "s_export_CSV-xls_CTU05_longnames_sep_ref.zip", package = "secuTrialR"))
#' # label the secuTrialdata object
#' sT_export_labelled <- label_secuTrial(sT_export)
#' # form label
#' label(sT_export_labelled$ctu05baseline)
#' # variable label
#' label(sT_export_labelled$ctu05baseline$visit_date)
#' # sampling units
#' units(sT_export_labelled$ctu05baseline$height)
label_secuTrial <- function(object, ...) UseMethod("label_secuTrial", object)
#' @export
label_secuTrial.secuTrialdata <- function(object) {
  if (!object$export_options$meta_available$items) {
    stop("'items' metadata not available")
  }
  if(object$export_options$factorized) warning("already labelled - any changes will be lost")


  it <- object[[object$export_options$meta_names$items]]
  qs <- object[[object$export_options$meta_names$questions]]
  it <- merge(it, qs, by = "fgid")
  it$ffcolname <- as.character(it$ffcolname)
  it$fflabel <- as.character(it$fflabel)
  it$formtablename <- as.character(it$formtablename)
  it$formname <- as.character(it$formname)
  it$unit <- as.character(it$unit)

  it <- it[!grepl("Dummy", as.character(it$itemtype)), ]
  # it <- it[, c("ffcolname", "fflabel", "formtablename")]
  it$formtablename <- as.character(it$formtablename)
  it$fname <- gsub(pattern = "^mnp", "", it$formtablename)
  it <- it[!duplicated(it[, c("ffcolname", "fflabel", "formtablename")]), ]
  # # some variables are still duplicated - fflabel can differ
  # # - retain longest
  # it$length <- nchar(it$fflabel)
  # it <- it[order(it$length, decreasing = TRUE), ]
  # it <- it[!duplicated(it[, c("ffcolname", "formtablename")]), ]
  if(any(duplicated(it[, c("ffcolname", "formtablename")]))) warning("some variable names appear to be duplicated - labels attribute may be longer that 1")

  x <- object$export_options$data_names
  names(x) <- NULL
  x <- x[!x %in% object$export_options$meta_names]
  if(!object$export_options$short_names) x <- x[x %in% it$fname]
  # note that the basic form for extended forms might have no variables
  obs <- lapply(x, function(obj){
    tmp <- object[[obj]]
    if(object$export_options$short_names) tmp <- label_secuTrial(tmp, it)
    if(!object$export_options$short_names) tmp <- label_secuTrial(tmp, it[it$fname == obj, ])
    tmp
  })
  obs
  object[x] <- obs
  object$export_options$labelled <- TRUE
  object
}

label_secuTrial.data.frame <- function(data, it) {
  it <- it[it$ffcolname %in% names(data), ]
  # print(it)
  for (i in names(data)[names(data) %in% it$ffcolname]) {
    x <- it$fflabel[it$ffcolname == i]
    u <- it$unit[it$ffcolname == i]
    # print(paste(i, "label", x))
    label(data[, i]) <- x
    if(any(!is.na(u))) units(data[, i]) <- u
  }
  label(data) <- it$formname[1]

  return(data)
}

#' @rdname labels_secuTrial
#' @param x any object
#' @export
label <- function(x) attr(x, "label")

#' @export
units <- function(x) attr(x, "units")

#' @export
"label<-" <- function(x, value){
  attr(x, "label") <- value
  return(x)
}

#' @export
"units<-" <- function(x, value){
  attr(x, "units") <- value
  return(x)
}
