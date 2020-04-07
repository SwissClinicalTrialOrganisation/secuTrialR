#' Get variable labels for secuTrialdata objects
#'
#' @rdname labels_secuTrial
#' @name labels_secuTrial
#' @description Variable labels are important for understanding the contents of a variable.
#'              \code{secuTrialR} offers two main methods to get those labels. \code{labels_secuTrial}
#'              returns a named list of labels. \code{label_secuTrial} adds labels and units to
#'              variables (and data.frames) which can then be queried via \code{label} or \code{units}.
#' @param object a \code{secuTrialdata} object
#' @param form which form (string)
#' @details For \code{labels_secuTrial}, regular expressions are used with \code{form}
#'          (specifically, it is inserted between \code{(} and \code{)$} to identify the form).
#'          Consequently, if \code{form} matches multiple forms (because the beginning is different),
#'          multiple forms may be returned. You could be more specific with the regular expression,
#'          remembering that it is inserted between \code{(} and \code{)$}.
#' @note The \code{label_secuTrial}/\code{label} syntax is similar to that used in Hmisc, with the
#'       advantage that it does not change data types (Hmisc coerces everything to labelled integer).
#'       Similar to Hmisc, however, most operations will remove the labels.
#' @return \code{labels_secuTrial} returns a named vector
#' \code{label_secuTrial} returns the same object as \code{object}, but with labels added to variables
#' and data.frames
#' \code{label} and \code{units} return strings with the appropriate labels
#' @export
#'
#' @examples
#' # APPROACH 1: labels_secuTrial
#' # ex. 1
#' # prepare path to example export
#' export_location <- system.file("extdata", "sT_exports", "BMD",
#'                                "s_export_CSV-xls_BMD_short_en_utf8.zip",
#'                                package = "secuTrialR")
#' # load all export data
#' sT_export <- read_secuTrial_raw(data_dir = export_location)
#' # get all labels
#' labels <- labels_secuTrial(sT_export)
#' labels[["age"]]
#'
#' # ex. 2
#' # load export
#' sT_export <- read_secuTrial_raw(system.file("extdata", "sT_exports", "lnames",
#'                                             "s_export_CSV-xls_CTU05_long_miss_en_utf8.zip",
#'                                             package = "secuTrialR"))
#'
#' # get labels for sae, treatment and surgeries forms
#' labels <- labels_secuTrial(sT_export, form = c("sae", "treatment", "surgeries"))
#'
labels_secuTrial <- function(object, form = NULL) {
  it <- object[[object$export_options$meta_names$items]]
  if (!is.null(form)) {
    if (!object$export_options$duplicate_meta) {
      qs <- object[[object$export_options$meta_names$questions]]
      it <- merge(it, qs, by = "fgid")
    }
    it <- it[grepl(paste0("(", paste0(form, collapse = "|"), ")$"), it$formtablename), ]
  }

  dict <- object$export_options$dict_items
  it <- it[!grepl(dict[, c("dummy")], as.character(it$itemtype)), ]

  it <- it[, c("ffcolname", "fflabel")]
  it <- unique(it)
  it2 <- as.character(it$fflabel)
  names(it2) <- it$ffcolname
  it2
}


#' @rdname labels_secuTrial
#' @param object a \code{secuTrialdata} object
#' @param ... further parameters
#' @export
#' @return \code{secuTrialdata} object with labels applied to each variable
#' @examples
#'
#' # APPROACH 2: label_secuTrial
#' # load secuTrial export with separate reference table
#' sT_export <- read_secuTrial_raw(system.file("extdata", "sT_exports", "lnames",
#'                                             "s_export_CSV-xls_CTU05_long_ref_miss_en_utf8.zip",
#'                                             package = "secuTrialR"))
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
label_secuTrial.secuTrialdata <- function(object, ...) {
  if (!object$export_options$meta_available$items) {
    stop("'items' metadata not available. Try reexporting your data with Project setup enabled.")
  }
  if (object$export_options$labelled) warning("already labelled - any changes will be lost")


  it <- object[[object$export_options$meta_names$items]]
  # if meta data has not been duplicated into all forms
  # it needs to be added to the items (it) table from the
  # questions (qs) table
  # the merge does not create an identical data frame compared
  # to the items table if meta data was duplicated into all tables
  if (!object$export_options$duplicate_meta) {
    qs <- object[[object$export_options$meta_names$questions]]
    it <- merge(it, qs, by = "fgid")
  }
  # make sure that the columns are characters to avoid exceptions
  it$ffcolname <- as.character(it$ffcolname)
  it$fflabel <- as.character(it$fflabel)
  it$formtablename <- as.character(it$formtablename)
  it$formname <- as.character(it$formname)
  it$unit <- as.character(it$unit)

  dict <- object$export_options$dict_items
  # remove Layout Dummies
  it <- it[!grepl(dict[, c("dummy")], as.character(it$itemtype)), ]
  it$fname <- gsub(pattern = "^mnp", "", it$formtablename)
  # these duplications happen because the old status is
  # stored after releasing a new version of a CDMA
  it <- it[!duplicated(it[, c("ffcolname", "fflabel", "formtablename")]), ]
  # some variables are still duplicated because fflabel can differ
  # this happens when the label is changed in the implementation
  # of the CDMA. The old and the new state of fflabel differ thus
  # both state are added and the label can be longer than 1
  if (any(duplicated(it[, c("ffcolname", "formtablename")]))) {
    # prep for specific warning
    longer_one_vars <- it$ffcolname[which(duplicated(it[, c("ffcolname", "formtablename")]))]
    longer_one_forms <- it$formtablename[which(duplicated(it[, c("ffcolname", "formtablename")]))]
    warning(paste0("The labels attribute may be longer than 1 for the following variables and forms.\n",
                   "Likely the label was changed from its original state in the secuTrial project setup.\n",
                   "variables: ", toString(longer_one_vars),
                   "\nforms: ", toString(longer_one_forms))
           )
  }
  x <- object$export_options$data_names
  names(x) <- NULL
  x <- x[!x %in% object$export_options$meta_names]
  if (!object$export_options$short_names) x <- x[x %in% it$fname]
  # note that the basic form for extended forms might have no variables
  obs <- lapply(x, function(obj) {
    tmp <- object[[obj]]
    if (object$export_options$short_names) tmp <- label_secuTrial(tmp, it)
    if (!object$export_options$short_names) tmp <- label_secuTrial(tmp, it[it$fname == obj, ])
    tmp
  })
  object[x] <- obs
  object$export_options$labelled <- TRUE
  return(object)
}

label_secuTrial.data.frame <- function(data, it) {
  it <- it[it$ffcolname %in% names(data), ]
  for (i in names(data)[names(data) %in% it$ffcolname]) {
    # variables can have the same name in different
    # forms, if this is not made unique() labels can contain
    # the same string several times which is not informative
    x <- unique(it$fflabel[it$ffcolname == i])
    u <- it$unit[it$ffcolname == i]
    label(data[, i]) <- x
    if (any(!is.na(u))) units(data[, i]) <- u
  }
  label(data) <- it$formname[1]

  return(data)
}

#' @rdname labels_secuTrial
#' @param x any object
#' @export
label <- function(x) attr(x, "label")

#' @rdname labels_secuTrial
#' @param x any object
#' @export
units <- function(x) attr(x, "units")

#' @rdname labels_secuTrial
#' @param x any object
#' @param value any object
#' @export
"label<-" <- function(x, value) {
  attr(x, "label") <- value
  return(x)
}

#' @rdname labels_secuTrial
#' @param x any object
#' @param value any object
#' @export
"units<-" <- function(x, value) {
  attr(x, "units") <- value
  return(x)
}
