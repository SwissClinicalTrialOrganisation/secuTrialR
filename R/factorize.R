#' Add factors to \code{secuTrialdata} objects
#' @description secuTrial can return a codebook of codes and labels for categorical variables, including lookup
#'              type variables, if this option is selected in the export tool ('reference values as separate table').
#'              This allows factors to be easily created. Factorize methods exist for \code{secuTrialdata} objects,
#'              \code{data.frames}, \code{integer}s and \code{logical}s, but the intent is that only the former be
#'              used by users. The other methods could be used with customized codebooks.
#' @rdname factorize
#' @name factorize
#' @param object \code{secuTrialdata} object with additional factor variables in study forms containing categorical data
#' @param ... further parameters
#' @export
#' @return factorized \code{secuTrialdata} object
#' @details factorize_secuTrial will return an error if the appropriate codebook is not available.
#'
#' @examples
#' # load secuTrial export with separate reference table
#' sT_export <- read_secuTrial_raw(system.file("extdata", "sT_exports", "lnames",
#'                                             "s_export_CSV-xls_CTU05_long_ref_miss_en_utf8.zip",
#'                                             package = "secuTrialR"))
#' # factorize the secuTrialdata object
#' sT_export_factorized <- factorize_secuTrial(sT_export)

# create factors
factorize_secuTrial <- function(object, ...) UseMethod("factorize_secuTrial", object)

#' @rdname factorize
#' @export
factorize_secuTrial.secuTrialdata <- function(object, ...) {
  if (!object$export_options$refvals_separate) {
    ifelse(options()$stringsAsFactors,
           saf <- "\nCategorical variables are probably factors already (options()$stringsAsFactors == TRUE)\n",
           saf <- "\nCategorical variables are probably strings (options()$stringsAsFactors == FALSE)\n")
    stop(saf, "Recommend saving reference values to seperate table in export")
  }
  if (object$export_options$factorized) warning("already factorized - any changes will be lost")

  table_names <- object$export_options$data_names
  names(table_names) <- NULL
  # not for meta tables
  table_names <- table_names[!table_names %in% object$export_options$meta_names]
  # factorize for every data table in the export
  factorized_tables <- lapply(table_names, function(form_name) {
    curr_form_data <- object[[form_name]]
    # passes to factorize_secuTrial.data.frame
    curr_form_data <- factorize_secuTrial(curr_form_data,
                                          cl = object$cl,
                                          form = form_name,
                                          items = object[[object$export_options$meta_names$items]],
                                          short_names = object$export_options$short_names)
    curr_form_data
  })
  # override with factorized output
  object[table_names] <- factorized_tables
  object$export_options$factorized <- TRUE
  object
}

# nolint start
# #' The data.frame method is used on the individual datasets within the \code{secuTrialdata} object, and relies on \code{cl}
# #' @rdname factorize
# #' @param data a \code{data.frame}, usually from within a \code{secuTrialdata} object
# #' @param cl a \code{data.frame}, usually taken from a \code{secuTrialdata} object, containing at least three variables - 1) \code{column} containing information on which form and variable is to be factorized (formatted as form.variable); 2) \code{code} containing the options that the variable can take as a number; 3) \code{value} contains the text relating to the \code{code} number
# #' @param form which form you are currently working on (used for filtering \code{cl})
# #'
# #' @examples
# # data.frame method
# nolint end
factorize_secuTrial.data.frame <- function(data, cl, form, items, short_names) {
  # character reformatting
  if (!is.character(cl$column)) cl$column <- as.character(cl$column)
  if (!is.character(items$ffcolname)) items$ffcolname <- as.character(items$ffcolname)
  if (!is.character(items$lookuptable)) items$lookuptable <- as.character(items$lookuptable)
  # lookups are similar to catalogues
  lookups <- subset(items, !is.na(items$lookuptable))
  lookups <- unique(lookups[, c("lookuptable", "ffcolname")])

  # needed for the meta variable exception check below
  meta_var_names <- unique(cl$column[! grepl(".", cl$column, fixed = TRUE)])

  str <- strsplit(cl$column, ".", fixed = TRUE)
  # split by "." have two entries and the second is needed
  # the rest has one entry i.e. length(x)
  str <- sapply(str, function(x) x[length(x)])
  cl$var <- str
  w <- cl$column %in% lookups$lookuptable
  cl$var[w] <- lookups$ffcolname[match(cl$column[w], lookups$lookuptable)]

  for (name in names(data)[names(data) %in% cl$var]) {
    # construct search regex
    # condition 1: only subforms (repetitions) have a "mnpsubdocid" column
    # condition 2: this is only appropriate if short_names == TRUE
    if ("mnpsubdocid" %in% names(data) & short_names) {
      adjusted_form <- gsub(form, pattern = "^e", replacement = "^e.+")
      regex_cl <- paste0(adjusted_form, ".", name, "$")
    } else { # non-repetitions (regular case)
      regex_cl <- paste0(form, ".", name, "$")
    }
    # lookup for non-meta variables
    lookup <- cl[grepl(regex_cl, cl$column) |
                   (cl$var %in% names(data) & cl$var %in% lookups$ffcolname), ]

    # exception for meta variables
    if (name %in% meta_var_names) {
      # meta variables do not need to be tied to a form thus the
      # formname does not need to be part of the regex
      lookup <- cl[grepl(paste0("^", name, "$"), cl$column), ]
    }

    # subset lookup rows for only current var name (relevant for lookup tables)
    # if this is not done then additional wrong levels may be set
    lookup <- lookup[which(lookup$var == name), ]

    # exception for non-unique entries in the value column of the lookup table
    # e.g. decoding of mnpptnid to user names can be non-unique
    # e.g. lookuptables can show this behaviour too
    if (any(duplicated(lookup$value))) { #} & name == "mnpptnid") {
      if (name != "mnpptnid") { # fix for Issue #135 on github
        warning(paste0("Duplicate values found during factorization of ", name))
      }
      # concat the value with the code for duplication after first
      lookup$value[which(duplicated(lookup$value))] <-
        paste(lookup$value[which(duplicated(lookup$value))],
              lookup$code[which(duplicated(lookup$value))])
    }

    # this is a fix for Issue #116 on github
    # factorize on same variable name in different forms with different data types
    if (nrow(lookup) == 0) {
      next
    }

    # this is a fix for Issue #121 on github
    # Radiobutton reset option interferes with factorize_secuTrial()
    lookup <- lookup[which(! is.na(lookup$code)), ]

    data[, paste0(name, ".factor")] <- factorize_secuTrial(data[, name], lookup)
    data <- .move_column_after(data, paste0(name, ".factor"), name)
    # check for conversion of all else warn // this is expected to never happen
    if (length(which(is.na(data[, paste0(name, ".factor")]))) >
        length(which(is.na(data[, name]))) &
        # exclude audit trail
        # note: this will also exclude forms with a name starting with
        #       "at" if shortnames is exported
        # exclude mnpfs variables since 0 is not converted
        ! (grepl("^mnpfs", name) | grepl("^at", form))) {
      warning(paste0("Unexpected: Not all levels converted for\n",
                     "  variable: '", name, "' \n  in form: '", form,
                     # fix for Issue #134 on github
                     "'\n  Has a 'Take value from ...' rule been implemented for '", name, "'?"))
    }
  }
  return(data)
}

# nolint start
# #' Methods for individual variables rely on a lookup table with variables code and value. They are basically just wrappers for \code{factor(...)}.
# #' @rdname factorize
# #' @param var a variable
# #' @param lookup a restricted version of cl (filtered based on \code{form}), containing only the rows relevant for \code{var}
# #'
# #' @examples
# nolint end
factorize_secuTrial.numeric <- function(var, lookup) {
  lookup <- unique(lookup)
  f <- factor(var, lookup$code, lookup$value)
  if (!is.null(label(var))) label(f) <- label(var)
  if (!is.null(units(var))) units(f) <- units(var)
  f
}

# #' @rdname factorize
factorize_secuTrial.logical <- function(var, lookup) {
  var <- as.numeric(var)
  f <- factor(var, lookup$code, lookup$value)
  if (!is.null(label(var))) label(f) <- label(var)
  if (!is.null(units(var))) units(f) <- units(var)
  f
}

# #' @rdname factorize
factorize_secuTrial.character <- function(var, lookup) {
  f <- factor(var, lookup$value, lookup$value)
  if (!is.null(label(var))) label(f) <- label(var)
  if (!is.null(units(var))) units(f) <- units(var)
  f
}
