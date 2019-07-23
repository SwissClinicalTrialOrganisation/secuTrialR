#' Add factors to secuTrialdata objects
#' @description secuTrial can return a codebook of codes and labels for categorical variables, including lookup
#'              type variables, if this option is selected in the export tool ('reference values as separate table').
#'              This allows factors to be easily created. Factorize methods exist for \code{secuTrialdata} objects,
#'              \code{data.frames}, \code{integer}s and \code{logical}s, but the intent is that only the former be
#'              used by users. The other methods could be used with customized codebooks.
#' @rdname factorize
#' @name factorize
#' @param x a \code{secuTrialdata} object
#' @export
#' @details factorize_secuTrial will return an error if the appropriate codebook is not available.
#'
#' @examples
#' # load secuTrial export with separate reference table
#' sT_export <- read_secuTrial_export(system.file("extdata",
#'                                                "s_export_CSV-xls_CTU05_longnames_sep_ref.zip",
#'                                                package = "secuTrialR"))
#' # factorize the secuTrialdata object
#' sT_export_factorized <- factorize_secuTrial(sT_export)

# create factors
factorize_secuTrial <- function(x, ...) UseMethod("factorize_secuTrial", x)

#' Method for secuTrialdata objects
#' These objects include all relevant data (assuming that reference values are saved to a separate table,
#' \link[see here for info]{https://swissclinicaltrialorganisation.github.io/secuTrial_recipes/export_data/})
#'
#' @rdname factorize
#' @param object a \code{secuTrialdata} object
#'
#' @return \code{secuTrialdata} object with extra variables in forms for factors (names are appended with \code{.factor})
#' @export
factorize_secuTrial.secuTrialdata <- function(object) {
  if (!object$export_options$refvals_separate) {
    ifelse(options()$stringsAsFactors,
           saf <- "\nCategorical variables are probably factors already (options()$stringsAsFactors == TRUE)\n",
           saf <- "\nCategorical variables are probably strings (options()$stringsAsFactors == FALSE)\n")
    stop(saf, "Recommend saving reference values to seperate table in export")
  }
  if (object$export_options$factorized) warning("already factorized - any changes will be lost")

  x <- object$export_options$data_names
  names(x) <- NULL
  # not for meta tables
  x <- x[!x %in% object$export_options$meta_names]
  # factorize for every data table in the export
  factorized_tables <- lapply(x, function(form_name) {
    curr_form_data <- object[[form_name]]
    # passes to factorize_secuTrial.data.frame
    curr_form_data <- factorize_secuTrial(curr_form_data,
                                          cl = object$cl,
                                          form = form_name,
                                          items = object[[object$export_options$meta_names$items]])
    curr_form_data
  })
  # override with factorized output
  object[x] <- factorized_tables
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
factorize_secuTrial.data.frame <- function(data, cl, form, items) {
  # character reformatting
  if (!is.character(cl$column)) cl$column <- as.character(cl$column)
  if (!is.character(items$ffcolname)) items$ffcolname <- as.character(items$ffcolname)
  if (!is.character(items$lookuptable)) items$lookuptable <- as.character(items$lookuptable)
  lookups <- subset(items, !is.na(items$lookuptable))
  lookups <- unique(lookups[, c("lookuptable", "ffcolname")])

  # needed for the meta variable exception check below
  meta_var_names <- unique(cl$column[! grepl(".", cl$column, fixed = TRUE)])

  str <- strsplit(cl$column, ".", fixed = TRUE)
  # filters for all which were split by "." the rest is "NA"
  str <- sapply(str, function(x) x[length(x)])
  cl$var <- str
  w <- cl$column %in% lookups$lookuptable
  cl$var[w] <- lookups$ffcolname[match(cl$column[w], lookups$lookuptable)]

  for (name in names(data)[names(data) %in% cl$var]) {
    # lookup for non-meta variables
    lookup <- cl[grepl(paste0(form, ".", name, "$"), cl$column) |
                   (cl$var %in% names(data) & cl$var %in% lookups$ffcolname), ]
    # exception for meta variables (for non meta variables the lookup should never be empty)
    if (length(lookup$column) == 0 & name %in% meta_var_names) {
      # meta variables do not need to be tied to a form thus the
      # formname does not need to be part of the regex
      lookup <- cl[grepl(paste0("^", name, "$"), cl$column), ]
    }
    data[, paste0(name, ".factor")] <- factorize_secuTrial(data[, name], lookup)
    data <- .move_column_after(data, paste0(name, ".factor"), name)
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
