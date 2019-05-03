#' Add factors to secuTrialdata objects
#' @description SecuTrial can return a codebook of codes and labels for categorical variables, if this option is selected in the export tool. This allows factors to be easily created. Factorize methods exist for \code{secuTrialdata} objects, \code{data.frames}, \code{integer}s and \code{logical}s, but the intent is that only the former be used by users. The other methods could be used with customized codebooks.
#' @rdname factorize
#' @name factorize
#' @param x a \code{secuTrialdata} object
#' @export
#' @details factorize_secuTrial will return an error if the appropriate codebook is not available.
#'
#' @examples
#' # TODO - NO SUITABLE EXPORT AVAILABLE IN THE PACKAGE

# create factors
factorize_secuTrial <- function(x, ...) UseMethod("factorize_secuTrial", x)

#' Method for secuTrialdata objects
#' These objects include all relevant data (assuming that reference values are saved to a separate table, \link[see here for info]{https://swissclinicaltrialorganisation.github.io/secuTrial_recipes/export_data/})
#' @rdname factorize
#' @param object a \code{secuTrialdata} object
#'
#' @return \code{secuTrialdata} object with extra variables in forms for factors (names are appended with \code{.factor})
#' @export
#'
#' @examples
factorize_secuTrial.secuTrialdata <- function(object){
  if(!object$export_options$refvals_separate) {
    ifelse(options()$stringsAsFactors,
           saf <- "\nCategorical variables are probably factors already (options()$stringsAsFactors == TRUE)\n",
           saf <- "\nCategorical variables are probably strings (options()$stringsAsFactors == FALSE)\n")
    stop(saf, "Recommend saving reference values to seperate table in export")
  }
  x <- object$export_options$data_names
  names(x) <- NULL
  x <- x[!x %in% object$export_options$meta_names]
  obs <- lapply(x, function(obj){
    # print(obj)
    tmp <- object[[obj]]
    tmp <- factorize_secuTrial(tmp, object$cl, form = obj)
    tmp
  })
  obs
  object[x] <- obs
  object
}

#' The data.frame method is used on the individual datasets within the \code{secuTrialdata} object, and relies on \code{cl}
#' @rdname factorize
#' @param data a \code{data.frame}, usually from within a \code{secuTrialdata} object
#' @param cl a \code{data.frame}, usually taken from a \code{secuTrialdata} object, containing at least three variables - 1) \code{column} containing information on which form and variable is to be factorized (formatted as form.variable); 2) \code{code} containing the options that the variable can take as a number; 3) \code{value} contains the text relating to the \code{code} number
#' @param form which form you are currently working on (used for filtering \code{cl})
#'
#' @export
#'
#' @examples
# data.frame method
factorize_secuTrial.data.frame <- function(data, cl, form){
  if(!is.character(cl$column)) cl$column <- as.character(cl$column)

  str <- strsplit(cl$column, ".", fixed = TRUE)
  str <- sapply(str, function(x) x[2])
  cl$var <- str

  for(i in names(data)[names(data) %in% cl$var]){
    # print(i)
    lookup <- cl[grepl(paste0(form, ".", i, "$"), cl$column), ]
    data[, paste0(i, ".factor")] <- factorize_secuTrial(data[, i], lookup)
  }
  return(data)
}

#' Methods for individual variables rely on a lookup table with variables code and value. They are basically just wrappers for \code{factor(...)}.
#' @rdname factorize
#' @param var a variable
#' @param lookup a restricted version of cl (filtered based on \code{form}), containing only the rows relevant for \code{var}
#'
#' @export
#'
#' @examples
factorize_secuTrial.integer <- function(var, lookup){
  lookup <- unique(lookup)
  # print(lookup)
  factor(var, lookup$code, lookup$value)
}

#' @rdname factorize
factorize_secuTrial.logical <- function(var, lookup){
  var <- as.numeric(var)
  factor(var, lookup$code, lookup$value)
}

#' @rdname factorize
factorize_secuTrial.character <- function(var, lookup){
  factor(var, lookup$value, lookup$value)
}
