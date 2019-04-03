#' Add factors to secuTrialdata objects
#'
#' @rdname factor
#' @name factor
#' @param x a \code{secuTrialdata} object
#' @return
#' @export
#'
#' @examples

# create factors
factorize_secuTrial <- function(x, ...) UseMethod("factorize_secuTrial", x)

#' Method for secuTrialdata objects
#' @rdname factor
#' @param object a \code{secuTrialdata} object
#'
#' @return object with extra variables in forms for factors (names are appended with \code{.factor})
#' @export
#'
#' @examples
factorize_secuTrial.secuTrialdata <- function(object){
  if(!object$export_options$refvals_separate) stop("Reference values not available. Save reference values to seperate table in export")
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

#' Method for data.frame objects
#' @rdname factor
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

#' Method for integer objects
#' @rdname factor
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

#' Method for logical objects
#' @rdname factor
factorize_secuTrial.logical <- function(var, lookup){
  var <- as.numeric(var)
  factor(var, lookup$code, lookup$value)
}
