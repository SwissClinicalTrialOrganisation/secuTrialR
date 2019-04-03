# create factors
factorize_secuTrial <- function(x, ...) UseMethod("factorize_secuTrial", x)

# secuTrialdata method
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

# integer method
factorize_secuTrial.integer <- function(data, lookup){
  lookup <- unique(lookup)
  # print(lookup)
  factor(data, lookup$code, lookup$value)
}

# logical method
factorize_secuTrial.logical <- function(data, lookup){
  data <- as.numeric(data)
  factor(data, lookup$code, lookup$value)
}
