#' Get the visit structure of secuTrialdata objects
#'
#' @param x secuTrialdata object
#'
#' @return data.frame with 1 for whether a form (rows) was collected during a particular visit (columns)
#' @export
#'
#' @name visit_structure
#' @rdname visit_structure
#'
#' @examples
#' #' # prepare path to example export
#' export_location <- system.file("extdata", "s_export_CSV-xls_BMD.zip",
#'                                package = "secuTrialR")
#' # load all export data
#' sT_export <- load_secuTrial_export(data_dir = export_location)
#' visit_structure(sT_export)
#'
visit_structure <- function(x){
  tmp <- merge(x$visitplan, x$visitplanforms)
  tmp <- merge(tmp, x$forms)
  u <- unique(tmp[, c("mnpvislabel", "formname")])
  u$tmpvar <- 1
  r <- reshape(u, direct = "wide",
               timevar = "mnpvislabel",
               idvar = "formname", v.names = "tmpvar")
  # column order
  tmpl <- split(x$visitplan, x$visitplan$mnpvsno)[[1]]
  tmpl$r <- 1:nrow(tmpl)
  neworder <- c(1, tmpl$r[match(tmpl$mnpvislabel, gsub("tmpvar.", "", names(r)[2:ncol(r)]))]+1)
  r <- r[, neworder]
  # row order
  tmpl <- split(x$forms, x$forms$mnpvslbl)[[1]]
  f <- as.character(tmpl$formname)
  neworder <- na.exclude((1:length(f))[match(f, r$formname)])
  ro <- r[neworder, ]
  names <- gsub("tmpvar.", "", names(ro[, grepl("tmpvar", names(ro))]))
  names(ro)[2:ncol(ro)] <- names

  class(ro) <- c("secuTrialvisit", "data.frame")
  return(ro)
}

