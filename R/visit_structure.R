#' Get the visit structure of secuTrialdata objects
#'
#' @param x secuTrialdata object
#' @note Requires a fixed visit structure - an error will be returned for projects without a visit structure or one with flexible visits
#' @return data.frame with 1 for whether a form (rows) was collected during a particular visit (columns)
#' @export
#'
#' @name visit_structure
#' @rdname visit_structure
#'
#' @examples
#' # prepare path to example export
#' export_location <- system.file("extdata", "s_export_CSV-xls_BMD.zip",
#'                                package = "secuTrialR")
#' # load all export data
#' sT_export <- read_secuTrial_export(data_dir = export_location)
#' \dontrun{
#' visit_structure(sT_export)
#' }
visit_structure <- function(x){
  if (class(x)[1] != "secuTrialdata") stop("'secuTrialdata object required'")
  vp <- x[[x$export_options$meta_names$visitplan]]
  if (any(is.na(vp$mnpvsno))) stop(paste("Visits do not appear to be a part of this database or",
                                         "only flexible visits defined\nvisit_structure requires",
                                         "a fixed structure"))
  vpf <- x[[x$export_options$meta_names$visitplanforms]]
  f <- x[[x$export_options$meta_names$forms]]
  for (j in c("vp", "f", "vpf")) {
    tmp <- get(j)
    for (i in names(tmp)[sapply(tmp, is.factor)]) {
      tmp[, i] <- as.character(tmp[, i])
    }
    assign(j, tmp)
  }
  tmp <- merge(vp, vpf, by = "mnpvisid")
  tmp <- merge(tmp, f, by = "formid")
  u <- unique(tmp[, c("mnpvislabel", "formname")])
  u$tmpvar <- 1
  r <- reshape(u, direct = "wide",
               timevar = "mnpvislabel",
               idvar = "formname", v.names = "tmpvar")
  # column order
  tmpl <- split(vp, vp$mnpvsno)
  w <- which(sapply(tmpl, function(x) all(unique(u$mnpvislabel) %in% x$mnpvislabel)))[1]
  tmpl <- tmpl[[w]]
  tmpl <- tmpl[!duplicated(tmpl$mnpvislabel), ]
  tmpl$r <- 1:nrow(tmpl)
  neworder <- c(1, tmpl$r[match(tmpl$mnpvislabel, gsub("tmpvar.", "", names(r)[2:ncol(r)]))] + 1)
  r <- r[, neworder]
  # row order
  tmpl <- split(f, f$mnpvslbl)
  w <- which(sapply(tmpl, function(x) all(unique(u$formname) %in% x$formname)))[1]
  tmpl <- tmpl[[1]]
  f <- as.character(tmpl$formname)
  neworder <- na.exclude( (1:length(f))[match(f, r$formname)])
  ro <- r[neworder, ]
  names <- gsub("tmpvar.", "", names(ro[, grepl("tmpvar", names(ro))]))
  names(ro)[2:ncol(ro)] <- names

  class(ro) <- c("secuTrialvisit", "data.frame")
  return(ro)
}

#' @rdname visit_structure
#' @usage plot(x)
#' @export
#' @examples
#'   vs <- visit_structure(sT_export)
#'   plot(vs)
#'   # or, equivalently
#'   plot(sT_export)

plot.secuTrialvisit <- function(r){
  # construct the figure
  z <- !is.na(as.matrix(r[, -1]))
  names <- gsub("tmpvar.", "", names(r[, -1]))
  paropts <- par()
  on.exit(paropts)
  par(mai = c(0, 0, 0.1, 0.1))
  layout(matrix(c(0, 1, 0, 0), 2, 2, byrow = TRUE))
  image(t(z), yaxt = "n", xaxt = "n", col = c("white", "black"))
  axis(2, r$formname, at = 0:(nrow(r) - 1) / (nrow(r) - 1), las = 1)
  axis(1, names, at = 0:(length(names) - 1) / (length(names) - 1), las = 2)
}
