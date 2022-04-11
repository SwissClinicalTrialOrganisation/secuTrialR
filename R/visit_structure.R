#' Get the visit structure of \code{secuTrialdata} objects
#'
#' @param x a \code{secuTrialdata} object
#' @param sorted logical if TRUE sorted by first visit
#' @note Requires a fixed visit structure - an error will be returned for projects without
#'       a visit structure or one with flexible visits
#' @return data.frame with 1 for whether a form (rows) was collected during a particular visit (columns)
#' @export
#'
#' @name visit_structure
#' @rdname visit_structure
#'
#' @examples
#' export_location <- system.file("extdata", "sT_exports", "lnames",
#'                                "s_export_CSV-xls_CTU05_long_ref_miss_en_utf8.zip",
#'                                package = "secuTrialR")
#' # read all export data
#' sT_export <- read_secuTrial(data_dir = export_location)
#' # get visit structure
#' vs <- visit_structure(sT_export)
#' # plot
#' plot(vs)
visit_structure <- function(x, sorted = TRUE) {
  if (class(x)[1] != "secuTrialdata") stop("'secuTrialdata object required'")
  if (! x$export_options$proj_setup) stop("Project setup data needs to be in the export but is not.")
  vp <- x[[x$export_options$meta_names$visitplan]]
  if (any(is.na(vp$mnpvisid))) stop(paste("Visits do not appear to be a part of this database or",
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
  r <- reshape(u, direction = "wide",
               timevar = "mnpvislabel",
               idvar = "formname", v.names = "tmpvar")
  # column order
  visits <- aggregate(visitnumber ~ mnpvislabel, vp, median)
  vis_order <- as.character(visits$mnpvislabel[order(visits$visitnumber)])
  # row order
  ff <- aggregate(formid ~ formname, f, median)
  form_order <- as.character(ff$formname[order(ff$formid)])
  form_order <- intersect(form_order, r$formname)

  # adjust names
  rownames(r) <- r$formname
  names(r) <- gsub("tmpvar.", "", names(r))

  ro <- r[form_order, c("formname", vis_order)]

  # By default (sorted = TRUE), forms are sorted by first visit of
  # occurence and number of occurences.
  if (sorted) {
    # where does which form appear
    z_input <- !is.na(as.matrix(ro[, -1]))
    # how often is each form used
    n_uses <- apply(z_input, 1, sum)
    # which visit first?
    first_use <- apply(z_input, 1, function(x) match(TRUE, x))
    # sort on when was used and and how often
    ro <- ro[order(first_use, n_uses, decreasing = FALSE), ]
  }
  class(ro) <- c("secuTrialvisit", "data.frame")
  return(ro)
}

#' @rdname visit_structure
#' @param ... further parameters
#' @export
#' @return plot of the visit plan
plot.secuTrialvisit <- function(x, ...) {
  # construct the figure.
  z <- !is.na(as.matrix(x[, -1]))
  names <- gsub("tmpvar.", "", names(x[, -1]))
  paropts <- par(no.readonly = TRUE)
  on.exit(par(paropts))
  par(mai = c(0, 0, 0.1, 0.1))
  layout(matrix(c(0, 1, 0, 0), 2, 2, byrow = TRUE))
  image(t(z), yaxt = "n", xaxt = "n", col = c("white", "black"))
  axis(2, x$formname, at = 0:(nrow(x) - 1) / (nrow(x) - 1), las = 1)
  axis(1, names, at = 0:(length(names) - 1) / (length(names) - 1), las = 2)
}
