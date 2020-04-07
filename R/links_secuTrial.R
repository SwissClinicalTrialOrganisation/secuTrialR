#' Show links between forms
#'
#' secuTrial creates a large number of files and identifiers with which to link files together.
#' Understanding the links can be difficult. This function produces a map linking the forms
#' with common variables.
#'
#' @param object \code{secuTrialdata} object
#' @param forms a regular expression for which forms should be included
#' @param formcol color for form name circles
#' @param varcol color for variable name circles
#' @param plot boolean specifies if the plot should be shown
#' @details We recommend to resize the tcltk window and and click view/"fit to screen" to improve readability.
#'          Forms are colored dull orange, variables are colored light blue.
#' @note Note that where a form name is also a variable name, it is appended by \code{_form}
#'       (igraph requires uniquely named nodes).
#' @return a tcltk plot window.
#' @import igraph
#' @import tcltk
#' @export
#' @examples
#' \donttest{
#' # ex. 1
#' # prepare path to example export
#' export_location <- system.file("extdata", "sT_exports", "BMD",
#'                                "s_export_CSV-xls_BMD_short_en_utf8.zip",
#'                                package = "secuTrialR")
#' # load all export data
#' sT_export <- read_secuTrial_raw(data_dir = export_location)
#' # plot links
#' links_secuTrial(sT_export)
#'
#' # ex. 2
#' # prepare path to example export
#' export_location <- system.file("extdata", "sT_exports", "lnames",
#'                                "s_export_CSV-xls_CTU05_long_ref_miss_en_utf8.zip",
#'                                package = "secuTrialR")
#' # load all export data
#' sT_export <- read_secuTrial_raw(data_dir = export_location)
#' # plot links for form names starting with "ctu05"
#' links_secuTrial(sT_export, forms = "^ctu05")
#' }
links_secuTrial <- function(object, forms = NULL, formcol = "#d8b365", varcol = "#e5f5f9", plot = TRUE) {
  if (!class(object) == "secuTrialdata") stop("object of class secuTrialdata expected")
  obj <- object[2:length(object)]


  if (object$export_options$audit_trail) {
    names <- names(obj)
    names2 <- gsub("^at", "", names)
    names <- names[!names %in% unique(names2)]
    obj <- obj[-which(names(obj) %in% names)]
    rm(names)
  }

  names <- lapply(obj, names)

  x <- lapply(names(names), function(x) {
    l <- ncol(obj[[x]])
    rx <- rep(x, l)
    data.frame(form = rx, var = names(obj[[x]]), stringsAsFactors = FALSE)
    })
  x <- do.call("rbind", x)
  if (any(x$form %in% x$var)) {
    w <- which(x$form %in% x$var)
    x$form[w] <- paste0(x$form[w], "_form")
  }
  sx <- split(x, x$var)
  sxid <- mapply(function(x, y) {
    x$q_id <- y
    x$q_n <- nrow(x)
    x
  }
  , sx, 1:length(unique(x$var)), SIMPLIFY = FALSE)
  qid <- do.call("rbind", sxid)
  sx <- split(qid, qid$form)
  sxid <- mapply(function(x, y) {
    x$f_id <- y
    x
  }
  , sx, 1:length(unique(qid$form)), SIMPLIFY = FALSE)
  fqid <- do.call("rbind", sxid)
  if (!is.null(forms)) fqid <- fqid[grepl(forms, fqid$form), ]
  fqid <- fqid[fqid$q_n > 1, ]

  vs <- data.frame(node = c(fqid$form, fqid$var),
                   y = rep(1:0, each = nrow(fqid)),
                   stringsAsFactors = FALSE)

  vs <- vs[!duplicated(vs), ]

  g <- igraph::graph_from_data_frame(fqid[, c("form", "var")],
                             directed = FALSE,
                             vertices = vs)
  igraph::V(g)$color <- ifelse(igraph::V(g)$y == 0, varcol, formcol)
  if (plot) {
    igraph::tkplot(g)
  } else {
    # for testing purposes
    length(unlist(g))
  }
}
