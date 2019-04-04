#' Show links between forms
#'
#' SecuTrial creates a large number of files and identifiers with which to link files together. Understanding the links can be difficult. This function produces a map linking the forms with common variables.
#'
#' @param object secuTrialdata object
#' @param forms a regular expression for which forms should be included
#' @details We recommend to resize the tikz window and and click view::fit to screen to improve readability. Forms are coloured red, variables are coloured blue. Note that where a form name is also a variable name, it is appended by \code{_form} (igraph requires uniquely named nodes).
#' @return a tikz plot window.
#' @export
#'
#' @examples
#' # prepare path to example export
#' export_location <- system.file("extdata", "s_export_CSV-xls_BMD.zip",
#'                                package = "secuTrialR")
#' # load all export data
#' sT_export <- load_secuTrial_export(data_dir = export_location)
#' # get labels
#' links_secuTrial(sT_export)
links_secuTrial <- function(object, forms = NULL){
  if(!class(object) == "secuTrialdata") stop("object of class secuTrialdata expect")
  obj <- object[2:length(object)]

  names <- lapply(obj, names)

  x <- lapply(names(names), function(x){
    # print(x)
    l <- ncol(obj[[x]])
    rx <- rep(x, l)
    data.frame(form = rx, var = names(obj[[x]]), stringsAsFactors = FALSE)
    })
  x <- do.call("rbind", x)
  if(any(x$form %in% x$var)){
    w <- which(x$form %in% x$var)
    x$form[w] <- paste0(x$form[w], "_form")
  }
  sx <- split(x, x$var)
  sxid <- mapply(function(x, y){
    x$q_id <- y
    x$q_n <- nrow(x)
    x
  }, sx, 1:length(unique(x$var)), SIMPLIFY = FALSE)
  qid <- do.call("rbind", sxid)
  sx <- split(qid, qid$form)
  sxid <- mapply(function(x, y){
    x$f_id <- y
    x
  }, sx, 1:length(unique(qid$form)), SIMPLIFY = FALSE)
  fqid <- do.call("rbind", sxid)
  if(!is.null(forms)) fqid <- fqid[grepl(forms, fqid$form), ]
  fqid <- fqid[fqid$q_n > 1, ]

  vs <- data.frame(node = c(fqid$form, fqid$var),
                   y = rep(1:0, each = nrow(fqid)),
                   stringsAsFactors = FALSE)

  vs <- vs[!duplicated(vs), ]

  g <- graph_from_data_frame(fqid[, c("form", "var")],
                             directed = FALSE,
                             vertices = vs)
  V(g)$color <- ifelse(V(g)$y == 0, "blue", "red")
  tkplot(g, layout = layout_as_tree)

}







