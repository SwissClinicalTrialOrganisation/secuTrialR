#' @rdname secuTrialdata
#' @param x secuTrialdata object as returned by \code{load_secuTrial_export}
#'
#' @return data.frame with a row for each table in the export. For each table it
#'         contains the name, number of rows and columns, an indicator for
#'         whether the table is a metadata table and the files original name.
#' @export
#' @usage plot(x)
#'
#' @examples
#' # Print method
#' print(sT_export)
#' # or
#' sT_export

print.secuTrialdata <- function(x){

  cat(paste("SecuTrial data imported from", x$export_options$data_dir, "\n"))

  tab <- lapply(x$export_options$data_names, function(y){
    tmp <- x[[y]]
    tmp
    # print(paste(length(y), nrow(tmp), ncol(tmp)))
    data.frame(table = y,
               nrow = nrow(tmp),
               ncol = ncol(tmp),
               meta = y %in% x$export_options$meta_names)
  })
  tab <- do.call("rbind", tab)
  tab$original_name <- rownames(tab)
  rownames(tab) <- NULL
  print(tab, row.names = FALSE)

}

#' @rdname secuTrialdata
#' @usage plot(x)
plot.secuTrialdata <- function(x){
  vs <- visitstructure(x)
  plot(vs)
}




#' @rdname visit_structure
#' @usage plot(x)
#' @export
#' @examples
#'   vs <- visit_structure(sT_export)
#'   plot(vs)
#'   # or, equivalently
#'   plot(sT_export)

plot.secuTrialvisit <- function(x){
  # construct the figure
  z <- !is.na(as.matrix(r[, grepl("tmpvar", names(r))]))
  names <- gsub("tmpvar.", "", names(r[, grepl("tmpvar", names(r))]))
  paropts <- par()
  on.exit(paropts)
  par(mai = c(0,0,0.1,0.1))
  layout(matrix(c(0,1,0,0), 2, 2, byrow = TRUE))
  image(t(z), yaxt = "n", xaxt = "n", col = c("white", "black"))
  axis(2, r$formname, at = 0:(nrow(r)-1)/(nrow(r)-1), las = 1)
  axis(1, names, at = 0:(length(names)-1)/(length(names)-1), las = 2)
}


