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






