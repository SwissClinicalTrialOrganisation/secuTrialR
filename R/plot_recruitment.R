#' Plots the recruitment over time for \code{secuTrialdata} objects
#' @description secuTrial exports inherently contain the information on which participant was
#'              registered at which point in time. This function makes use of this property
#'              to plot recruitment over time. Centers indicated with a black line in the
#'              legend did not recruit at all.
#' @param x a \code{secuTrialdata} object
#' @param return_data logical - return the data used to produce the plot instead of the plot
#' @param show_centres logical - subset the data into centres
#' @param cex double - specifies font size in legend
#' @param rm_regex character - specifies a regular expression to be removed from the centre names in the legend.
#                                              # this is escaped three times here to
#                                              # show up properly in the help file
#'                             e.g. rm_regex = "\\\(.*\\\)$" will remove trailing brackets and their contents.
#' @export
#' @importFrom purrr modify_if
#' @return a simple line plot showing recruitment over time
#'         or a list of data.frames if return_data is set to TRUE
#'
#' @examples
#' # export location
#' expot_loc <- system.file("extdata", "sT_exports", "lnames",
#'                          "s_export_CSV-xls_CTU05_long_ref_miss_en_utf8.zip",
#'                          package = "secuTrialR")
#' # read export
#' sT_export <- read_secuTrial(expot_loc)
#'
#' # plot recruitment
#' plot_recruitment(sT_export)
#'
#' # plot without trailing bracket
#' plot_recruitment(sT_export, rm_regex = "\\(.*\\)$")
#'
plot_recruitment <- function(x, return_data = FALSE, show_centres = TRUE, cex = 1, rm_regex = "") {
  if (class(x) == "secuTrialdata") {

    # check for meta data Structure available
    if (! x$export_options$structure) {
      stop("Structure meta data not exported.\n  Please enable this option in ExportSeachTool and reexport.")
    }

    ctr <- x[[x$export_options$meta_names$centres]]
    cn <- x[[x$export_options$meta_names$casenodes]]
    dates_centre_ids <- .prep_line_data(cn = cn, ctr = ctr, encoding = x$export_options$encoding)
    # for return_data
    plot_data <- list(dates_centre_ids)
    # legend
    legend_names <- paste0("All (n=", nrow(plot_data[[1]]), ")")
    if (return_data & (! show_centres)) {
      return(plot_data)
    } else if (! return_data) {
      plot(dates_centre_ids$date, dates_centre_ids$pat_count, type = "s", lwd = 2, col = "steelblue",
           main = "Recruitment over time", xlab = "Date of enrollment", ylab = "Participant count")
    }
    # centre recruitment
    if (show_centres) {
      # create uniqe colors for each centre
      cols <-  rainbow(length(ctr$mnpctrid))
      col_idx <- 1
      for (centre_id in ctr$mnpctrid) {
        curr_ctr_cn <- subset(cn, cn$mnpctrid == centre_id)
        if (! length(curr_ctr_cn$mnppid)) {
          # centres with 0 entries are labelled black in the legend
          cols[col_idx] <- "black"
          legend_names <- c(legend_names, paste0(gsub(ctr$mnpctrname[which(ctr$mnpctrid == centre_id)],
                                                                         pattern = rm_regex, replacement = "")
                                                 , " (n=0)"))
          col_idx <- col_idx + 1
          next
        }
        dates_centre_ids_curr_ctr <- .prep_line_data(cn = curr_ctr_cn, ctr = ctr,
                                                     encoding = x$export_options$encoding)
        legend_names <- c(legend_names, paste0(gsub(unique(dates_centre_ids_curr_ctr$centre_name),
                                                    pattern = rm_regex, replacement = ""),
                                               " (n=",
                                               nrow(dates_centre_ids_curr_ctr),
                                               ")"))
        # append to return data
        plot_data[[length(plot_data) + 1]] <- dates_centre_ids_curr_ctr
        if (! return_data) {
          lines(dates_centre_ids_curr_ctr$date, dates_centre_ids_curr_ctr$pat_count,
                lwd = 2, col = cols[col_idx], lty = 2, type = "s")
        }
        col_idx <- col_idx + 1
      }
      # legend plots last
      if (! return_data) {
        legend("topleft", col = c("steelblue", cols),
               legend = legend_names, lty = 1, cex = cex, bty = "n")
      }
      if (return_data) {
        return(plot_data)
      }
    }
  } else {
    stop("plot_recruitment requires objects of the class 'secuTrialdata' as input.")
  }
}

# helper function to prep the data for the line plot
# take a casenodes (cn) and centres (ctr) data frame from the
# secuTrial export
.prep_line_data <- function(cn, ctr, encoding = "UTF-8") {
  dates_centre_ids <- cn[, c("mnpvisstartdate", "mnpctrid")]
  # set to "Date" class
  dates_centre_ids$mnpvisstartdate <- as.Date(dates_centre_ids$mnpvisstartdate)
  # sort
  dates_centre_ids <- dates_centre_ids[order(dates_centre_ids$mnpvisstartdate), ]
  # add count column
  dates_centre_ids$pat_count <- 1:nrow(dates_centre_ids)
  names(dates_centre_ids) <- c("date", "centre_id", "pat_count")
  # translate centre names
  dates_centre_ids$centre_name <- ctr$mnpctrname[match(dates_centre_ids$centre_id, ctr$mnpctrid)]
  if (encoding != "UTF-8") {
    Encoding(dates_centre_ids$centre_name) <- "latin1"
  }
  return(dates_centre_ids)
}
