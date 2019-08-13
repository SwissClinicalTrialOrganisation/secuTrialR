#' Plots the recruitment over time for secuTrialdata objects
#' @description secuTrial exports inherently contain the information on which case was
#'              registered at which point in time. This function makes use of this property
#'              to plot recruitment over time.
#' @param x a \code{secuTrialdata} object
#' @param return_data logical - return the data used to produce the plot
#' @export
#' @details plot_recruitment will return a simple line plot showing recruitment over time
#'
#' @examples
#' # export location
#' expot_loc <- system.file("extdata",
#'                          "s_export_CSV-xls_CTU05_longnames_sep_ref.zip",
#'                          package = "secuTrialR")
#' # read export
#' sT_export <- read_secuTrial(expot_loc)
#'
#' # plot recruitment
#' plot_recruitment(sT_export)
#'
plot_recruitment <- function(x, return_data = FALSE) {
  if (class(x) == "secuTrialdata") {
    cn <- x[[x$export_options$meta_names$casenodes]]
    dates <- sort(as.Date(cn$mnpvisstartdate))
    dates_df <- data.frame(dates, 1:length(dates))
    names(dates_df) <- c("date", "case_count")
    plot(dates_df$date, dates_df$case_count, type = "l", lwd = 2, col = "steelblue",
         main = "Recruitment over time", xlab = "Date", ylab = "Count")
    if (return_data) {
      return(dates_df)
    }
  } else {
    stop("plot_recruitment requires objects of the class 'secuTrialdata' as input.")
  }
}
