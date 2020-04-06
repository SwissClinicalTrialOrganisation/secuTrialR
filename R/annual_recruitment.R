#' Returns a data.frame showing the annual recruitment per center
#' @description secuTrial exports inherently contain the information on which participant was
#'              registered at which point in time. This function makes use of this property
#'              to show annual recruitment.
#' @note This function wraps plot_recruitment to retrieve the data.
#' @param x a \code{secuTrialdata} object
#' @param rm_regex character - specifies a regular expression to be removed from the centre names in the legend.
#                                              # this is escaped three times here to
#                                              # show up properly in the help file
#'                             e.g. rm_regex = "\\\(.*\\\)$" will remove trailing brackets and their contents.
#' @export
#' @importFrom lubridate year
#' @importFrom purrr modify_if
#' @importFrom dplyr distinct
#' @return a data.frame showing the annual recruitment counts per center
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
#' annual_recruitment(sT_export)
#'
#' # show without trailing bracket
#' annual_recruitment(sT_export, rm_regex = "\\(.*\\)$")
#'
annual_recruitment <- function(x, rm_regex = "") {
  if (class(x) == "secuTrialdata") {
    # wrap plot_recruitment to retrieve data
    recruitment_data <- plot_recruitment(x, return_data = TRUE)
    # construct header
    header <- c("Center", "Total", unique(year(recruitment_data[[1]]$date)))

    # init output
    recruitment_table <- data.frame(matrix(ncol = length(header), nrow = 0))

    for (i in 1:length(recruitment_data)) {
      curr_dat <- recruitment_data[[i]]
      curr_centre <- unique(curr_dat$centre_name)
      # this is only true for the first data.frame in the list
      if (length(curr_centre) > 1) {
        curr_centre <- "All"
      }
      year_counts <- table(year(curr_dat$date))
      counts_vect <- c()
      # 1 and 2 are "Center" and "Total"
      #i.e. skip them and only look at the years
      for (j in 3:length(header)) {
        curr_year <- header[j]
        curr_count <- as.numeric(year_counts[curr_year])
        if (is.na(curr_count)) {
          curr_count <- 0
        }
        counts_vect <- c(counts_vect, curr_count)
      }
      output_line <- c(curr_centre, sum(counts_vect), counts_vect)
      recruitment_table <- rbind(recruitment_table, output_line)
    }
    # adjust names
    names(recruitment_table) <- header
    # add rows for missing centres
    ctr <- x[[x$export_options$meta_names$centres]]
    missing_centers <- ctr$mnpctrname[which(! ctr$mnpctrname %in% recruitment_table$Center)]
    for (centre in missing_centers) {
      recruitment_table <- rbind(recruitment_table,
                                 c(centre, rep(0, length(recruitment_table[1, ]) - 1)))
    }
    # apply rm_regex
    recruitment_table$Center <- trimws(gsub(recruitment_table$Center, pattern = rm_regex, replacement = ""))
    # remove duplicate lines (this will happen if there is only one center)
    recruitment_table <- distinct(recruitment_table)
    return(recruitment_table)
  } else {
    stop("annual_recruitment requires objects of the class 'secuTrialdata' as input.")
  }
}
