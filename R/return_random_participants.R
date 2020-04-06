#' Returns the random participants from a secuTrial export
#' @description There are situations (e.g. randomized monitoring) in which you may want to
#'              return a list of random participants from a secuTrial export.
#' @param x a \code{secuTrialdata} object
#' @param centres A character vector of centres for which participants should be returned. If left
#'                unspecified it will return participants for every study centre.
#' @param percent A number greater than 0 and smaller than 1 specifying the approximate percentage
#'                of participants to be returned per centre.
#' @param date If only participants after a specific date should be considered this can be entered here.
#'             Format should be "YYYY-MM-DD" (e.g. "2011-03-26" for March 26th 2011).
#'             This date is checked against mnpvisstartdate in the casenodes table.
#' @param seed Allows to configure a seed for id sampling. Every centre will use a small variation of this seed.
#' @export
#' @importFrom lubridate ymd
#' @importFrom dplyr filter
#' @return  list of two elements. First, a data.frame that contains the
#'          random participants from each specified centre. This is performed based on a specified seed to retain
#'          reproducibilty. Second, the configuration of the randomization (i.e. result of \code{RNGkind()}).
#'          If the percentage does not yield an integer for a centre the number is tranformed into an integer
#'          under application of the \code{ceiling()} function (i.e. it is rounded up).
#'
#' @examples
#' # export location
#' expot_loc <- system.file("extdata", "sT_exports", "lnames",
#'                          "s_export_CSV-xls_CTU05_long_ref_miss_en_utf8.zip",
#'                          package = "secuTrialR")
#' # read export
#' sT_export <- read_secuTrial(expot_loc)
#'
#' # return random participants
#' return_random_participants(sT_export, percent = 0.25, seed = 1337, date = "2019-03-18",
#'                            centres = c("Inselspital Bern (RPACK)", "Charité Berlin (RPACK)"))
#'
return_random_participants <- function(x, centres = "all", percent = 0.1, date = "1900-01-01", seed = 1) {
  if (class(x) == "secuTrialdata") {

     # check for meta data Structure available
     if (! x$export_options$structure) {
       stop("Structure meta data not exported.\n  Please enable this option in ExportSeachTool and reexport.")
     }

     cn_table <- x[[x$export_options$meta_names$casenodes]]
     ctr_table <- x[[x$export_options$meta_names$centres]]
     # need add ids because these will be returned
     if (! x$export_options$add_id) {
       stop("Please reexport with Add-ID.")
     }
     # need centre info
     if (! x$export_options$centre_info) {
       stop("Please reexport with Centre information.")
     }
     # check for centres being part of the export (github issue #151)
     if (! "all" %in% centres) {
       missing_ctr <- centres[which(! centres %in% ctr_table$mnpctrname)]
       if (length(missing_ctr)) {
         stop(paste(paste(missing_ctr, collapse = ", "),
                    "not part of the specified export.\nViable centres are:",
                    paste(ctr_table$mnpctrname, collapse = ", ")))
       }
     }
     # 0 and 1 can also be excluded because 0 means nothing
     # and 1 means everything. Both cases are implicitly useless.
     if (percent <= 0 | percent >= 1) {
       stop("Please specify a number greater than 0 and smaller than 1.")
     }
     # check date format
     date_parsed <- ymd(date)
     if (is.na(date_parsed)) {
       stop("The specified date is not parsable by lubridate::ymd. Please use format 'YYYY-MM-DD'.")
     }

     if ("all" %in% centres) {
       centres <- ctr_table$mnpctrname
     }

     # add centre strings
     cn_table <- add_centre_col(cn_table, casenodes_table = cn_table, centre_table = ctr_table)
     # remove participants before specified date
     removed_date_idx <- which(ymd(cn_table$mnpvisstartdate) <= date_parsed)
     if (length(removed_date_idx)) {
       cn_table <- cn_table[-removed_date_idx, ]
     }
     # init output data.frame
     random_participants <- setNames(data.frame(matrix(ncol = 3, nrow = 0)),
                                     c("mnpaid", "centre", "mnpvisstartdate"))
     for (ctr in centres) {
       # different seed for every centre
       seed <- seed + 1
       curr_ctr_cn <- cn_table %>% filter(cn_table$centre == ctr)
       curr_ctr_cn_trunc <- curr_ctr_cn[, c("mnpaid", "centre", "mnpvisstartdate")]
       participant_ids_count <- length(curr_ctr_cn$mnpaid)
       # sample count / round up
       sample_count <- ceiling(participant_ids_count * percent)
       # set seed
       set.seed(seed)
       # sample
       random_participants <- rbind(random_participants,
                                    curr_ctr_cn_trunc[sample(nrow(curr_ctr_cn_trunc), sample_count), ])
     }
     rng_config <- RNGkind()
     return_list <- list(random_participants, rng_config)
     names(return_list) <- c("participants", "rng_config")
     return(return_list)
  } else {
    stop("return_random_participants requires objects of the class 'secuTrialdata' as input.")
  }
}
