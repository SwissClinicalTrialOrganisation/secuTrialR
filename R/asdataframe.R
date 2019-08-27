#' as.data.frame method for secuTrialdata objects
#'
#' @param object secuTrialdata object
#' @param data.frames character vector of data.frame names to turn into data.frames
#' @param meta logical should metadata be returned
#' @param regex regex syntax to remove from names
#' @param rep replacement for regex
#' @param envir environment in which to put the data
#'
#' @return each data.frame on the secuTrialdata object is saved to it's own data.frame in the environment
#' @export
#'
#' @examples
#' # prepare path to example export
#' export_location <- system.file("extdata",
#'                                "s_export_CSV-xls_CTU05_longnames_sep_ref.zip",
#'                                package = "secuTrialR")
#' # load all export data
#' sT_export <- read_secuTrial_export(data_dir = export_location)
#' # add files to global environment
#' as.data.frame(sT_export)
#' # add files to global environment, removing the project name from the file names
#' as.data.frame(sT_export, regex = "ctu05")
as.data.frame.secuTrialdata <- function(object,
                                        data.frames = NULL,
                                        meta = FALSE,
                                        regex = NULL,
                                        rep = "",
                                        envir = .GlobalEnv) {

  if (all(!is.character(regex), !is.null(regex))) stop("regex should be either NULL or character")
  if (!is.character(rep)) stop("rep should be character")
  if (!is.environment(envir)) stop("envir should be an environment")

  datanames <- object$export_options$data_names
  datanames <- as.character(datanames)
  if (!all(data.frames %in% datanames)) stop("unrecognised data.frame specified")
  if (!meta) {
    datanames <- datanames[!datanames %in% unlist(object$export_options$meta_names)]
  }
  if (!is.null(data.frames)) datanames <- datanames[datanames %in% data.frames]
  datanames2 <- datanames
  if (!is.null(regex)) datanames2 <- gsub(regex, rep, datanames)

  invisible(
    mapply(
      function(orig_name, new_name){
        assign(new_name, object[[orig_name]], envir = envir)
      },
  datanames, datanames2)
  )

}
