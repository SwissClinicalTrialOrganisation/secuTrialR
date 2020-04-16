#' as.data.frame method for secuTrialdata objects
#' Make the data from the exports more easily accessible by placing them in
#' another environment (e.g. place them in the global environment
#' (\code{.GlobalEnv}) and you can reference them without referring to the
#' \code{secuTrialdata} object anymore. Ie. they become regular \code{data.frame}s).
#' @param x \code{secuTrialdata} object
#' @param envir environment in which to put the data (e.g. \code{.GlobalEnv})
#' @param data.frames character vector of data.frame names to turn into data.frames
#' @param meta logical should metadata be returned
#' @param regex regex syntax to remove from names
#' @param rep replacement for regex
#' @param ... further parameters
#' @details \code{envir} must be specifically defined. For simplicity,
#' \code{.GlobalEnv} would probably be the easiest (assigning it to another
#' environment would still entail referring to that environment).
#' @return each \code{data.frame} in the \code{secuTrialdata} object is saved to it's
#' own \code{data.frame} in the designated environment
#' @export
#'
#' @examples
#' # prepare path to example export
#' export_location <- system.file("extdata", "sT_exports", "lnames",
#'                                "s_export_CSV-xls_CTU05_long_ref_miss_en_utf8.zip",
#'                                package = "secuTrialR")
#' # load all export data
#' sT_export <- read_secuTrial_raw(data_dir = export_location)
#' # add files to a new environment called env1
#' env1 <- new.env()
#' as.data.frame(sT_export, envir = env1)
#' # add files to a new environment called env2, removing the project name from
#' # the file names
#' env2 <- new.env()
#' as.data.frame(sT_export, regex = "ctu05", envir = env2)
as.data.frame.secuTrialdata <- function(x,
                                        ...,
                                        envir,
                                        data.frames = NULL,
                                        meta = FALSE,
                                        regex = NULL,
                                        rep = ""
                                        ) {

  if (all(!is.character(regex), !is.null(regex))) stop("regex should be either NULL or character")
  if (!is.character(rep)) stop("rep should be character")
  if (!is.environment(envir)) stop("envir should be an environment")

  datanames <- x$export_options$data_names
  datanames <- as.character(datanames)
  if (!all(data.frames %in% datanames)) stop("unrecognised data.frame specified")
  if (!meta) {
    datanames <- datanames[!datanames %in% unlist(x$export_options$meta_names)]
  }
  if (!is.null(data.frames)) datanames <- datanames[datanames %in% data.frames]
  datanames2 <- datanames
  if (!is.null(regex)) datanames2 <- gsub(regex, rep, datanames)

  invisible(
    mapply(
      function(orig_name, new_name) {
        assign(new_name, x[[orig_name]], envir = envir)
      },
      datanames, datanames2)
  )

}
