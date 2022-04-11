#'
#' This function loads a secuTrial export
#'
#' @description
#' This function will always load the full set of meta and data tables.
#' The export options are also loaded and written into export_options.
#'
#' @export read_secuTrial_raw
#' @importFrom stringr str_match str_length str_wrap
#' @importFrom dplyr all_equal
#' @importFrom magrittr %>%
#' @import readr
#' @importFrom grDevices rainbow
#' @importFrom graphics axis image layout legend lines par plot
#' @importFrom stats aggregate median na.omit reshape setNames
#' @importFrom utils read.table unzip
#' @name secuTrialdata
#' @rdname secuTrialdata
#' @param data_dir string The data_dir specifies the path to the secuTrial data export.
#' @return \code{secuTrialdata} object containing a list of
#'         export options and data.frames with all the data loaded from
#'         the secuTrial export.
#'         The list will contain at least the metadata data.frames and
#'         export_options list.
#'
#' @examples
#' # prepare path to example export
#' export_location <- system.file("extdata", "sT_exports", "BMD",
#'                                "s_export_CSV-xls_BMD_short_en_utf8.zip",
#'                                package = "secuTrialR")
#' # read all export data
#' sT_export <- read_secuTrial_raw(data_dir = export_location)
#'
read_secuTrial_raw <- function(data_dir) {

  # check for file existence
  if (! file.exists(data_dir)) {
    stop(paste0("There is no file '", data_dir, "'"))
  }

  # load export options
  export_options <- read_export_options(data_dir = data_dir)

  # check for language not english
  if (export_options$lang_not_supported) {
    stop("Your export language is not supported and can not be processed.")
  }

  # check if it is a rectangular export
  if (export_options$is_rectangular) {
    stop("Your export is rectangular. It can not be loaded with this function.")
  }

  # check for column names in export_options
  # if column names is not selected then the tables in the export have no header
  if (! export_options$column_names) {
    stop(paste0("The specified secuTrial export does not include 'Column names'. ",
                "Rerun your export in the ExportSearchTool with ",
                "'Column names' activated."))
  }

  # init return list
  return_list <- list(export_options = export_options)

  # load meta tables
  meta_names <- as.vector(unlist(export_options$meta_names))
  # meta table names reference for exclusion from later loading
  meta_files <- c()
  for (name in meta_names) {
    # file name
    file <- names(which(export_options$data_names == name))
    meta_files <- c(meta_files, file)
    # skip loading if file does not exist
    if (length(file) == 0) {
      next
    }
    loaded_table <- read_export_table(data_dir = data_dir,
                                      file_name = file,
                                      export_options = export_options,
                                      is_meta_table = TRUE)
    # update name
    loaded_table <- setNames(list(loaded_table), name)
    # make add_id and lab_id entry in export_options
    if (name == export_options$meta_names$casenodes) {
      col_names_casenode <- names(loaded_table[[names(loaded_table)]])
      return_list$export_options$add_id <- any(col_names_casenode == "mnpaid")
      return_list$export_options$lab_id <- any(col_names_casenode == "mnplabid")
    }
    return_list <- c(return_list, loaded_table)
  }

  # init load_list
  load_list <- names(export_options$data_names)

  # exclude meta tables since they have already been loaded
  load_list <- load_list[! load_list %in% meta_files]

  for (file in load_list) {
    # get table name from export options
    table_name <- export_options$data_names[file]
    # load table
    loaded_table <- read_export_table(data_dir = data_dir,
                                      file_name = file,
                                      # needs to be return_list$export_options
                                      # because this is updated with add-id and lab-id
                                      # export_options is not
                                      export_options = return_list$export_options,
                                      casenodes_table = return_list[[export_options$meta_names$casenodes]],
                                      centre_table = return_list[[export_options$meta_names$centres]],
                                      visitplan_table = return_list[[export_options$meta_names$visitplan]])

    # update name
    loaded_table <- setNames(list(loaded_table), table_name[[1]])
    return_list <- c(return_list, loaded_table)
  }
  class(return_list) <- "secuTrialdata"
  return(return_list)
}



#' @rdname secuTrialdata
#' @param x secuTrialdata object as returned by \code{read_secuTrial_raw}
#' @param ... further parameters
#' @return data.frame with a row for each table in the export. For each table it
#'         contains the name, number of rows and columns, an indicator for
#'         whether the table is a metadata table and the files original name.
#' @export
#'
#' @examples
#' # Print method
#' print(sT_export)
#' # or
#' sT_export

print.secuTrialdata <- function(x, ...) {

  cat("secuTrial data imported from:\n")
  cat(str_wrap(x$export_options$data_dir, width = 80), "\n")

  tab <- lapply(x$export_options$data_names, function(y) {
    tmp <- x[[y]]
    tmp
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
