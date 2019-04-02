#'
#' This function loads a secuTrial export
#'
#' @description
#' This function will always load the full set of meta tables first.
#' Further tables to load can be specified. The standard is to load
#' the entire export. The export options are also loaded and written
#' into export_options.
#'
#' @export load_secuTrial_export
#' @name secuTrialdata
#' @rdname secuTrialdata
#' @param data_dir string The data_dir specifies the path to the secuTrial data export.
#' @param add_id_name string This needs to be specified if your Add-ID name has
#'                    been changed in the AdminTool Design setting.
#' @param tables vector containing the names of the files to be loaded. Default
#'                      behavior load all tables. "none" can also be
#'                      specified if only meta tables and export_options
#'                      shall be loaded.
#'
#' @return This is a list of class secuTrialdata containing a list of
#'         export options and data.frames with all the data loaded from
#'         the secuTrial export.
#'         The list will contain at least the metadata data.frames and
#'         export_options list.
#'
#' @examples
#' # prepare path to example export
#' export_location <- system.file("extdata", "s_export_CSV-xls_BMD.zip",
#'                                package = "secuTrialR")
#' # load all export data
#' sT_export <- load_secuTrial_export(data_dir = export_location)
#'
load_secuTrial_export <- function(data_dir, tables = "all", add_id_name = NULL) {

  # load export options
  export_options <- load_export_options(data_dir = data_dir, add_id_name = add_id_name)

  # check for language not english
  if (export_options$lang_not_en) {
    stop("Your export language is not English and can not be processed.")
  }

  # check if it is a rectangular export
  if (export_options$is_rectangular) {
    stop("Your export is rectangular. It can not be loaded with this function.")
  }

  # check for column names in export_options
  if (! export_options$column_names) {
    stop(paste0("The specified secuTrial export does not include 'Column names'. ",
                "Rerun your export in the ExportSearchTool with ",
                "'Column names' activated."))
  }

  # check if add_id is available
  if (! export_options$add_id) {
    stop(paste0("The specified secuTrial export does not include an Add-ID. ",
                "Maybe you can specifty a custom add_id_name which has been ",
                "set in the AdminTool 'Design' setting?"
                ))
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
    loaded_table <- load_export_table(data_dir = data_dir,
                                      file_name = file,
                                      export_options = export_options,
                                      is_meta_table = TRUE)
    # update name
    loaded_table <- setNames(list(loaded_table), name)
    return_list <- c(return_list, loaded_table)
  }

  # check if tables is "none" then stop here
  if (tables == "none") {
    return(return_list)
  }

  # load all other specified tables
  if (all(tables == "all")) {
    load_list <- names(export_options$data_names)
  # if a custom list is supplied
  } else {
    load_list <- tables
  }

  # exclude meta tables since they have already been loaded
  load_list <- load_list[! load_list %in% meta_files]

  for (file in load_list) {
    # get table name from export options
    table_name <- export_options$data_names[file]
    # load table
    if (! export_options$short_names) {
      loaded_table <- load_export_table(data_dir = data_dir,
                                        file_name = file,
                                        export_options = export_options,
                                        casenodes_table = return_list$casenodes,
                                        centre_table = return_list$centres,
                                        visitplan_table = return_list$visitplan)
    } else {
      loaded_table <- load_export_table(data_dir = data_dir,
                                        file_name = file,
                                        export_options = export_options,
                                        casenodes_table = return_list$cn,
                                        centre_table = return_list$ctr,
                                        visitplan_table = return_list$vp)
    }
    # update name
    loaded_table <- setNames(list(loaded_table), table_name[[1]])
    return_list <- c(return_list, loaded_table)
  }
  class(return_list) <- "secuTrialdata"
  return(return_list)
}
