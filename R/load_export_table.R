#' Load an individual file/table from a secuTrial export archive
#'
#' @description
#' This function can load individual tables from a secuTrial data
#' export. Before loading any other tables the casenodes/cn
#' and centres/ctr tables need to be loaded with this function.
#' Also export_options needs to be generated with load_export_options.
#'
#' @param data_dir string The data_dir specifies the path to the secuTrial data export.
#' @param file_name string The file_name specifies which file to load.
#' @param export_options list This can be generated with load_export_options.
#' @param add_pat_id boolean If TRUE this will add the pat.id to the table.
#' @param add_centre boolean If TRUE this will add the centre name to the table.
#' @param patient_table data.frame This data.frame must be created with this function
#'                      before any other secuTrial tables are read (exception centre_table).
#'                      While creating it this argument is obsolete. (see examples)
#' @param centre_table data.frame his data.frame must be created with this function
#'                      before any other secuTrial tables are read (exception patient_table).
#'                      While creating it this argument is obsolete. (see examples)
#'
#' @return The function returns a data.frame for the data in file_name.
#'
#' @seealso load_export_options
#'
#' @examples
#' \donttest{
#' # load patient (casenodes/cn) table
#' patient <- load_export_table(data_dir = system.file("extdata",
#'                                                     "s_export_CSV-xls_BMD.zip",
#'                                                     package = "secuTrialR"),
#'                              file_name = "cn.xls",
#'                              export_options=export_options)
#'
#' # load centre (centres/ctr) table
#' centre <- load_export_table(data_dir = system.file("extdata",
#'                                                    "s_export_CSV-xls_BMD.zip",
#'                                                    package = "secuTrialR"),
#'                             file_name = "ctr.xls",
#'                             export_options=export_options)
#'
#' # load bone mineral denisty form data
#' bmd <- load_export_table(data_dir = system.file("extdata",
#'                                                 "s_export_CSV-xls_BMD.zip",
#'                                                 package = "secuTrialR"),
#'                          file_name = "bmd.xls",
#'                          export_options = export_options,
#'                          patient_table = patient,
#'                          centre_table = centre)
#' }
#'
load_export_table <- function(data_dir, file_name, export_options, add_pat_id=TRUE, add_centre=TRUE,
                              patient_table = patient, centre_table = centre) {

  # confirm export_options presence
  if (!exists("export_options")) stop("export_options have not been specified. Run load_export_options to create.")

  if (export_options$is_zip) {
    archive_con <- unz(data_dir, file_name)
    loaded_table <- read.table(file = archive_con,
                               header = TRUE,
                               na.strings = export_options$na.strings,
                               sep = export_options$sep,
                               fill = TRUE)
  } else if (export_options$is_zip == FALSE) {
    loaded_table <- read.table(file = paste0(data_dir, "/", file_name),
                               header = TRUE,
                               na.strings = export_options$na.strings,
                               sep = export_options$sep,
                               fill = TRUE)
  } else {
    stop(paste0("Could not load table ", file_name))
  }

  # in earlier secuTrial exports there was
  # a last/empty column "X" which can be removed
  if ("X" %in% names(loaded_table)) {
    loaded_table <- loaded_table[, -ncol(loaded_table)]
  }

  # adding pat.id
  if (add_pat_id & ("mnppid" %in% names(loaded_table))) {
    # In order to be able to translate mnppid to mnpaid the casenode (patient) table is required.
    # This table should be loaded first to enable the translations of the other tables.
    # If not already loaded there will be an error since the "patient"=casenode table is missing.
    # The casenode (patient) table or any other table that already has an mnpaid must not invoke add_pat_id_col().
    if (! "mnpaid" %in% names(loaded_table)) {
      loaded_table <- add_pat_id_col(table = loaded_table,
                                     id = "pat.id",
                                     patient_table = patient_table)
    # this will happen for the casenodes/cn table
    } else {
      loaded_table$pat.id <- loaded_table$mnpaid
      loaded_table <- .move_column_after(loaded_table, "pat.id", "first")
    }
  }

  # adding centres
  if (add_centre & "mnppid" %in% names(loaded_table))  {
    if (! ("mnpaid" %in% names(loaded_table))) {
      loaded_table <- add_centre_col(table = loaded_table, id = "centre",
                                     remove_ctag = FALSE, patient_table = patient_table,
                                     centre_table = centre_table)
    # this will happen for the centres/ctr table
    } else if ("mnpctrname" %in% names(loaded_table))  {
      # Since the introduction of the flag "Duplicate form meta data into all tables"
      # The centre-metadate-id is missing in some tables
      stopifnot("mnpctrname" %in% names(loaded_table))
      loaded_table$centre <- as.factor(loaded_table$mnpctrname)
      loaded_table <- .move_column_after(loaded_table, "centre", "pat.id")
      loaded_table$centre <- as.factor(unlist(lapply(loaded_table$centre, remove_trailing_bracket)))
    }
  }
  return(loaded_table)
}
