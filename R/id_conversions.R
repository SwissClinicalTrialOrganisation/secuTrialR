# collection of smaller functions for ID conversions

#' Adds a mapped pat.id column to a table given the patient table
#'
#' @param table data.frame A data.frame with a column mnppid to be translated to pat.id (mnpaid).
#' @param id string This specifies the name of the new id column.
#' @param patient_table data.frame This is the loaded reference table to make the translation (i.e. casenodes/cn in export archive).
#'
#' @return Returns a data.frame with a new first column containing the translated ids.
#'
#' @examples
#' \donttest{
#' bmd_with_patid <- add_pat_id_col(table = bmd,
#'                                  id = "pat.id",
#'                                  patient_table = patient)
#' }
#'
add_pat_id_col <- function(table, id = "pat.id", patient_table = patient) {
  # check for mnppid in table
  if ("mnppid" %in% names(table)) {
    table[[id]] <- mnppid2mnpaid(table$mnppid, patient_table)
    table <- .move_column_after(table, id, "first")
    return(table)
  } else {
    stop("Error: There is no mnppid column in your input table.")
  }
}

#' Find the mnpaid related to an mnppid
#'
#' @param mnppid The mnppid for which the mnpaid should be retrieved.
#' @param patient_table data.frame This is the loaded reference table to make the translation (i.e. casenodes/cn in export archive).
#'
#' @return mnpaid related to the entered mnppid.
#'
#' @examples
#' \donttest{
#' mnppid2mnpaid(1780)
#' [1] 103
#' }
#'
mnppid2mnpaid <- function(mnppid, patient_table = patient) {
  patient_table$mnpaid[match(mnppid, patient_table$mnppid)]
}

#' Adds a mapped centre column to a table given the patient and centre tables
#'
#' @param table data.frame A data.frame with a column mnppid.
#' @param id string This specifies the name of the new centre column.
#' @param remove_ctag boolean If this is TRUE it will remove trailing brackets and their contents from the centre name.
#' @param patient_table data.frame This is the loaded reference table to make the translation (i.e. casenodes/cn in export archive).
#' @param centre_table data.frame This is the loaded reference table to make the translation (i.e. centres/ctr in export archive).
#'
#' @return Returns a data.frame with a new column containing the centres.
#'
#' @examples
#' \donttest{
#' bmd_with_centre <- add_centre_col(table = bmd,
#'                                   patient_table = patient,
#'                                   centre_table = centre)
#' }
#'
add_centre_col <- function(table, id = "centre", remove_ctag = FALSE, patient_table = patient, centre_table = centre) {
  table[[id]] <- mnppid2centre(mnppid = table$mnppid,
                               remove_ctag = remove_ctag,
                               patient_table = patient_table,
                               centre_table = centre_table)
  if ("pat.id" %in% names(table)) {
    table <- .move_column_after(table, id, "pat.id")
  } else {
    table <- .move_column_after(table, id, "mnppid")
  }
  return(table)
}

#' Find the centre related to an mnppid
#'
#' @param mnppid The mnppid for which the centre should be retrieved.
#' @param remove_ctag boolean If this is TRUE it will remove trailing brackets and their contents from the centre name.
#' @param patient_table data.frame This is the loaded reference table to make the translation (i.e. casenodes/cn in export archive).
#' @param centre_table data.frame This is the loaded reference table to make the translation (i.e. centres/ctr in export archive).
#'
#' @return centre related to the entered mnppid.
#'
#' @examples
#' \donttest{
#' mnppid2centre(1780)
#' [1] Hospital (BMD)
#' Levels: Hospital (BMD)
#' }
#'
mnppid2centre <- function(mnppid, remove_ctag = FALSE, patient_table = patient, centre_table = centre) {
  # convert mnppid to mnpctrid
  mnpctrid <- patient_table$mnpctrid[match(mnppid, patient_table$mnppid)]
  if (remove_ctag == TRUE) {
    # convert centre id to centre and remove centre tag
    as.factor(unlist(lapply(centre_table$mnpctrname[match(mnpctrid, centre_table$mnpctrid)],
                            remove_trailing_bracket)))
  } else {
    as.factor(unlist(centre_table$mnpctrname[match(mnpctrid, centre_table$mnpctrid)]))
  }
}

#' Removes a trailing bracket and its contents from a string
#'
#' @param x string
#'
#' @return Returns a string without the trailing brackets.
#'
#' @examples
#' \donttest{
#' remove_trailing_bracket("Hospital (Test Study)")
#' }
#'
remove_trailing_bracket <- function(x) {
  x <- as.character(x)
  x <- gsub(pattern = "\\(.*\\)$", replacement = "", x)
  x <- trimws(x)
  return(x)
}
