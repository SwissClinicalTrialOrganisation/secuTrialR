# collection of smaller functions for ID conversions

# Find the mnpvislabel related to an mnpvisid
#
# @param mnpvisid The mnpvisid for which the mnpvislabel should be retrieved.
# @param visitplan_table data.frame This is the loaded reference table to make
#        the translation (i.e. visitplan/vp in export archive).
#
# @return mnpvislabel related to the entered mnpvisid.
#
# @examples
# \donttest{
# nolint start
# mnpvisid2mnpvislabel(mnpvisid = 13920, visitplan_table = visitplan)
# nolint end
# [1] "#. visit"
# }
mnpvisid2mnpvislabel <- function(mnpvisid, visitplan_table) {
  as.character(visitplan_table$mnpvislabel[match(mnpvisid, visitplan_table$mnpvisid)])
}

# Adds a mapped visit_name column to a table given the visitplan table
#
# @param table data.frame with a column mnpvisid to be translated to visit names.
# @param id string This specifies the name of the new id column.
# @param visitplan_table data.frame This is the loaded reference table
#        to make the translation (i.e. visitplan/vp in export archive).
#
# @return Returns a data.frame with a new column containing the translated visit names.
# @examples
# \donttest{
# bmd_with_visit_name <- add_pat_id_col(table = bmd,
#                                       id = "visit_name",
#                                       visitplan_table = visitplan)
# }
#
add_visitname_col <- function(table, id = "visit_name", visitplan_table) {
  # check for mnpvisid in table
  if ("mnpvisid" %in% names(table)) {
    table[[id]] <- mnpvisid2mnpvislabel(table$mnpvisid, visitplan_table)
  } else {
    stop("There is no mnpvisid column in your input table.")
  }
  if ("pat_id" %in% names(table)) {
    table <- .move_column_after(table, id, "pat_id")
  }
  return(table)
}

# Adds a mapped pat_id column to a table given the casenodes table
#
# @param table data.frame with a column mnppid to be translated to pat_id (mnpaid).
# @param id string This specifies the name of the new id column.
# @param casenodes_table data.frame This is the loaded reference table to make
#                        the translation (i.e. casenodes/cn in export archive).
#
# @return Returns a data.frame with a new first column containing the translated ids.
#
# @examples
# \donttest{
# bmd_with_patid <- add_pat_id_col(table = bmd,
#                                  id = "pat_id",
#                                  casenodes_table = casenodes)
# }
#
add_pat_id_col <- function(table, id = "pat_id", casenodes_table) {
  # check for mnppid in table
  if ("mnppid" %in% names(table)) {
    table[[id]] <- mnppid2mnpaid(table$mnppid, casenodes_table)
    table <- .move_column_after(table, id, "first")
    return(table)
  } else {
    stop("There is no mnppid column in your input table.")
  }
}

# Find the mnpaid related to an mnppid
#
# @param mnppid The mnppid for which the mnpaid should be retrieved.
# @param casenodes_table data.frame This is the loaded reference table
#                        to make the translation (i.e. casenodes/cn in export archive).
#
# @return mnpaid related to the entered mnppid.
#
# @examples
# \donttest{
# nolint start
# mnppid2mnpaid(mnppid = 1780, casenodes_table = casenodes)
# nolint end
# [1] 103
# }
#
mnppid2mnpaid <- function(mnppid, casenodes_table) {
  casenodes_table$mnpaid[match(mnppid, casenodes_table$mnppid)]
}

# Adds a mapped centre column to a table given the casenodes and centre tables
#
# @param table data.frame A data.frame with a column mnppid.
# @param id string This specifies the name of the new centre column.
# @param remove_ctag boolean If this is TRUE it will remove trailing brackets and
#                    their contents from the centre name.
# @param casenodes_table data.frame This is the loaded reference table to make the
#                        translation (i.e. casenodes/cn in export archive).
# @param centre_table data.frame This is the loaded reference table to make the
#                     translation (i.e. centres/ctr in export archive).
#
# @return Returns a data.frame with a new column containing the centres.
#
# @examples
# \donttest{
# bmd_with_centre <- add_centre_col(table = bmd,
#                                   casenodes_table = casenodes,
#                                   centre_table = centre)
# }
#
add_centre_col <- function(table, id = "centre", remove_ctag = FALSE, casenodes_table, centre_table) {
  table[[id]] <- mnppid2centre(mnppid = table$mnppid,
                               remove_ctag = remove_ctag,
                               casenodes_table = casenodes_table,
                               centre_table = centre_table)
  if ("pat_id" %in% names(table)) {
    table <- .move_column_after(table, id, "pat_id")
  } else {
    table <- .move_column_after(table, id, "mnppid")
  }
  return(table)
}

# Find the centre related to an mnppid
#
# @param mnppid The mnppid for which the centre should be retrieved.
# @param remove_ctag boolean If this is TRUE it will remove trailing brackets and their
#                    contents from the centre name.
# @param casenodes_table data.frame This is the loaded reference table to make the
#                        translation (i.e. casenodes/cn in export archive).
# @param centre_table data.frame This is the loaded reference table to make the
#                     translation (i.e. centres/ctr in export archive).
#
# @return centre related to the entered mnppid.
#
# @examples
# \donttest{
# nolint start
# mnppid2centre(mnppid = 1780, casenodes_table = casenodes, centre_table = centre)
# nolint end
# [1] Hospital (BMD)
# Levels: Hospital (BMD)
# }
#
mnppid2centre <- function(mnppid, remove_ctag = FALSE, casenodes_table, centre_table) {
  # convert mnppid to mnpctrid
  mnpctrid <- casenodes_table$mnpctrid[match(mnppid, casenodes_table$mnppid)]
  if (remove_ctag == TRUE) {
    # convert centre id to centre and remove centre tag
    as.factor(unlist(lapply(centre_table$mnpctrname[match(mnpctrid, centre_table$mnpctrid)],
                            remove_trailing_bracket)))
  } else {
    as.factor(unlist(centre_table$mnpctrname[match(mnpctrid, centre_table$mnpctrid)]))
  }
}

# Removes a trailing bracket and its contents from a string
#
# @param x string
#
# @return Returns a string without the trailing brackets.
#
# @examples
# \donttest{
# nolint start
# remove_trailing_bracket("Hospital (Test Study)")
# nolint end
# }
#
remove_trailing_bracket <- function(x) {
  x <- as.character(x)
  x <- gsub(pattern = "\\(.*\\)$", replacement = "", x)
  x <- trimws(x)
  return(x)
}
