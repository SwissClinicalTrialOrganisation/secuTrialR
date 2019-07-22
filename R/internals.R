# internal helper functions

# Returns internal dictionary file
#
# @return data.frame containing a secuTrialR dictionary
#
.get_dict <- function(file, language = NULL){
  dict_file <- system.file("extdata", "dictionaries",
                           file,
                           package = "secuTrialR")
  dict <- read.csv(dict_file, stringsAsFactors = FALSE)
  if (!is.null(language) & any(grep("lang", names(dict)))){
    dict <- dict[dict$lang == language, ]
  }
  return(dict)
}

# construct names of metadata
.construct_metaname <- function(x, meta_names, file_tag, file_extension) {
  paste0(meta_names[x],
         file_tag,
         ".",
         file_extension)
}

# remove project name (mnpXYZ_) from string
.removeproj <- function(x) {
  x <- gsub("mnp[[:alnum:]]{1,}_", "", x)
  x <- gsub("^_", "", x)
  x
}

# convert names (used in write_secutrial)
convertnames <- function(df, format){
  name <- names(df)
  # if (format %in% c("dta", "sav")){
  name <- gsub("\\.datetime", "_dt", name)
  name <- gsub("\\.date", "_d", name)
  name <- gsub("\\.factor", "_f", name)
  # }
  names(df) <- name
  return(df)
}

# This function moves a given column (defined by the column index)
# to a specific position in the data frame.
.move_column_to_pos <- function(df, col_idx, new_col_idx) {
  # checks
  # check for data.frame
  if (! is.data.frame(df)) {
    stop("Error: Passed object is not a data.frame in .move_column_to_pos")
  }
  # check for integer
  if (! ( (col_idx %% 1) == 0)) {
    stop(paste0("Error: col_idx ", col_idx, " is not an integer."))
  }
  # check for integer
  if (! (new_col_idx %% 1) == 0) {
    stop(paste0("Error: new_col_idx ", new_col_idx, " is not an integer."))
  }
  # check out of bounds
  if (! col_idx <= ncol(df)) {
    stop("Error: col_idx is out of data.frame bounds.")
  }
  # check out of bounds
  if (! new_col_idx <= ncol(df)) {
    stop("Error: new_col_idx is out of data.frame bounds.")
  }
  # move to front
  if (new_col_idx == 1) {
    df <- df[, c(col_idx, (1:ncol(df))[-col_idx])]
  }
  # move to end
  else if (new_col_idx == ncol(df)) {
    df <- df[, c( (1:ncol(df))[-col_idx], col_idx)]
  } else {
    # move forward
    if (new_col_idx < col_idx) {
      df <- df[, c(1:new_col_idx - 1, col_idx, (new_col_idx:ncol(df))[(new_col_idx:ncol(df)) != col_idx])]
      # move backwards
    } else {
      df <- df[, c( (1:new_col_idx)[-col_idx], col_idx, ( (new_col_idx + 1):ncol(df)))]
    }
  }
  return(df)
}

# This function moves a given column (col_name) or a vector of columns behind a specific
# position (col_name_after) in the data frame. Columns are specified by their names.
.move_column_after <- function(df, col_name, col_name_after) {
  if (is.data.frame(df)) {
    if (! (col_name_after %in% names(df)) & col_name_after != "first") {
       stop(paste("Error: Unknown column", col_name_after))
    }
    if (! is.na(match(col_name_after, col_name))) {
      stop("Error: Reference column cannot be moved.")
    }
    # iterate from last to first to keep order
    range <- 1:length(col_name)
    for (i in range) {
      # index of col_name
      col_idx <- match(col_name[i], names(df))
      if (is.na(col_idx)) {
        stop(paste("Unknown column", col_name[i]))
      }
      # index to move (one after col_name_after)
      new_col_idx <- 1 + (i - 1)
      ref_col_idx <- match(col_name_after, names(df)) + (i - 1)
      if (col_name_after != "first") {
        # last term handles artefact in move.column.to.pos that reference
        # is move in different diretions depending on whether col comes from
        # position   before or after ref
        new_col_idx <- ref_col_idx + as.numeric(col_idx > ref_col_idx)
      }
      if (!is.null(label(df))){
        lab <- label(df)
        labl <- TRUE
      } else {
        labl <- FALSE
      }
      df <- .move_column_to_pos(df, col_idx, new_col_idx)
      if (labl) label(df) <- lab
    }
    return(df)
  } else {
    stop("Error: Passed object is not a data.frame in .move_column_after")
  }
}


# Determines the language of secuTrial export
#
# @param parsed_export - string containing parsed ExportOptions file
# @return string containing iso 639-1 language code of the secuTrial export, or "unknown" if the language was not recognised.
#
.get_export_language <- function(parsed_export){
  # read dictionary of export options
  dict <- .get_dict("dict_export_options_keys.csv")
  # need to omit the "patients" column since this can be manually changed in the
  # AdminTool for some projects and the language will then be wrongly set to "unknown"
  # "email" must also be removed since it is not available for every export
  dict <- subset(dict, select = -c(patients, email))
  # determine export language
  is_de <- all(sapply(dict[which(dict$lang == "de"), ], function(x) any(grepl(x, parsed_export))))
  is_en <- all(sapply(dict[which(dict$lang == "en"), ], function(x) any(grepl(x, parsed_export))))
  is_it <- all(sapply(dict[which(dict$lang == "it"), ], function(x) any(grepl(x, parsed_export))))
  is_fr <- all(sapply(dict[which(dict$lang == "fr"), ], function(x) any(grepl(x, parsed_export))))
  is_pl <- all(sapply(dict[which(dict$lang == "pl"), ], function(x) any(grepl(x, parsed_export))))
  is_es <- all(sapply(dict[which(dict$lang == "es"), ], function(x) any(grepl(x, parsed_export))))
  # if export language is none of the known languages, return "unknown"
  if (sum(is_de, is_en, is_it, is_fr, is_pl, is_es, na.rm = TRUE) != 1){
    lang <- "unknown"
    return(lang)
  }
  # write export language
  if (is_en){
    lang <- "en"
  } else if (is_de){
    lang <- "de"
  } else if (is_it){
    lang <- "it"
  } else if (is_fr){
    lang <- "fr"
  } else if (is_pl){
    lang <- "pl"
  } else if (is_es){
    lang <- "es"
  } else {
    lang <- "unknown"
  }
  return(lang)
}
