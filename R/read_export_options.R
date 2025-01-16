# Load secuTrial data export options
#
# @description
# The read_export_options reads the secuTrial data export and
# determines specific options needed to handle the data properly.
#
# This is an internal function which is wrapped by read_secuTrial_raw
#
# @param data_dir string The data_dir specifies the path to the secuTrial data export.
#
# @return The function returns a list with the data export options.
#
# @examples
# \donttest{
# read_export_options(data_dir = system.file("extdata", "sT_exports", "BMD",
#                                            "s_export_CSV-xls_BMD_short_en_utf8.zip",
#                                            package = "secuTrialR"))
# }
#
read_export_options <- function(data_dir) {

  is_zip <- grepl(".zip$", data_dir)

  # load ExportOptions source
  if (is_zip) {
    files <- unzip(data_dir, list = TRUE)
    study_options_file_idx <- grep("ExportOptions", files$Name)
    if(length(study_options_file_idx) != 1) stop("Exports should contain a single ExportOptions html file")
    file_con <- unz(data_dir, files$Name[study_options_file_idx])
    parsed_export <- readLines(file_con)
    close(file_con)
  } else {
    files <- data.frame(Name = list.files(data_dir))
    files$Name <- as.character(files$Name)
    study_options_file_idx <- grep("ExportOptions", files$Name)
    if(length(study_options_file_idx) != 1) stop("Exports should contain a single ExportOptions html file")
    parsed_export <- readLines(file.path(data_dir, files$Name[study_options_file_idx]))
  }

  # recode if there is ISO encoding
  if (length(grep("ISO-8859-", parsed_export[1:10]))) {
    Encoding(parsed_export) <- "latin1"
  }

  # utf16 exception
  if (! length(parsed_export)) {
    stop("Likely you have exported with UTF-16 encoding which is not compatible. Please reexport with UTF-8.")
  }

  # dictionaries for metadata keys and selected export settings
  dict_keys <- .get_dict("dict_export_options_keys.csv")
  dict_settings <- .get_dict("dict_export_options_settings.csv")
  # version reference is on the bottom of the page
  version_line <- parsed_export[max(grep("secuTrial", parsed_export))]
  version <- unlist(regmatches(version_line,
                               gregexpr(pattern = "[[:digit:]]+\\.[[:digit:]]+\\.[[:digit:]]+\\.[[:digit:]]+",
                                        text = version_line)
                               )
                    )
  # Project version
  pversion_line <- parsed_export[max(grep(paste(dict_keys[, "version"], collapse = "|"), parsed_export)) + 2]
  pversion <- str_match(pversion_line, pattern = "<b>(.+)</b>")[, 2]

  # Project name
  pname_line <- parsed_export[grep(paste(dict_keys[, "project"], collapse = "|"), parsed_export) + 2]
  pname <- str_match(pname_line, pattern = "<b>(.+)</b>")[, 2]

  # Format e.g. "CSV format for MS Excel"
  format_line <- parsed_export[grep(paste(dict_keys[, "format"], collapse = "|"), parsed_export) + 2]
  format_info <- str_match(format_line, pattern = "<b>(.+)</b>")[, 2]

  # check format info // only the two CSV formats are compatible
  # grep instead of exact match because there are language differences in the string
  if (! grepl("CSV", format_info)) {
    stop(paste0("Your export must be exported as CSV format for MS Excel or CSV format. ",
                "It currently is: ", format_info))
  }

  # short names
  short_names <- any(sapply(dict_settings[, "shortnames"], function(x) any(grepl(x, parsed_export))))

  # rectangular data
  rectangular_table <- any(sapply(dict_settings[, "rectangulartable"], function(x) any(grepl(x, parsed_export))))

  # audit trail
  audit_trail <- any(sapply(dict_settings[, "audittrail"], function(x) any(grepl(x, parsed_export))))

  # language of the export (2-letter code)
  lang <- .get_export_language(parsed_export)
  # determine if the languages is one of languages supported by secuTrialR
  lang_not_supported <- !lang %in% c("en", "de", "fr", "it", "es", "pl")

  # items dictionary
  dict_items <- .get_dict("dict_items_table.csv", lang)

  # Column names
  column_names <- any(sapply(dict_settings[, "columnnames"], function(x) any(grepl(x, parsed_export))))

  # Duplicate form meta data into all tables
  duplicate_meta <- any(sapply(dict_settings[, "duplicatemeta"], function(x) any(grepl(x, parsed_export))))

  # Form status
  form_status <- any(sapply(dict_settings[, "formstatus"], function(x) any(grepl(x, parsed_export))))

  # Project setup
  proj_setup <- any(sapply(dict_settings[, "projsetup"], function(x) any(grepl(x, parsed_export))))

  # centre information
  centre_info <- any(sapply(dict_settings[, "centreinfo"], function(x) any(grepl(x, parsed_export))))

  # hidden field data included
  hidden_fields <- any(sapply(dict_settings[, "hiddenfields"], function(x) any(grepl(x, parsed_export))))

  # Form meta data: Structure data included
  structure <- any(sapply(dict_settings[, "structure"], function(x) any(grepl(x, parsed_export))))

  # metadata file names
  meta_names <- list()
  if (short_names == TRUE) {
    meta_names$forms <- "fs"
    meta_names$casenodes <- "cn"
    meta_names$centres <- "ctr"
    meta_names$items <- "is"
    meta_names$questions <- "qs"
    meta_names$queries <- "qac"
    meta_names$visitplan <- "vp"
    meta_names$visitplanforms <- "vpfs"
    meta_names$atcasenodes <- "atcn"
    meta_names$atcasevisitplans <- "atcvp"
    meta_names$comments <- "cts"
  } else if (short_names == FALSE) {
    meta_names$forms <- "forms"
    meta_names$casenodes <- "casenodes"
    meta_names$centres <- "centres"
    meta_names$items <- "items"
    meta_names$questions <- "questions"
    meta_names$queries <- "queries"
    meta_names$visitplan <- "visitplan"
    meta_names$visitplanforms <- "visitplanforms"
    meta_names$atcasenodes <- "atcasenodes"
    meta_names$atcasevisitplans <- "atcasevisitplans"
    meta_names$comments <- "comments"
  }
  # no difference long/short
  meta_names$miv <- "miv"
  meta_names$cl <- "cl"

  # retrieve project specific file tags
  if (rectangular_table & short_names) {
    meta_name_patterns <- paste("^", meta_names, collapse = "|", sep = "")
    first_file <- files$Name[grepl(pattern = meta_name_patterns, files$Name)][1]
    first_file_trunc <- gsub(".xls", "", first_file)
    file_tag <- gsub(meta_name_patterns, "", first_file_trunc)
  } else {
    file_tag <- gsub(pattern = paste0("ExportOptions|(_",lang,"|).html"),
                     replacement = "",
                     files$Name[study_options_file_idx])
  }

  # get data file extension
  file_extension <- unique(sapply(strsplit(files$Name[-study_options_file_idx], ".", fixed = TRUE), function(x) x[2]))

  meta_available <- list()
  for (entry in c("forms", "casenodes", "centres", "items", "questions", "queries", "visitplan",
                  "visitplanforms", "atcasenodes", "atcasevisitplans", "comments", "miv", "cl")) {
    meta_available[entry] <- .construct_metaname(entry, meta_names, file_tag, file_extension) %in% files$Name
  }

  # find form data separator and enclosure ----
  if (is_zip) {
    file_con <- unz(data_dir, files$Name[!grepl("html$", files$Name)][1])
    header <- readLines(file_con, 1)
    close(file_con)
  } else if (!is_zip) {
    header <- readLines(file.path(data_dir, files$Name[!grepl("html$", files$Name)][1]), 1)
  }
  quote <- substr(header, 1, 1)
  header <- gsub(quote, "" , header)
  if (grepl(",", header)) {
    sep <- ","
  } else if (grepl("'", header)) {
    sep <- "'"
  } else if (grepl(";", header)) {
    sep <- ";"
  } else if (grepl("\\t", header)) {
    sep <- "\t"
  } else if (grepl("@", header)) {
    sep <- "@"
  } else if (grepl("\"", header)) {
    sep <- "\""
  } else {
    stop("Error: Field separator could not be retrieved.")
    return(NULL)
  }

  # NA strings
  na.strings <- c("NA", "")
  # TODO : custom formats? parsed from ExportOptions?

  # reference values
  refvals_seperate <- any(sapply(dict_settings[, "separatetable"], function(x) any(grepl(x, parsed_export))))

  # this should only match once
  created_idx <- unique(unlist(sapply(dict_keys[, "created"], function(x) grep(x, parsed_export))))
  # should never happen
  if (length(created_idx) > 1) {
    warning("Export creation date parsing matching unexpectedly more than once and may thus be wrong.")
  }
  # time of export is two lines below the Created pattern
  time_of_export <- parsed_export[created_idx + 2] %>%
    trimws() %>%
    gsub(pattern = "^<b>|</b>$", replacement = "")

  # dates ----
  # date format
  date_format_meta <- "%Y-%m-%d"
  date_format <- "%Y%m%d"
  datetime_format <- "%Y%m%d%H%M"

  # filenames
  datafiles <- files$Name[!grepl(".html$", files$Name)]

  datanames <- gsub(file_tag, "", datafiles)
  datanames <- gsub(paste0("\\.", file_extension), "", datanames)
  # long names
  datanames <- gsub(pattern = "^mnp", "", datanames)
  names(datanames) <- datafiles

  # retrieve export encoding
  # Note: We are aware that this may match if someone enters a matching expression
  #       in the Description

  # it is important to check for UTF-8 + BOM first
  # since UTF-8 will also match UTF-8 + BOM
  if (length(grep(">UTF-8 \\+ BOM", parsed_export))) {
    encoding <- "UTF-8 + BOM"
  } else if (length(grep("UTF-8", parsed_export))) {
    encoding <- "UTF-8"
  # it is important to check for ISO-8859-15 first
  # since ISO-8859-1 will also match ISO-8859-15
  } else if (length(grep(">ISO-8859-15", parsed_export))) {
    encoding <- "ISO-8859-15"
  } else if (length(grep(">ISO-8859-1", parsed_export))) {
    encoding <- "ISO-8859-1"
  } else if (length(grep(">MacRoman", parsed_export))) {
    encoding <- "MacRoman"
  } else if (length(grep(">UTF-16", parsed_export))) {
    encoding <- "UTF-16"
  } else {
    stop("Unexpectedly not found a character encoding in the ExportOptions.")
  }

  if (encoding == "MacRoman") {
    stop("Your export has been produced with MacRoman encoding. Please reexport with UTF-8.")
  }
  # this is because the unz() function can not make a propper connection to BOM encoding
  if (encoding == "UTF-8 + BOM") {
    stop("Your export has been produced with UTF-8 + BOM encoding. Please reexport with UTF-8.")
  }

  # return object ----
  study_options <- list(sep = sep,
                        quote = quote,
                        date_format = date_format,
                        datetime_format = datetime_format,
                        date_format_meta = date_format_meta,
                        # this stays with a "." to keep consistency
                        # with read.table "na.strings" parameter
                        na.strings = na.strings, # if blanks mean missing
                        short_names = short_names,
                        is_zip = is_zip,
                        is_rectangular = rectangular_table,
                        audit_trail = audit_trail,
                        column_names = column_names,
                        lang_not_supported = lang_not_supported,
                        dict_items = dict_items,
                        refvals_separate = refvals_seperate,
                        add_id = NULL, # handled in read_secuTrial_raw
                        lab_id = NULL, # handled in read_secuTrial_raw
                        meta_names = meta_names,
                        meta_available = meta_available,
                        duplicate_meta = duplicate_meta,
                        all_files = files$Name,
                        data_files = datafiles,
                        data_names = datanames,
                        file_end = file_tag,
                        extension = file_extension,
                        data_dir = data_dir,
                        secuTrial_version = version,
                        project_version = pversion,
                        project_name = pname,
                        format_info = format_info,
                        time_of_export = time_of_export,
                        encoding = encoding,
                        form_status = form_status,
                        centre_info = centre_info,
                        hidden_fields = hidden_fields,
                        structure = structure,
                        proj_setup = proj_setup,
                        factorized = FALSE,
                        dated = FALSE,
                        labelled = FALSE)
  class(study_options) <- "secuTrialoptions"
  return(study_options)
}

#' @export
print.secuTrialoptions <- function(x, ...) {
  cat(paste("secuTrial version:", x$secuTrial_version, "\n"))
  cat(paste("Time of export on server:", x$time_of_export, "\n"))
  cat(paste("Project version:", x$project_version, "\n"))
  if (x$short_names) cat("Exported with short names \n")
  if (!x$short_names) cat(paste("File names appended with:", x$file_end, "\n"))
  cat(paste("File extension:", x$extension, "\n"))
  # Print " and tab with escape char
  if(grepl("\"", x$quote)) x$quote <- "\\\""
  if(grepl("\t", x$quote)) x$quote <- "\\t"
  if(grepl("\"", x$sep)) x$sep <- "\\\""
  if(grepl("\t", x$sep)) x$sep <- "\\t"
  cat(paste0("Enclosure: '", x$quote, "'\n"))
  cat(paste0("Seperator: '", x$sep, "'\n"))
  cat(paste(length(x$all_files), "files exported\n"))
  cat(paste("  including", sum(unlist(x$meta_available)), "metadata tables\n"))
  cat(paste("Reference values",
            ifelse(x$refvals_separate,
                   "exported - factorize possible\n",
                   "not exported - factorize not possible\n")))
  cat("Metadata tables:\n")
  df <- data.frame(type = names(x$meta_available),
                   exportname = unlist(x$meta_names),
                   available = unlist(x$meta_available))
  print(df, row.names = FALSE)
}
