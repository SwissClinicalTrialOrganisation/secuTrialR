# Load secuTrial data export options
#
# @description
# The read_export_options reads the secuTrial data export and
# determines specific options needed to handle the data properly.
#
# This is an internal function which is wrapped by read_secuTrial_export
#
# @param data_dir string The data_dir specifies the path to the secuTrial data export.
#
# @return The function returns a list with the data export options.
#
# @examples
# \donttest{
# read_export_options(data_dir = system.file("extdata",
#                                            "s_export_CSV-xls_BMD.zip",
#                                            package = "secuTrialR"))
# }
#
read_export_options <- function(data_dir) {

  is_zip <- grepl(".zip$", data_dir)

  # load ExportOptions source
  if (is_zip) {
    files <- unzip(data_dir, list = TRUE)
    study_options_file_idx <- grep("ExportOptions", files$Name)
    file_con <- unz(data_dir, files$Name[study_options_file_idx])
    parsed_export <- readLines(file_con)
    close(file_con)
  } else {
    files <- data.frame(Name = list.files(data_dir))
    files$Name <- as.character(files$Name)
    study_options_file_idx <- grep("ExportOptions", files$Name)
    parsed_export <- readLines(file.path(data_dir, files$Name[study_options_file_idx]))
  }
  # version reference is on the bottom of the page
  version_line <- parsed_export[max(grep("secuTrial", parsed_export))]
  version <- unlist(regmatches(version_line,
                               gregexpr(pattern = "[[:digit:]]+\\.[[:digit:]]+\\.[[:digit:]]+\\.[[:digit:]]+",
                                        text = version_line)
                               )
                    )
  pversion_line <- parsed_export[max(grep("Version", parsed_export)) + 2]
  pversion <- unlist(regmatches(pversion_line,
                               gregexpr(pattern = "(?<=\\().*(?=\\))",
                                        text = pversion_line, perl = TRUE)
  )
  )

  # short names
  short_names <- any(grepl("[sS]horten", parsed_export))
  # rectangular data
  rectangular_table <- any(grepl("[rR]ectangular table", parsed_export))
  # audit trail
  audit_trail <- any(grepl("[aA]udit [tT]rail", parsed_export))
  # language not english
  lang_not_en <- any(grepl("Export Optionen", parsed_export))
  # Column names
  column_names <- any(grepl("[cC]olumn names", parsed_export))
  # Duplicate form meta data into all tables
  duplicate_meta <- any(grepl("[dD]uplicate form meta data into all tables", parsed_export))

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
    file_tag <- gsub(pattern = "ExportOptions|.html",
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

  # find form data separator ----
  if (is_zip){
    file_con <- unz(data_dir, files$Name[!grepl("html$", files$Name)][1])
    header <- readLines(file_con, 1)
    close(file_con)
  } else if (!is_zip) {
    header <- readLines(file.path(data_dir, files$Name[!grepl("html$", files$Name)][1]), 1)
  }
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
  } else {
    stop("Error: Field separator could not be retrieved.")
    return(NULL)
  }

  # NA strings
  na.strings <- c("NA", "")
  # TODO : custom formats? parsed from ExportOptions?

  # reference values
  refvals_seperate <- any(grepl("separate table", parsed_export))

  # dates ----
  # date format
  date.format.meta <- "%Y-%m-%d"
  date.format <- "%Y%m%d"
  datetime.format <- "%Y%m%d%H%M"
  # TODO : custom formats? parsed from ExportOptions?

  # unknown date strings
  unknown_date_string <- NULL
  # TODO : custom formats? parsed from ExportOptions?

  # partial dates
  partial_date_string <- ""
  partial_date_handling <- "fill.partial.dates.and.keep.original"
  # TODO : parsed from ExportOptions?

  # IDs
  # TODO : parsed from ExportOptions?

  # filenames
  datafiles <- files$Name[!grepl(".html$", files$Name)]

  datanames <- gsub(file_tag, "", datafiles)
  datanames <- gsub(paste0("\\.", file_extension), "", datanames)
  # long names
  datanames <- gsub(pattern = "^mnp", "", datanames)
  names(datanames) <- datafiles

  # return object ----
  study.options <- list(sep = sep,
                        date.format = date.format,
                        datetime.format = datetime.format,
                        date.format.meta = date.format.meta,
                        na.strings = na.strings, # if blanks mean missing
                        unknown.date.string = unknown_date_string, # incomplete dates
                        partial.date.string = partial_date_string,
                        partial.date.handling = partial_date_handling,
                        short_names = short_names,
                        is_zip = is_zip,
                        is_rectangular = rectangular_table,
                        audit_trail = audit_trail,
                        column_names = column_names,
                        lang_not_en = lang_not_en,
                        refvals_separate = refvals_seperate,
                        add_id = NULL, # handled in read_secuTrial_export
                        lab_id = NULL, # handled in read_secuTrial_export
                        meta_names = meta_names,
                        meta_available = meta_available,
                        duplicate_meta = duplicate_meta,
                        all_files = files$Name,
                        data_files = datafiles,
                        data_names = datanames,
                        file_end = file_tag,
                        extension = file_extension,
                        data_dir = data_dir,
                        secuTrial.version = version,
                        project.version = pversion,
                        factorized = FALSE,
                        dated = FALSE,
                        labelled = FALSE,
                        durations = FALSE)
  class(study.options) <- "secutrialoptions"
  return(study.options)
}

#' @export
print.secutrialoptions <- function(x){
  cat(paste("SecuTrial version:", x$secuTrial.version, "\n"))
  cat(paste("Project version:", x$project.version, "\n"))
  if (x$short_names) cat("Exported with short names \n")
  if (!x$short_names) cat(paste("File names appended with:", x$file_end, "\n"))
  cat(paste("File extension:", x$extension, "\n"))
  cat(paste0("Seperator: '", x$sep, "'\n"))
  cat(paste(length(x$all_files), "files exported\n"))
  cat(paste("  including", sum(unlist(x$meta_available)), "metadata elements\n"))
  cat(paste("Reference values",
            ifelse(x$refvals_separate,
                   "exported - factorize possible\n",
                   "not exported - factorize not possible\n")))
  cat("Metadata elements:\n")
  df <- data.frame(type = names(x$meta_available),
                   exportname = unlist(x$meta_names),
                   available = unlist(x$meta_available))
  print(df, row.names = FALSE)

}
