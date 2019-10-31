#' Returns differences in the setup of two secuTrialdata objects
#' @description During ongoing studies it is possible that changes to the DataCapture interface
#'              are made. Sometimes these changes may call for adjustments in analysis code.
#'              It is considered good practice to run \code{diff_secuTrial()} on the last export
#'              and the current export of a project to at least make yourself aware of
#'              potential changes to the setup. If there are differences, the results of this function should
#'              be interpreted as a first indicator since they may not cover all alterations.
#'              Information is returned on new forms and variables.
#'              A detailed list of changes can be produced in the FormBuilder with
#'              "Compare project setup".
#' @param x a \code{secuTrialdata} object (the older export)
#' @param y a \code{secuTrialdata} object (the newer export)
#' @export
#' @details If there are differences, \code{diff_secuTrial()} will produce a list of vectors.
#'          The fist vector informs about new forms and the second vector informs about
#'          new variables.
#'
#' @examples
#' # export location
#' export_loc <- system.file("extdata", "sT_exports", "longnames",
#'                           "s_export_CSV-xls_CTU05_long_ref_miss_en_utf8.zip",
#'                           package = "secuTrialR")
#' # read export
#' sT_export <- read_secuTrial(export_loc)
#'
#' # return scores // in this case on the same export
#' diff_secuTrial(sT_export, sT_export)
#'
diff_secuTrial <- function(x, y) {
  if (class(x) == "secuTrialdata" & class(y) == "secuTrialdata") {

    # comapring different projects makes no sense really
    if(x$export_options$project_name != y$export_options$project_name) {
      stop("The two exports appear to originate from different secuTrial projects.")
    }

    if(x$export_options$project_version == y$export_options$project_version) {
      print(paste0("The project structure has not changed. Project versions are both: ",
                   x$export_options$project_version))
    } else if (x$export_options$project_version != y$export_options$project_version) {
      # forms
      forms_x <- x[[x$export_options$meta_names$forms]]
      forms_y <- y[[y$export_options$meta_names$forms]]
      form_diff <- unique(forms_y$formtablename[which(! forms_y$formtablename %in% forms_x$formtablename)])

      # items
      items_x <- x[[x$export_options$meta_names$items]]
      items_y <- y[[y$export_options$meta_names$items]]
      item_diff <- unique(items_y$ffcolname[which(! items_y$ffcolname %in% items_x$ffcolname)])

      return_list <- list(form_diff, item_diff)
      names(return_list) <- c("new_forms", "new_variables")
      return(return_list)
    }
  } else {
    stop("diff_secuTrial requires objects of the class 'secuTrialdata' as input.")
  }
}
