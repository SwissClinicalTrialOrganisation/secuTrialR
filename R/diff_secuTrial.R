#' Returns differences in the setup of two \code{secuTrialdata} objects
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
#' @return If there are differences, \code{diff_secuTrial()} will produce a list of vectors.
#'         The fist vector informs about new forms and the second vector informs about
#'         new variables.
#'
#' @examples
#' # read exports
#'
#' # v1 is essentially a clone of the CTU05 setup
#' ctu06_v1 <- read_secuTrial(system.file("extdata", "sT_exports", "change_tracking",
#'                                        "s_export_CSV-xls_CTU06_version1.zip",
#'                                        package = "secuTrialR"))
#' # v2 contains 2 additional forms (mnpctu06anewform, mnpctu06anothernewform) and
#' # 2 additional variables (new_item_in_fu, new_item_in_new_form)
#' ctu06_v2 <- read_secuTrial(system.file("extdata", "sT_exports", "change_tracking",
#'                                        "s_export_CSV-xls_CTU06_version2.zip",
#'                                        package = "secuTrialR"))
#' # return diff
#' diff_secuTrial(ctu06_v1, ctu06_v2)
#'
diff_secuTrial <- function(x, y) {
  if (class(x) == "secuTrialdata" & class(y) == "secuTrialdata") {

    # comparisons are only possible if the project setup was exported
    if (! x$export_options$proj_setup & y$export_options$proj_setup) {
      stop("Both exports must be exported with Project setup data to allow the comparison.")
    }

    # comapring different projects makes no sense really
    if (x$export_options$project_name != y$export_options$project_name) {
      stop("The two exports appear to originate from different secuTrial projects.")
    }

    if (x$export_options$project_version == y$export_options$project_version) {
      print(paste0("The project structure has not changed. Project versions are both: ",
                   x$export_options$project_version))
    } else if (x$export_options$project_version != y$export_options$project_version) {
      # forms
      forms_x <- x[[x$export_options$meta_names$forms]]
      forms_y <- y[[y$export_options$meta_names$forms]]
      # new forms
      form_diff_new <- unique(forms_y$formtablename[which(! forms_y$formtablename %in% forms_x$formtablename)])
      # removed forms
      form_diff_rm <- unique(forms_x$formtablename[which(! forms_x$formtablename %in% forms_y$formtablename)])

      # items
      items_x <- x[[x$export_options$meta_names$items]]
      items_y <- y[[y$export_options$meta_names$items]]
      # new items
      item_diff_new <- unique(items_y$ffcolname[which(! items_y$ffcolname %in% items_x$ffcolname)])
      # removed items
      item_diff_rm <- unique(items_x$ffcolname[which(! items_x$ffcolname %in% items_y$ffcolname)])

      return_list <- list(form_diff_new, item_diff_new, form_diff_rm, item_diff_rm)
      names(return_list) <- c("new_forms", "new_variables", "removed_forms", "removed_variables")
      return(return_list)
    }
  } else {
    stop("diff_secuTrial requires objects of the class 'secuTrialdata' as input.")
  }
}
