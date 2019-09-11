#' Compose a secuTrial URL
#'
#' Given a Secutrial server URL, and optionally instance, customer, project id, document id and subdoc id,
#' this function composes a URL to a specific secuTrial instance, customer, form or subform.
#'
#' If only server URL is provided, the output will point to the secutrial Server
#' If secuTrial server and instance are provided, the output will point to the secuTrial instance.
#' If secutrial server, instance and customer id are provided, the output will point to the customer page.
#' If secuTrial server, instance, customer, project and document id are provided, the output will point to a specific secuTrial form.
#'
#' @param server - string containing a server URL
#' @param instance - (optional) string containing secuTrial instance name
#' @param customer - (optional) string containing secuTrial customer label
#' @param projid - (optional) string containing secuTrial project identifier
#' @param docid - (optional) secuTrial document/form identifer
#' @return string containing a URL to desired secuTrial page. Currently we provide no guarantee that the returned URL is valid.
#' @export
#'
build_secuTrial_url <- function(server, instance = NA, customer = NA, projid = NA, docid = NA){
  if (!grepl("^https://", server)){
    server <- paste0("https://", server)
  }
  if (!grepl("/$", server)){
    server <- paste0(server, "/")
  }
  # start building the secuTrial url
  sT_url <- server
  # check what info is availalbe and compose with what you have
  if (!is.na(instance)){
    sT_url <- paste0(sT_url, "apps/WebObjects/", instance)
    if (!is.na(customer)){
      sT_url <- paste0(sT_url, ".woa/wa/choose?customer=", customer)
      if (!is.na(projid)){
        sT_url <- paste0(sT_url, "&projectid=", projid)
        if (!is.na(docid)){
          sT_url <- paste0(sT_url, "&docid=", docid)
        }
      }
    }
  }
  return(sT_url)
}
