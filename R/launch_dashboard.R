#' @export

launch_dashboard <- function(){
  shiny::runApp(system.file("shinyApp", package = "secuTrialR"))
}
