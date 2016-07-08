
#' launcher for the shiny application
#' 
#' @keywords internal

runUI <- function () {
  
  shiny::runApp( system.file('shiny_app', package = 'OpenImageR') )
}