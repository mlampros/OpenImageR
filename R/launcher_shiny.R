
#' launcher for the shiny application
#'
#' @importFrom shiny runApp
#'
#' @keywords internal

runUI <- function () {

  shiny::runApp( system.file('shiny_app', package = 'OpenImageR') )
}
