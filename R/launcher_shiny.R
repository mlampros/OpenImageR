
#' launcher for the shiny application
#'
#' @param file_path the normalized path of the image to display (read by the server through shiny::getShinyOption)
#'
#' @importFrom shiny runApp
#'
#' @keywords internal

runUI <- function (file_path) {

  shiny::shinyOptions(OpenImageR_file_path = file_path)

  shiny::runApp( system.file('shiny_app', package = 'OpenImageR') )
}
