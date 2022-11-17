
#' display an image
#'
#'
#' This function displays an image
#'
#' @param file_path if file_path is a character string, then a shiny application is utilized. If file_path is a matrix, data.frame OR a 3-dimensional array (where the third dimension is equal to 3) then the grid.raster function of the base grid package is used.
#' @param clear_viewer a boolean. If TRUE then the previous image will be removed in the viewer before displaying the next one
#' @return displays an image
#' @details
#' This function displays an image using either a character path, a 2- or a 3-dimensional object where the third dimension is equal to 3
#'
#' @importFrom grid grid.raster
#'
#' @export
#' @examples
#'
#' # path = system.file("tmp_images", "1.png", package = "OpenImageR")
#'
#' # imageShow(path)
#'


imageShow = function(file_path, clear_viewer = FALSE) {

  try_err_files = inherits(tryCatch(normalizePath(file_path, mustWork = T), error = function(e) e), "error")
  if (!inherits(file_path, "character") && length(dim(file_path)) == 2 && !is.matrix(file_path)) file_path = as.matrix(file_path)

  if (inherits(file_path, 'matrix') || inherits(file_path, 'array')) {

    file_path = func_chech_range(file_path)
    if (clear_viewer) grDevices::graphics.off()
    grid::grid.raster(file_path)
  }
  else if (inherits(file_path, "character") && try_err_files == F){

    file_path <<- normalizePath(file_path)
    runUI()
  }
  else {
    stop('invalid path or object')
  }
}


