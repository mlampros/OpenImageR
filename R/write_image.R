
#' This function writes 2- or 3-dimensional image data to a file
#'
#' This function writes 2- or 3-dimensional image data to a file. Supported types are .png, .jpeg, .jpg, .tiff (or .tif, .TIFF, .TIF)
#'
#' @param data a 2- or 3-dimensional object (matrix, data frame or array)
#' @param file_name a string specifying the name of the new file
#' @param ... further arguments for the writePNG, writeJPEG and writeTIFF functions
#' @return a saved image file
#' @details
#' This function takes as input a matrix, data frame or array and saves the data in one of the supported image types ( .png, .jpeg, .jpg, .tiff ).
#' Extension types similar to .tiff such as .tif, .TIFF, .TIF are also supported
#'
#' @importFrom png writePNG
#' @importFrom jpeg writeJPEG
#' @importFrom tiff writeTIFF
#'
#' @export
#' @examples
#'
#' # path = system.file("tmp_images", "1.png", package = "OpenImageR")
#'
#' # im = readImage(path)
#'
#' # writeImage(im, 'new_image.jpeg')
#'


writeImage = function(data, file_name, ...) {

  if (inherits(data, 'data.frame')) data = as.matrix(data)
  if (!inherits(data, c('matrix', 'array'))) stop('supported image data are matrix, data frame, array')
  if (!inherits(file_name, "character")) stop("The file_name should be a character string. For instance, '/home/my_image.png'")

  flag_type = strsplit(file_name, '[.]')[[1]]
  flag_type = flag_type[length(flag_type)]

  if (flag_type == "png") {

    png::writePNG(data, target = file_name, ...)
  }
  else if (flag_type == "jpg" || flag_type == "jpeg") {

    jpeg::writeJPEG(data, target = file_name, ...)
  }
  else if (flag_type %in% c("tiff", "tif", "TIFF", "TIF")) {

    tiff::writeTIFF(data, where = file_name, ...)
  }
  else {

    stop('supported image types are .png, .jpeg, .jpg, .tiff (or .tif, .TIFF, .TIF)')
  }
}


