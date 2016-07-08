
#' This function writes 2- or 3-dimensional image data to a file
#' 
#' This function writes 2- or 3-dimensional image data to a file ( supported types .png, .jpeg, .jpg, .tiff )
#' 
#' @param data a 2- or 3-dimensional object (matrix, data frame or array)
#' @param file_name a string specifying the name of the new file
#' @param ... further arguments for the writePNG, writeJPEG and writeTIFF functions
#' @return a saved image file
#' @details
#' This function takes as input a matrix, data frame or array and saves the data in one of the supported image types ( .png, .jpeg, .jpg, .tiff ).
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
  
  if (is.data.frame(data)) data = as.matrix(data)
  
  if (!class(data) %in% c('matrix', 'array')) stop('supported image data are matrix, data frame, array')
  
  if (class(file_name) != "character") stop("the file_name should be a character string such as '/home/my_image.png'")
  
  flag_type = strsplit(file_name, '[.]')[[1]]
  
  flag_type = flag_type[length(flag_type)]

  if (flag_type == "png") {
    
    png::writePNG(data, target = file_name, ...)}
  
  else if (flag_type == "jpg" || flag_type == "jpeg") {
    
    jpeg::writeJPEG(data, target = file_name, ...)}
  
  else if (flag_type == "tiff") {
    
    tiff::writeTIFF(data, where = file_name, ...)}
  
  else {
    
    stop('invalid image type')
  }
}


