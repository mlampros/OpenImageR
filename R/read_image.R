
#' this function reads various types of images
#' 
#' Reads images of type .png, .jpeg, .jpg, .tiff
#' 
#' @param path a string specifying the path to the saved image
#' @param ... further arguments for the readPNG, readJPEG and readTIFF functions
#' @return the image in a matrix or array form
#' @details
#' This function takes as input a string-path and returns the image in a matrix or array form. Supported types of images are .png, .jpeg, .jpg, .tiff.
#' Extension types similar to .tiff such as .tif, .TIFF, .TIF are also supported
#' @export
#' @examples
#' 
#' path = system.file("tmp_images", "1.png", package = "OpenImageR")
#' 
#' image = readImage(path)
#' 


readImage = function(path, ...) {
  
  try_err_file = inherits(tryCatch(normalizePath(path, mustWork = T), error = function(e) e), "error")
  
  if (class(path) != "character" || try_err_file == T) stop('the path to an image is invalid or the image does not exist')
  
  flag_type = strsplit(path, '[.]')[[1]]
  
  flag_type = flag_type[length(flag_type)]
  
  if (length(flag_type) == 0) stop('invalid path')
  
  if (flag_type == "png") {
    
    img = png::readPNG(path, ...)}
  
  else if (flag_type == "jpg" || flag_type == "jpeg") {
    
    img = jpeg::readJPEG(path, ...)}
  
  else if (flag_type %in% c("tiff", "tif", "TIFF", "TIF")) {
    
    img = tiff::readTIFF(path, ...)}
  
  else {
    
    stop('supported image types are .png, .jpeg, .jpg, .tiff (or .tif, .TIFF, .TIF)')
  }
  
  return(img)
}


