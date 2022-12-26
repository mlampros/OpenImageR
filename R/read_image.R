

#' Verify that the input image extension is valid
#'
#' @param image_path a character string specifying the path to the saved image
#' @param regex_img a character string specifying the regex used to verify if the image extension is valid
#' @return either the image path extension or an error
#' @details
#'
#' The OpenImageR package uses the 'readPNG', 'readJPEG' and 'readTIFF' R packages in the background. Thus, only image file path
#' extensions that can be processed from these R packages should be used as input to the 'readImage' function
#'
#' @importFrom tools file_ext
#'
#' @references
#'
#' https://github.com/mlampros/OpenImageR/issues/25
#'
#' @export
#' @examples
#'
#' vec_img_ext = c('png', 'PNG', 'jpg', 'JPG', 'jpeg', 'JPEG', 'tif', 'TIF', 'tiff', 'TIFF')
#'
#' vec_valid = sapply(vec_img_ext, function(x) {
#'   ext_iter = paste(c('example_image', x), collapse = '.')
#'   verify_image_extension(image_path = ext_iter)
#' })
#'
#' all(vec_img_ext == vec_valid)


verify_image_extension = function(image_path,
                                  regex_img = "jpe?g|png|tif$|tiff$") {

  img_ext = tools::file_ext(image_path)
  img_valid = grepl(x = img_ext, pattern = regex_img, ignore.case = TRUE)
  if (img_valid) {
    return(img_ext)
  }
  else {
    stop("The input image path does not have a file extension OR is not a supported image path extension of the OpenImageR package!", call. = F)
  }
}


#' this function reads various types of images
#'
#' Reads images of type .png, .jpeg, .jpg, .tiff
#'
#' @param path a character string specifying the path to the saved image
#' @param ... further arguments for the readPNG, readJPEG and readTIFF functions
#' @return the image in a matrix or array form
#' @details
#' This function takes as input a string-path and returns the image in a matrix or array form. Supported types of images are .png, .jpeg, .jpg, .tiff.
#' Extension types similar to .tiff such as .tif, .TIFF, .TIF are also supported
#'
#' @importFrom png readPNG
#' @importFrom jpeg readJPEG
#' @importFrom tiff readTIFF
#'
#' @export
#' @examples
#'
#' path = system.file("tmp_images", "1.png", package = "OpenImageR")
#'
#' image = readImage(path)
#'


readImage = function(path, ...) {

  try_err_file = inherits(tryCatch(normalizePath(path, mustWork = T), error = function(e) e), "error")
  if (!inherits(path, "character") || try_err_file == T) stop('the path to an image is invalid or the image does not exist!', call. = F)

  inp_img_ext = verify_image_extension(image_path = path)

  if (inp_img_ext %in% c("png", "PNG")) {
    img = png::readPNG(path, ...)
  }
  else if (inp_img_ext %in% c("jpg", "JPG", "jpeg", "JPEG")) {
    img = jpeg::readJPEG(path, ...)
  }
  else if (inp_img_ext %in% c("tiff", "tif", "TIFF", "TIF")) {
    img = tiff::readTIFF(path, ...)
  }
  else {
    stop('supported image types are .png, .jpeg, .jpg, .tif or .tiff (case insensitive)!', call. = F)
  }

  return(img)
}


