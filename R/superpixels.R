
#' SLIC and SLICO superpixel implementations
#'
#'
#' @param input_image a 3-dimensional input image (the range of the pixel values should be preferably in the range 0 to 255)
#' @param method a character string specifying the method to use. Either "slic" or "slico"
#' @param superpixel a numeric value specifying the number of superpixels to use
#' @param compactness a numeric value specifying the compactness parameter. The \emph{compactness} parameter is needed only if \emph{method} is "slic". The "slico" method adaptively chooses the compactness parameter for each superpixel differently.
#' @param return_slic_data a boolean. If TRUE then the resulted slic or slico data will be returned
#' @param return_lab_data a boolean. If TRUE then the Lab data will be returned
#' @param return_labels a boolean. If TRUE then the labels will be returned
#' @param write_slic a character string. If not an empty string ("") then it should be a path to the output file with extension .bin ( for instance "/my_dir/output.bin" ). The data will be saved in binary format.
#' @param verbose a boolean. If TRUE then information will be printed in the R session
#' @details
#'
#' The \emph{input_image} can take images of any type as long as they are 3-dimensional. In case that a .tif file has less than 3-image-bands the user can convert the image to 3-dimensional using the \emph{List_2_Array ()} function.
#'
#' @export
#' @references
#' https://ivrl.epfl.ch/research-2/research-current/research-superpixels
#' @examples
#'
#' library(OpenImageR)
#'
#' path = system.file("tmp_images", "slic_im.png", package = "OpenImageR")
#' 
#' im = readImage(path)
#'
#' res = superpixels(input_image = im, method = "slic", superpixel = 200, 
#' 
#'                   compactness = 20, return_slic_data = TRUE)
#'

superpixels = function(input_image, method = "slic", superpixel = 200, compactness = 20,
                       return_slic_data = FALSE, return_lab_data = FALSE, 
                       return_labels = FALSE, write_slic = "", verbose = FALSE) {

  return(interface_superpixels(input_image, method, superpixel, compactness, return_slic_data,
                               return_lab_data, return_labels, write_slic, verbose))
}




#' load 3-dimensional data from a binary file
#'
#'
#' @param slic_data a 3-dimensional array
#' @details
#'
#' This function can be used to load 3-dimensional data from a binary file. It is used in combination with the \emph{superpixels} function in case that the \emph{write_slic} parameter is not an empty string ("").
#'
#' @export
#' @examples
#'
#' \dontrun{
#'
#' library(OpenImageR)
#'
#' path = "/my_dir/data.bin"
#'
#' res = load_3d_binary(path)
#'
#' }
#'

load_3d_binary = function(slic_data) {

  if (dim(slic_data)[3] != 3) { stop("The 'slic_data' parameter should be a 3-dimensional data object!", call. = F) }
  return(LOAD_3d_data(slic_data))
}


#' Conversion of RGB to Lab colour type
#'
#'
#' @param input_data a 3-dimensional array (RGB image)
#' @export
#' @details 
#' Meaning: RGB (Red-Green-Blue) to LAB (Lightness, A-colour-dimension, B-colour-dimension) colour conversion
#' @references
#' https://ivrl.epfl.ch/research-2/research-current/research-superpixels/research-snic_superpixels/
#' @examples
#'
#' library(OpenImageR)
#'
#' set.seed(1)
#' array_3d = array(sample(1:255, 675, replace = TRUE), c(15, 15, 3))
#'
#' res = RGB_to_Lab(array_3d)
#'

RGB_to_Lab = function(input_data) {

  if (dim(input_data)[3] != 3) { stop("The 'slic_data' parameter should be a 3-dimensional data object!", call. = F) }
  return(rgbtolab(input_data))
}


#' Conversion of RGB to HSV colour type
#'
#'
#' @param input_data a 3-dimensional array (RGB image)
#' @export
#' @details 
#' Meaning: RGB (Red-Green-Blue) to HSV (Hue, Saturation, Value) colour conversion
#' @examples
#'
#' library(OpenImageR)
#'
#' set.seed(1)
#' array_3d = array(sample(1:255, 675, replace = TRUE), c(15, 15, 3))
#'
#' res = RGB_to_HSV(array_3d)
#'

RGB_to_HSV = function(input_data) {

  if (dim(input_data)[3] != 3) { stop("The 'slic_data' parameter should be a 3-dimensional data object!", call. = F) }
  return(RGB_to_hsv(input_data))
}

