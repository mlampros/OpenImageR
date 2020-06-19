
#' SLIC and SLICO superpixel implementations
#'
#'
#' @param input_image either a 2-dimensional or a 3-dimensional input image (the range of the pixel values should be preferably in the range 0 to 255)
#' @param method a character string specifying the method to use. Either "slic" or "slico"
#' @param superpixel a numeric value specifying the number of superpixels to use
#' @param compactness a numeric value specifying the compactness parameter. The \emph{compactness} parameter is needed only if \emph{method} is "slic". The "slico" method adaptively chooses the compactness parameter for each superpixel differently.
#' @param return_slic_data a boolean. If TRUE then the resulted slic or slico data will be returned
#' @param return_lab_data a boolean. If TRUE then the Lab data will be returned ( the Lab-colour format )
#' @param return_labels a boolean. If TRUE then the labels will be returned
#' @param write_slic a character string. If not an empty string ("") then it should be a path to the output file with extension .bin ( for instance "/my_dir/output.bin" ). The data will be saved in binary format.
#' @param verbose a boolean. If TRUE then information will be printed in the R session
#' @export
#' @references
#' https://www.epfl.ch/labs/ivrl/research/slic-superpixels/
#' @examples
#'
#' library(OpenImageR)
#'
#' #-------------------
#' # 3-dimensional data
#' #-------------------
#'
#' path = system.file("tmp_images", "slic_im.png", package = "OpenImageR")
#'
#' im = readImage(path)
#'
#' res = superpixels(input_image = im, method = "slic", superpixel = 200,
#'
#'                   compactness = 20, return_slic_data = TRUE)
#'
#'
#' #-------------------
#' # 2-dimensional data
#' #-------------------
#'
#' im_2d = im[,,1]
#'
#' res_mt = superpixels(input_image = im_2d, method = "slic", superpixel = 200,
#'
#'                      compactness = 20, return_slic_data = TRUE)
#'

superpixels = function(input_image, method = "slic", superpixel = 200, compactness = 20,
                       return_slic_data = FALSE, return_lab_data = FALSE,
                       return_labels = FALSE, write_slic = "", verbose = FALSE) {

  if (!inherits(input_image, c('matrix', 'array'))) {
    stop("The 'superpixels' function takes either matrices or arrays as input!", call. = F)
  }
  if (inherits(input_image, 'matrix')) {                                    # in case of a 'matrix' convert it to an array with 1 slice [ for instance dims = c(128, 128, 1) ]
    input_image = array(data = input_image, dim = c(dim(input_image), 1))
  }

  dat = interface_superpixels(input_image, method, superpixel, compactness, return_slic_data,
                              return_lab_data, return_labels, write_slic, verbose)

  if (inherits(input_image, 'matrix')) {
    if (return_slic_data) {
      dat$slic_data = dat$slic_data[,,1]
    }
    if (return_lab_data) {
      dat$lab_data = dat$lab_data[,,1]
    }
  }

  return(dat)
}




#' Bounding box for the superpixel labels
#'
#'
#' @param superpixel_labels a matrix. The \emph{superpixel_labels} parameter corresponds to the output \emph{labels} of the \emph{superpixels} function
#' @param non_overlapping_superpixels either TRUE or FALSE. If TRUE then besides the (x,y) coordinates of each superpixel-segment (matrix), the overlapping indices for each superpixel will be returned (list). See the details section for more information
#' @details
#'
#' If the \emph{non_overlapping_superpixels} parameter is set to \emph{FALSE} then : the \emph{superpixel_bbox} function returns the bounding box for the labels of the \emph{superpixels} function. The
#' output is a matrix which contains the min and max indices of the x-y-coordinates and the corresponding unique superpixel labels.
#'
#' If the \emph{non_overlapping_superpixels} parameter is set to \emph{TRUE} then : the \emph{superpixel_bbox} function returns besides the previously explained matrix also the overlapping indices for each
#' superpixel. These indices can be used to overwrite pixels with a specific value (say 0.0), which might appear in two superpixels simultaneously. This feature might be useful in case a user
#' intends to use an algorithm and the separability of superpixel-segments is of importance.
#'
#' Therefore in both cases overlapping superpixels will be computed, however if the \emph{non_overlapping_superpixels} parameter is set to \emph{TRUE} then also a list of overlapping indices will be returned.
#'
#' @export
#' @examples
#'
#' library(OpenImageR)
#'
#'
#' #-----------
#' # read image
#' #-----------
#'
#' path = system.file("tmp_images", "slic_im.png", package = "OpenImageR")
#'
#' im = readImage(path)
#'
#' im = im[,, 1:3]
#'
#'
#' #--------------------
#' # compute superpixels
#' #--------------------
#'
#' res = superpixels(input_image = im, method = "slic", superpixel = 200,
#'
#'                   compactness = 20, return_labels = TRUE)
#'
#'
#' #-------------------------
#' # compute the bounding box
#' #-------------------------
#'
#' bbox = superpixel_bbox(res$labels, non_overlapping_superpixels = FALSE)
#'
#'
#' #-------------------------------------------
#' # plot the bounding boxes of the superpixels ( for illustration purposes )
#' #-------------------------------------------
#'
#'
#' graphics::plot(1:ncol(im), type='n', xlim = c(ncol(im), 1), ylim = c(1, nrow(im)))
#'
#' graphics::rasterImage( flipImage(im), 1, 1, ncol(im), nrow(im))
#'
#'
#' for (i in 1:nrow(bbox)) {
#'
#'   # the order of the bounding box is c('xmin', 'ymin', 'xmax', 'ymax')
#'   graphics::rect(bbox[i,3], bbox[i,1], bbox[i,4], bbox[i,2], border = "red", lwd = 2)
#' }
#'

superpixel_bbox = function(superpixel_labels, non_overlapping_superpixels = FALSE) {
  
  if (!inherits(superpixel_labels, 'matrix')) {
    stop("The 'superpixel_labels' parameter must be of type 'matrix'!")
  }
  
  mt = spix_bbox(superpixel_labels, non_overlapping_superpixels)
  
  if (non_overlapping_superpixels) {
    
    mt$overlapping_pixs = lapply(mt$overlapping_pixs, function(x) {
      
      if (length(x) > 0) {
        as.vector(x) + 1         # adjust the indices to R
      }
      else {
        as.vector(x)
      }
    })
    
    mt$bbox_matrix[, 1:4] = mt$bbox_matrix[, 1:4] + 1
    colnames(mt$bbox_matrix) = c('y_min', 'y_max', 'x_min', 'x_max', 'dif_y', 'dif_x', 'superpixel_segment')
  }
  else {
    mt = mt[[1]]
    mt[, 1:4] = mt[, 1:4] + 1                  # Adjust the indexing (first 4 columns only) due to the difference between C++ and R  [ avoid this inside the C++ code because it's possible that I'll utilize the C++ function somewhere else ]
    colnames(mt) = c('y_min', 'y_max', 'x_min', 'x_max', 'dif_y', 'dif_x', 'superpixel_segment')
  }
  
  return(mt)
}



#' Bounding box for a subset of superpixel labels
#'
#'
#' @param superpixel_labels a matrix. The \emph{superpixel_labels} parameter corresponds to the output \emph{labels} of the \emph{superpixels} function
#' @param superpixel_subset a numeric or integer vector specifying the subset of superpixel segments.
#' @details
#' This function should be utilized to return the bounding box for a subset of superpixel segments. To compute the bounding box for all superpixels use the \emph{superpixel_bbox} function.
#' @export
#' @examples
#'
#' library(OpenImageR)
#'
#'
#' #-----------
#' # read image
#' #-----------
#'
#' path = system.file("tmp_images", "slic_im.png", package = "OpenImageR")
#'
#' im = readImage(path)
#'
#' im = im[,, 1:3]
#'
#'
#' #--------------------
#' # compute superpixels
#' #--------------------
#'
#' res = superpixels(input_image = im, method = "slic", superpixel = 200,
#'
#'                   compactness = 20, return_labels = TRUE)
#'
#'
#' #-------------------------
#' # compute the bounding box   ( for subset of superpixels )
#' #-------------------------
#'
#' bbox = superpixel_bbox_subset(res$labels, superpixel_subset = c(0, 10, 30))
#'

superpixel_bbox_subset = function(superpixel_labels, superpixel_subset) {
  
  if (!inherits(superpixel_labels, 'matrix')) {
    stop("The 'superpixel_labels' parameter should be of type 'matrix'!")
  }
  if (!inherits(superpixel_subset, c('numeric', 'integer'))) {
    stop("The 'superpixel_subset' parameter should be of type vector!")
  }
  
  mt = spix_bbox_vector(superpixel_labels, superpixel_subset)
  mt = as.vector(mt)
  mt[1:4] = mt[1:4] + 1                  # Adjust indexing (first 4 columns only) due to the difference between C++ and R  [ don't do this inside C++ because it might be the case that I'll use the C++ function directly somewhere else ]
  mt = matrix(mt, nrow = 1, ncol = 6)
  colnames(mt) = c('y_min', 'y_max', 'x_min', 'x_max', 'dif_y', 'dif_x')
  
  return(mt)
}



#' Padding of matrices or n-dimensional arrays with a user specified value
#'
#'
#' @param input_data either a matrix or a 3-dimensional array
#' @param new_rows an integer specifying the new rows of the output matrix or array
#' @param new_cols an integer specifying the new columns of the output matrix or array
#' @param fill_value a numeric value to fill the extended rows / columns of the initial input data
#' @return a list
#' @details
#' The \emph{padding} function returns a list, where \emph{data} is the padded / extended matrix or array and \emph{padded_start}, \emph{padded_end}, \emph{padded_left} and \emph{padded_right} are integer values specifying how
#' many rows or columsn in up-, down-, left- or right-direction the input matrix or array was padded / extended with the specified fill-value.
#' @export
#' @examples
#'
#' library(OpenImageR)
#'
#'
#' #-------
#' # matrix
#' #-------
#'
#' set.seed(1)
#' mt = matrix(runif(100), 10, 10)
#'
#' res_mt = padding(mt, 15, 20, fill_value = -1)
#'
#'
#' #------
#' # array
#' #------
#'
#' lst = list(matrix(1, 10, 10), matrix(2, 10, 10))
#'
#' arr = List_2_Array(lst, verbose = FALSE)
#'
#' res_arr = padding(arr, 15, 20, fill_value = mean(as.vector(mt)))
#'

padding = function(input_data, new_rows, new_cols, fill_value = 0.0) {
  
  if (inherits(input_data, 'matrix')) {
    return(pad_matrix(input_data, new_rows, new_cols, fill_value))
  }
  else if ( is.array(input_data) && !is.na(dim(input_data)[3]) ) {
    new_array = res_out = list()
    metadata = NULL
    for (i in 1:dim(input_data)[3]) {
      tmp_pad = pad_matrix(input_data[,,i], new_rows, new_cols, fill_value)
      new_array[[i]] = tmp_pad$data
      if (i == 1) {
        metadata = tmp_pad[2:5]
      }
    }
    new_array = array(unlist(new_array), dim = c(nrow(new_array[[1]]), ncol(new_array[[1]]), length(new_array)))
    res_out[['data']] = new_array
    res_out[['padded_start']] = metadata$padded_start
    res_out[['padded_end']] = metadata$padded_end
    res_out[['padded_left']] = metadata$padded_left
    res_out[['padded_right']] = metadata$padded_right
    return(res_out)
  }
  else {
    stop("Invalid data type. The 'input_data' parameter should be either of type 'matrix' or of type 'array'!", call. = F)
  }
}



#' loads either 2- or 3-dimensional data from a binary file
#'
#'
#' @param path a character string specifying a file path ( where the binary data is saved )
#' @param type a character string. Either '2d' or '3d' to indicate what kind of data data will be loaded from the specified \emph{path}
#' @details
#'
#' This function can be used to load either 2- or 3-dimensional data from a binary file. It is used in combination with the \emph{superpixels} function in case that the \emph{write_slic} parameter is not an empty string ("").
#'
#' @export
#' @examples
#'
#' \dontrun{
#'
#' library(OpenImageR)
#'
#' #------------------------------------------
#' # assuming the saved data are 2-dimensional
#' #------------------------------------------
#'
#' path = "/my_dir/data.bin"
#'
#' res = load_binary(path, type = '2d')
#'
#' }
#'

load_binary = function(path, type) {

  lst_dat = LOAD_data(path, type)
  return(lst_dat[[1]])
}


#' Conversion of RGB to Lab colour type
#'
#'
#' @param input_data a 3-dimensional array (RGB image)
#' @export
#' @details
#' Meaning: RGB (Red-Green-Blue) to LAB (Lightness, A-colour-dimension, B-colour-dimension) colour conversion
#' @references
#' https://www.epfl.ch/labs/ivrl/research/snic-superpixels/
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

  if (is.na(dim(input_data)[3])) stop("The 'input_data' parameter should be a 3-dimensional data object!", call. = F)
  if (length(dim(input_data)) > 3) stop("The 'input_data' parameter should be a 3-dimensional data object!", call. = F)
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

  if (is.na(dim(input_data)[3])) stop("The 'input_data' parameter should be a 3-dimensional data object!", call. = F)
  if (length(dim(input_data)) > 3) stop("The 'input_data' parameter should be a 3-dimensional data object!", call. = F)
  return(RGB_to_hsv(input_data))
}

