
#' gaussian-kernel
#'
#' @keywords internal

gaussian_kernel = function(xy_length = 2, sigma = 1.0, range_gauss = 2) {

  if (xy_length < 1 || !is.numeric(xy_length)) {

    stop('xy_length should be an integer greater than 0')
  }

  if (range_gauss <= 0.0) {

    stop("the 'range_gauss' should be a positive number")
  }

  gaussian_formula = function(x, y)  1/(2 * pi * (sigma ^ 2)) * (exp(-(x ^ 2 + y ^ 2)/(2 * sigma ^ 2)))

  tmp_seq = seq(-range_gauss, range_gauss, length = xy_length)

  tmp_outer = outer(tmp_seq, tmp_seq, gaussian_formula)

  return(tmp_outer/sum(tmp_outer))
}



#' normalize in specific range of values
#'
#' @keywords internal

norm_range_gauss = function(data, max_range = -1, min_range = 1) {

  if (!is.matrix(data)) {

    stop('data should be a matrix')
  }

  data = (data - min(data))/(max(data) - min(data))           # first normalize data to 0-1

  rng = max_range - min_range

  out = data * rng - min_range                        # then adjust to specified range

  return(round(out))
}


#' laplacian kernels
#'
#' @keywords internal

laplacian_kernels = function(type = 1) {

  if (type == 1) {

    out = matrix(c(1,1,1,1,-8,1,1,1,1), 3, 3)}

  else if (type == 2) {

    out = matrix(c(-1,2,-1,2,-4,2,-1,2,-1), 3, 3)}

  else if (type == 3) {

    out = matrix(c(0,1,0,1,-4,1,0,1,0), 3, 3)
  }

  else if (type == 4) {

    out = matrix(c(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,24,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1), 5, 5)
  }

  else {

    stop('invalid type for laplacian mask')
  }

  return(out)
}


#' secondary function for edge_detection function
#'
#' @keywords internal

switch_filter = function(kernel, conv_mod, gaussian_dims = 5, sigma = 1.0, laplacian_type = 1, range_gauss = 2) {

  kernel <- match.arg(kernel, c('Sobel', 'Prewitt', 'Roberts_cross', 'Frei_chen', 'Scharr', 'LoG'), FALSE)

  if (kernel == 'LoG') {                  # a discrete kernel approximation for laplacian of Gaussian for a sigma of 1.4, http://homepages.inf.ed.ac.uk/rbf/HIPR2/log.htm

    gaus_kern = norm_range_gauss(gaussian_kernel(gaussian_dims, sigma, range_gauss = 2), 255, 0)        # pixels between 0 and 255  (by default)

    lapl_kern = laplacian_kernels(laplacian_type)

    convl = conv2d(gaus_kern, lapl_kern, mode = conv_mod)

    return(convl/255.0)
  }

  else {

    switch(kernel,

           Sobel = {

             G_horiz = matrix(c(-1, 0, 1, -2, 0, 2, -1, 0, 1), 3, 3);
             G_vert = t(matrix(c(-1, 0, 1, -2, 0, 2, -1, 0, 1), 3, 3))},

           Prewitt = {

             G_horiz = matrix(c(-1, -1, -1, 0, 0, 0, 1, 1, 1), 3, 3);
             G_vert = t(matrix(c(-1, -1, -1, 0, 0, 0, 1, 1, 1), 3, 3))},

           Roberts_cross = {

             G_horiz = matrix(c(1, 0, 0, -1), 2, 2);
             G_vert = matrix(c(0, -1, 1, 0), 2, 2)},

           Frei_chen = {

             G_horiz = matrix(c(1, sqrt(2), 1, 0, 0, 0, -1, -sqrt(2), -1), 3, 3);
             G_vert = t(matrix(c(-1, -sqrt(2), -1, 0, 0, 0, 1, sqrt(2), 1), 3, 3))},

           Scharr = {

             G_horiz = t(matrix(c(3, 10, 3, 0, 0, 0, -3, -10, -3), 3, 3));
             G_vert = matrix(c(3, 10, 3, 0, 0, 0, -3, -10, -3), 3, 3)}
    )

    return(list(G_horiz = G_horiz, G_vert = G_vert))
  }
}



#' function to check the range of values of an image or normalize an image
#'
#' @keywords internal

func_chech_range = function(image) {

  if (class(image) == 'matrix' && (max(image) != 1.0 || min(image) != 0.0)) {

    image = Normalize_matrix(image)}

  if (class(image) == 'array' && (max(Array_range(image, 1)) != 1.0 || min(Array_range(image, 2)) != 0.0)) {

    image = Normalize_array(image)
  }

  image
}


#' edge detection (Frei_chen, LoG, Prewitt, Roberts_cross, Scharr, Sobel)
#'
#' @param image matrix or 3-dimensional array
#' @param method the method should be one of 'Frei_chen', 'LoG' (Laplacian of Gaussian), 'Prewitt', 'Roberts_cross', 'Scharr', 'Sobel'
#' @param conv_mode the convolution mode should be one of 'same', 'full'
#' @param approx if TRUE, approximate calculation of gradient (applies to all filters except for 'LoG')
#' @param gaussian_dims integer specifying the horizontal and vertical dimensions of the gaussian filter
#' @param sigma float parameter sigma for the gaussian filter
#' @param range_gauss float number specifying the range of values for the gaussian filter
#' @param laplacian_type integer value specifying the type for the laplacian kernel (one of 1, 2, 3, 4)
#' @return depending on the input, either a matrix or an array
#' @author Lampros Mouselimis
#' @details
#' This function takes either a matrix or a 3-dimensional array and it performs edge detection using one of the following filters : 'Frei_chen', 'LoG' (Laplacian of Gaussian),
#' 'Prewitt', 'Roberts_cross', 'Scharr', 'Sobel'
#' @export
#' @examples
#' 
#' path = system.file("tmp_images", "1.png", package = "OpenImageR")
#'
#' image = readImage(path)
#'
#' res = edge_detection(image, method = 'Frei_chen', conv_mode = 'same')
#'


edge_detection = function(image, method = NULL, conv_mode = 'same', approx = F, gaussian_dims = 5, sigma = 1.0, range_gauss = 2, laplacian_type = 1) {
  
  if (is.null(method) || !method %in% c('Frei_chen', 'LoG', 'Prewitt', 'Roberts_cross', 'Scharr', 'Sobel')) stop("method shoud be non-NULL and one of : 'Frei_chen', 'LoG', 'Prewitt', 'Roberts_cross', 'Scharr', 'Sobel'")
  if (is.data.frame(image)) image = as.matrix(image)                    # default conversion for armadillo function conv2d
  if (is.null(conv_mode)) stop("conv_mode should be one of : 'full', 'same'")
  if (!conv_mode %in% c('full', 'same')) stop("conv_mode should be one of : 'full', 'same'")
  
  if (!approx %in% c(T, F)) stop("the 'approx' argument should be a boolean")
  
  res_kernel = switch_filter(method, conv_mode, gaussian_dims = gaussian_dims, sigma = sigma, laplacian_type = laplacian_type, range_gauss = range_gauss)
  
  if (is.list(res_kernel)) {
    
    if (is.array(image) && !is.na(dim(image)[3]) && dim(image)[3] == 3) {
      
      new_image_horizontal = conv3d(image, res_kernel$G_horiz, conv_mode)
      
      new_image_vertical = conv3d(image, res_kernel$G_vert, conv_mode)
    }
    
    if (is.matrix(image)) {
      
      new_image_horizontal = conv2d(image, res_kernel$G_horiz, conv_mode)
      
      new_image_vertical = conv2d(image, res_kernel$G_vert, conv_mode)
    }
    
    if (approx) {
      
      res = abs(new_image_horizontal) + abs(new_image_vertical)}                   # an approximate calculation is much faster, http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.301.927&rep=rep1&type=pdf
    
    else {
      
      res = sqrt((new_image_horizontal) ^ 2 + (new_image_vertical) ^ 2)
    }
    
    res = func_chech_range(res)
    
    return(res)
  }
  
  else if (is.matrix(res_kernel)) {
    
    if (is.array(image) && !is.na(dim(image)[3]) && dim(image)[3] == 3) {
      
      new_LoG = conv3d(image, res_kernel, conv_mode)
    }
    
    if (is.matrix(image)) {
      
      new_LoG = conv2d(image, res_kernel, mode = conv_mode)
    }
    
    new_LoG = func_chech_range(new_LoG)
    
    return(new_LoG)
  }
  
  else {
    
    stop('invalid kernel object')
  }
}



#' uniform filter (convolution with uniform kernel)
#'
#' @param image matrix or 3-dimensional array
#' @param size a 2-item vector specifying the horizontal and vertical dimensions of the uniform kernel, e.g. c(3,3)
#' @param conv_mode the convolution mode should be one of 'same', 'full'
#' @return depending on the input, either a matrix or an array
#' @author Lampros Mouselimis
#' @details
#' This function applies a uniform filter to a matrix or to a 3-dimensional array
#' @export
#' @examples
#' 
#' path = system.file("tmp_images", "1.png", package = "OpenImageR")
#'
#' image = readImage(path)
#'
#' filt = uniform_filter(image, c(4,4), conv_mode = "same")
#'

uniform_filter = function(image, size, conv_mode = 'same') {
  
  if (!is.vector(size)) stop('The size argument must be a vector specifying the dimensions of the uniform filter such as c(3,3)')
  if (is.null(conv_mode)) stop("conv_mode should be one of : 'full', 'same'")
  if (!conv_mode %in% c('full', 'same')) stop("conv_mode should be one of : 'full', 'same'")
  
  unif_filt = matrix(1, ncol = size[1], nrow = size[2])/(size[1] * size[2])
  
  if (is.array(image) && !is.na(dim(image)[3]) && dim(image)[3] == 3) {
    
    out = conv3d(image, unif_filt, conv_mode)
  }
  
  else if (is.matrix(image)) {
    
    out = conv2d(image, unif_filt, conv_mode)}
  
  else {
    
    stop('valid type of input-images is array or matrix')
  }
  
  return(out)
}


#' image thresholding
#'
#' @param image matrix or 3-dimensional array
#' @param thresh the threshold parameter should be between 0 and 1 if the data is normalized or between 0-255 otherwise
#' @return a matrix
#' @author Lampros Mouselimis
#' @details
#' This function applies thresholding to a matrix or to a 3-dimensional array.
#' @export
#' @examples
#' 
#' path = system.file("tmp_images", "1.png", package = "OpenImageR")
#'
#' image = readImage(path)
#'
#' filt = image_thresholding(image, thresh = 0.5)
#'


image_thresholding = function(image, thresh) {
  
  if (thresh <= 0.0) stop('the thresh parameter should be greater than 0')

  if (is.data.frame(image)) { image = as.matrix(image) }

  if (is.matrix(image)) {

    image_out = ifelse(image > thresh, 1, 0)}

  else if (is.array(image) && !is.na(dim(image)[3]) && dim(image)[3] == 3) {

    image = rgb_2gray(image)

    image_out = ifelse(image > thresh, 1, 0)}

  else {

    stop('the image should be either a matrix or an array')
  }

  return(image_out)
}



#' Gamma correction
#'
#' @param image matrix or 3-dimensional array
#' @param gamma a positive value
#' @return depending on the input, either a matrix or an array
#' @author Lampros Mouselimis
#' @details
#' This function applies gamma correction to a matrix or to a 3-dimensional array. The gamma correction controls the overall brightness of an image.
#' @export
#' @examples
#' 
#' path = system.file("tmp_images", "2.jpg", package = "OpenImageR")
#'
#' image = readImage(path)
#'
#' filt = gamma_correction(image, gamma = 0.5)
#'


gamma_correction = function(image, gamma) {

  if (is.array(image) || is.matrix(image)) {

    out = ((image ) ^ (1 / gamma))}

  else {

    stop('the image should be either a matrix or an array')
  }

  return(out)
}



#' secondary function for downsampling
#'
#' @keywords internal


sec_gaus_bl = function(image, factor, sigma, range_gauss) {

  kernel_size = ifelse(factor == 2, 3, round( 3 * (factor / 2)))

  gaus_kern = gaussian_kernel(kernel_size, sigma = sigma, range_gauss = range_gauss)

  image = conv2d(image, gaus_kern, 'same')        # convolve

  return(image)
}



#' downsampling an image ( by a factor ) using gaussian blur
#'
#' @param image matrix or 3-dimensional array
#' @param factor a positive number greater or equal to 1.0
#' @param gaussian_blur a boolean (TRUE,FALSE) specifying if gaussian blur should be applied when downsampling
#' @param gauss_sigma float parameter sigma for the gaussian filter
#' @param range_gauss float number specifying the range of values for the gaussian filter
#' @return depending on the input, either a matrix or an array
#' @author Lampros Mouselimis
#' @details
#' This function downsamples an image with the option to use gaussian blur for optimal output.
#' @export
#' @examples
#' 
#' path = system.file("tmp_images", "2.jpg", package = "OpenImageR")
#'
#' image = readImage(path)
#'
#' dsamp = down_sample_image(image, factor = 2.0, gaussian_blur = TRUE)
#'


down_sample_image = function(image, factor, gaussian_blur = FALSE, gauss_sigma = 1.0, range_gauss = 2) {
  
  if (!is.logical(gaussian_blur)) stop("'gaussian_blur should be one of TRUE, FALSE")
  if (is.data.frame(image)) image = as.matrix(image)
  if (!class(image) %in% c('matrix', 'array')) stop('invalid type of image, use either array or matrix')
  if (factor < 1.0) stop('factor should be greater or equal to 1.0')

  new_rows = seq(1, nrow(image), factor)

  new_cols = seq(1, ncol(image), factor)

  if (gaussian_blur) {        # use a gaussian kernel to perform gaussian blurring

    if (is.array(image) && !is.na(dim(image)[3]) && dim(image)[3] == 3) {

      new_array = list()

      for (i in 1:dim(image)[3]) {

        new_array[[i]] = sec_gaus_bl(matrix(image[,,i], dim(image)[1], dim(image)[2]), factor, gauss_sigma, range_gauss)
      }

      new_array = array(unlist(new_array), dim = c(nrow(new_array[[1]]), ncol(new_array[[1]]), length(new_array)))

      out = new_array[new_rows, new_cols, ]
    }
    
    if (is.matrix(image)) {

      new_array = sec_gaus_bl(image, factor, gauss_sigma, range_gauss)

      out = new_array[new_rows, new_cols]
    }
  }

  else {

    if (class(image) == 'array') {

      out = image[new_rows, new_cols, ]
    }

    if (class(image) == 'matrix') {

      out = image[new_rows, new_cols]
    }
  }

  return(out)
}



#' crop an image in R  [ for RGB or grey images ]
#'
#' @keywords internal


crop_image_secondary = function(image, new_width, new_height) {         # reduce image size for 'equal_spaced'

  r = nrow(image);
  c = ncol(image);

  if (r <= new_width || c <= new_height) { stop("new_height or new_width should be less than or equal to the initial dimensions of the image") }

  dif_rows = r - new_width
  dif_cols = c - new_height

  rem_rows = dif_rows %% 2
  rem_cols = dif_cols %% 2

  remov_rows = (floor(dif_rows/2) + 1):(r - floor(dif_rows/2) - rem_rows)
  remov_cols = (floor(dif_cols/2) + 1):(c - floor(dif_cols/2) - rem_cols)

  return(image[remov_rows, remov_cols])

}


#' crop an image
#'
#' @param image matrix or 3-dimensional array
#' @param new_width Corresponds to the image-rows. If 'equal_spaced' then the new_width should be numeric of length 1. If 'user_defined' then the new_width should be a sequence of numeric values.
#' @param new_height Corresponds to the image-columns. If 'equal_spaced' then the new_height should be numeric of length 1. If 'user_defined' then the new_height should be a sequence of numeric values.
#' @param type a string specifying the type ('equal_spaced' or 'user_defined'). If 'equal_spaced' the image will be cropped towards the center (equal distances horizontaly and verticaly). If 'user_defined' the user specifies the cropped region.
#' @return depending on the input, either a matrix or an array
#' @author Lampros Mouselimis
#' @details
#' This function crops an image in two different ways.
#' @export
#' @examples
#' 
#' path = system.file("tmp_images", "2.jpg", package = "OpenImageR")
#'
#' image = readImage(path)
#'
#' # IF 'equal_spaced':
#' crop1 = cropImage(image, new_width = 20, new_height = 20, type = 'equal_spaced')
#'
#' # IF 'user_defined':
#' crop2 = cropImage(image, new_width = 5:20, new_height = 5:20, type = 'user_defined')
#'


cropImage = function(image, new_width, new_height, type = 'equal_spaced') {

  if (is.null(new_width) || is.null(new_height) || !class(new_height) %in% c('numeric', 'integer') || !class(new_width) %in% c('numeric', 'integer')) stop('new_height and new_width should be of type numeric')
  if (!type %in% c('equal_spaced', 'user_defined') || is.null(type)) stop('valid types are equal_spaced and user_defined')
  if (type == 'equal_spaced' && (length(new_height) != 1 || length(new_width) != 1)) stop('if the type is equal_spaced then the new_height and new_width should be numeric of length 1')
  if (type == 'user_defined' && (length(new_height) < 2 || length(new_width) < 2)) stop('if the type is user_defined then the new_height and new_width should be a sequence of numeric values')

  if (type == 'equal_spaced') {

    if (is.matrix(image)) {

      res = crop_image_secondary(image, new_width, new_height)}

    else if (is.array(image) && !is.na(dim(image)[3]) && dim(image)[3] == 3) {

      res = array(, dim = c(new_width, new_height, dim(image)[3]))

      for (i in 1:dim(image)[3]) {

        res[,, i] = crop_image_secondary(image[,, i], new_width, new_height)
      }
    }

    else {

      stop('invalid type of image, supported types are matrix and 3 dimensional array')
    }
  }

  if (type == 'user_defined') {

    if (is.matrix(image)) {

      res = image[new_width, new_height]}

    else if (is.array(image) && !is.na(dim(image)[3]) && dim(image)[3] == 3) {

      res = array(, dim = c(length(new_width), length(new_height), dim(image)[3]))

      for (i in 1:dim(image)[3]) {

        res[,, i] = image[new_width, new_height, i]
      }
    }

    else {

      stop('invalid type of image, supported types are matrix and 3 dimensional array')
    }
  }

  return(res)
}




#' Rotate an image using the 'nearest' or 'bilinear' method
#'
#'
#' Rotate an image by angle using the 'nearest' or 'bilinear' method
#'
#' @param image matrix, data frame or 3-dimensional array
#' @param angle specifies the number of radians
#' @param method a string specifying the interpolation method when rotating an image ( 'nearest', 'bilinear' )
#' @param mode one of 'full', 'same' (same indicates that the ouput image will have the same dimensions with initial image) 
#' @param threads the number of cores to run in parallel
#' @return depending on the input, either a matrix or an array
#' @details
#' This function rotates an image by a user-specified angle
#' @export
#' @examples
#' 
#' path = system.file("tmp_images", "2.jpg", package = "OpenImageR")
#'
#' image = readImage(path)
#'
#' r = rotateImage(image, 75, threads = 1)
#'


rotateImage = function(image, angle, method = 'nearest', mode = 'same', threads = 1) {
  
  if (threads < 1) stop('threads should be greater than 0')
  if (angle > 360.0 || angle < 0.0) stop("valid angles to rotate an image are values greater than 0 and less than 360")
  if (!method %in% c('nearest', 'bilinear')) stop("valid methods are 'nearest', 'bilinear'")
  if (!mode %in% c('same', 'full')) stop("invalid mode, choose one of 'same', 'full'")
  
  if (class(image) == 'data.frame') image = as.matrix(image)
  
  if (class(image) == 'matrix') {
    
    out = rotate_nearest_bilinear(image, angle, method, mode, threads)}
  
  else if (class(image) == 'array' && !is.na(dim(image)[3]) && dim(image)[3] == 3) {
    
    if (mode == 'same') {
      
      out = rotate_nearest_bilinear_array_same(image, angle, method, threads)}
    
    if (mode == 'full') {
      
      out = rotate_nearest_bilinear_array_full(image, angle, method, threads)
    }
  }
  
  else {
    
    stop('invalid type of image, supported types are matrix, data frame and 3 dimensional array')
  }
  
  return(out)
}


#' Rotate an image by 90, 180, 270 degrees
#'
#' @param image matrix, data frame or 3-dimensional array
#' @param angle one of 90, 180 and 270 radians
#' @return depending on the input, either a matrix or an array
#' @details
#' This function is faster than the rotateImage function as it rotates an image for specific angles (90, 180 or 270 radians).
#' @export
#' @examples
#' 
#' path = system.file("tmp_images", "3.jpeg", package = "OpenImageR")
#'
#' image = readImage(path)
#'
#' r = rotateFixed(image, 90)
#'


rotateFixed = function(image, angle) {

  if (class(image) == 'data.frame') image = as.matrix(image)

  if (class(image) == 'matrix') {

    out = rotate_rcpp(image, angle)}

  else if (class(image) == 'array' && !is.na(dim(image)[3]) && dim(image)[3] == 3) {

    new_array = list()

    for (i in 1:dim(image)[3]) {

      new_array[[i]] = rotate_rcpp(matrix(image[,,i], dim(image)[1], dim(image)[2]), angle)
    }

    out = array(unlist(new_array), dim = c(nrow(new_array[[1]]), ncol(new_array[[1]]), length(new_array)))
  }

  else {

    stop('invalid type of image, supported types are matrix and 3 dimensional array')
  }

  return(out)
}



#' secondary function for 'resizeImage'  [ array ]
#'
#' @keywords internal

sec_resiz_array = function(image, flag = T) {

  if (flag) {

    if (max(apply(image, 3, max)) <= 1.0) {

      image = image * 255
    }
  }

  else {

    if (max(as.vector(image)) <= 1.0) {

      image = image * 255
    }
  }

  return(image)
}



#' resize an image using the 'nearest neighbors' or the 'bilinear' method
#'
#' @param image matrix or 3-dimensional array
#' @param width a number specifying the new width of the image. Corresponds to the image-rows.
#' @param height a number specifying the new height of the image. Corresponds to the image-columns.
#' @param method one of 'nearest', 'bilinear'
#' @return depending on the input, either a matrix or an array
#' @author Lampros Mouselimis
#' @details
#' This function down- or upsamples an image using the 'nearest neighbors' or the 'bilinear' method
#' @export
#' @examples
#' 
#' path = system.file("tmp_images", "2.jpg", package = "OpenImageR")
#'
#' image = readImage(path)
#'
#' resiz = resizeImage(image, width = 32, height = 32, method = 'nearest')
#'

resizeImage = function(image, width, height, method = 'nearest') {

  if (is.data.frame(image)) image = as.matrix(image)
  if (width < 1.0) stop("width should be at least 1.0")
  if (height < 1.0) stop("height should be at least 1.0")
  if (!method %in% c('nearest', 'bilinear')) stop("valid methods are 'nearest' or 'bilinear'")

  if (is.array(image) && !is.na(dim(image)[3]) && dim(image)[3] == 3) {

    image = sec_resiz_array(image, T)

    if (method == 'nearest') {

      out = resize_nearest_array(image, width, height)}

    if (method == 'bilinear') {

      out = bilinear_array(image, width, height)
    }
  }

  else if (is.matrix(image)) {

    image = sec_resiz_array(image, F)

    if (method == 'nearest') {

      out = resize_nearest_rcpp(image, width, height)}

    if (method == 'bilinear') {

      out = resize_bilinear_rcpp(image, width, height)
    }
  }

  else {

    stop('invalid type of image, use either a matrix, data frame or array')
  }

  return(out/255.0)
}



#' flip image horizontally or vertically
#'
#' flip an image row-wise (horizontally) or column-wise (vertically)
#'
#' @param image a matrix, data frame or 3-dimensional array
#' @param mode one of 'horizontal', 'vertical'
#' @return a matrix or 3-dimensional array
#' @details
#' This function flips an image row-wise or column-wise
#' @export
#' @examples
#'
#' path = system.file("tmp_images", "1.png", package = "OpenImageR")
#'
#' im = readImage(path)
#'
#' flp = flipImage(im, mode = 'vertical')
#'


flipImage = function(image, mode = 'horizontal') {

  if (mode == 'vertical') {
    mode = 1}
  else if (mode == 'horizontal') {
    mode = 2}
  else {
    stop('invalid mode')
  }

  if (is.data.frame(image)) image = as.matrix(image)

  if (is.matrix(image)) {

    res = im_flip(image, mode)}

  else if (is.array(image) && !is.na(dim(image)[3]) && dim(image)[3] == 3) {

    res = im_flip_cube(image, mode)}

  else {

    stop('valid types of input are matrix, data frame and 3-dimensional array')
  }

  return(res)
}



#' zca whiten of an image
#'
#'
#' this function performs zca-whitening to a 2- or 3- dimensional image
#' @param image a matrix, data frame or 3-dimensional array
#' @param k an integer specifying the number of components to keep when svd is performed (reduced dimension representation of the data)
#' @param epsilon a float specifying the regularization parameter
#' @return a matrix or 3-dimensional array
#' @details
#' Whitening (or sphering) is the preprocessing needed for some algorithms. If we are training on images, the raw input is redundant, since adjacent
#' pixel values are highly correlated. When using whitening the features become less correlated and all features have the same variance.
#' @references 
#' http://ufldl.stanford.edu/wiki/index.php/Whitening
#' @export
#' @examples
#' 
#' path = system.file("tmp_images", "1.png", package = "OpenImageR")
#'
#' image = readImage(path)
#'
#' res = ZCAwhiten(image, k = 20, epsilon = 0.1)
#'

ZCAwhiten = function(image, k, epsilon) {

  if (epsilon <= 0) stop('epsilon should be greater than 0')

  if (is.data.frame(image)) image = as.matrix(image)

  if (is.matrix(image)) {

    if (k < 1 || k > ncol(image)) stop('k should be greater or equal to 1 and less than ncol(image) + 1')

    res = zca_whitening(image, k, epsilon)}

  else if (is.array(image) && !is.na(dim(image)[3]) && dim(image)[3] == 3) {

    if (k < 1 || k > ncol(image[,,1])) stop('k should be greater or equal to 1 and less than ncol(image) + 1 of each array slice')

    res = zca_whiten_cube(image, k, epsilon)}

  else {

    stop('valid types of input are matrix, data frame and 3-dimensional array')
  }

  return(res)
}



#' Delation or Erosion of an image
#'
#'
#' this function performs delation or erosion to a 2- or 3- dimensional image
#' @param image a matrix, data frame or 3-dimensional array
#' @param Filter a vector specifying the dimensions of the kernel, which will be used to perform either delation or erosion, such as c(3,3)
#' @param method one of 'delation', 'erosion'
#' @param threads number of cores to run in parallel ( > 1 should be used if image high dimensional )
#' @return a matrix or 3-dimensional array
#' @details
#' This function utilizes a kernel to perform delation or erosion. The first value of the vector indicates the number of rows of the kernel, whereas 
#' the second value indicates the number of columns.
#' @export
#' @examples
#' 
#' path = system.file("tmp_images", "1.png", package = "OpenImageR")
#'
#' image = readImage(path)
#'
#' res_delate = delationErosion(image, Filter = c(3,3), method = 'delation')
#' 
#' res_erode = delationErosion(image, Filter = c(5,5), method = 'erosion')
#'


delationErosion = function(image, Filter, method = 'delation', threads = 1) {
  
  if (is.data.frame(image)) image = as.matrix(image)
  if (!class(image) %in% c('matrix', 'array')) stop('invalid type of image, use either array or matrix')
  if (threads < 1) stop('theads should be at least 1')
  if (!method %in% c('delation', 'erosion')) stop("invalid method, choose one of 'delation', 'erosion'")
  if (length(Filter) != 2 || Filter[1] < 1 || Filter[2] < 1 || Filter[1] > nrow(image) - 1 || Filter[2] > ncol(image) - 1 || class(Filter) != 'numeric') 
    stop('Filter should be a numeric vector, such as c(3,3), where each value of the vector is greater than 1 and less than the number of 
         rows and columns of the image')
  
  if (method == 'delation') {
    method = 1}
  if (method == 'erosion') {
    method = 2
  }
  
  if (is.matrix(image)) {

    res = diate_erode(image, Filter, method, threads)}
  
  if (is.array(image) && !is.na(dim(image)[3]) && dim(image)[3] == 3) {
    
    res = diate_erode_cube(image, Filter, method, threads)
  }
  
  return(res)
}



#' image translation
#'
#'
#' shift the position of an image by adding/subtracting a value to/from the X or Y coordinates
#' @param image a matrix, data frame or 3-dimensional array
#' @param shift_rows a positive or negative integer specifying the direction that the rows should be shifted
#' @param shift_cols a positive or negative integer specifying the direction that the columns should be shifted
#' @param padded_value either a numeric value or a numeric vector of length 3 (corresponding to RGB). If it's not equal to 0 then the values of the shifted rows or columns will be filled with the user-defined padded_value
#' @return a matrix or 3-dimensional array
#' @details
#' If shift_rows is not zero then the image will be sifted row-wise (upsides or downsides depending on the sign). If shift_cols is not zero then 
#' the image will be sifted column-wise (right or left depending on the sign).
#' @export
#' @examples
#' 
#' path = system.file("tmp_images", "1.png", package = "OpenImageR")
#'
#' image = readImage(path)
#'
#' res_tr = translation(image, shift_rows = 10, shift_cols = -10)
#' 



translation = function(image, shift_rows = 0, shift_cols = 0, padded_value = 0) {
  
  if (is.data.frame(image)) image = as.matrix(image)
  if (shift_rows == 0 && shift_cols == 0) stop("one of shift_rows, shift_cols should be non zero")

  if (class(image) == 'matrix') {
    
    if (length(padded_value) != 1) {
     
      stop("the padded_value parameter should be a numeric value", call. = F) 
    }
    
    out = translation_mat(image, shift_rows, shift_cols, padded_value)}                 # padded_value is a numeric value
  
  else if (is.array(image) && !is.na(dim(image)[3]) && dim(image)[3] == 3) { 
    
    if (length(padded_value) == 1) {
      
      padded_value = rep(padded_value, 3)}                                               # here by default arrays are 3-dimensional (padded_value vector of length 3)
    
    else {
      
      if (length(padded_value) > 1 && length(padded_value) != 3) {
        
        stop("the padded_value parameter should be a vector of length 3", call. = F)
      }
    }
    
    out = list()
    
    for (i in 1:3) {
      
      out[[i]] = translation_mat(image[,,i], shift_rows, shift_cols, padded_value[i])
    }
    
    out = list_2array_convert(out)
  }
  
  else {
    
    stop('invalid data type, use one of matrix, data frame or array')
  }
  
  return(out)
}



#' convert a list of matrices to an array of matrices
#' 
#' @param data a list of matrices
#' @param verbose if TRUE then the time taken to complete the task will be printed
#' @return an array
#' @author Lampros Mouselimis
#' @details
#' This is a helper function mainly for the HOG and hash functions. In case that matrices are stored in a list, 
#' this function converts the list to an array of 2-dimensional data.
#' @export
#' @examples
#' 
#' lst = list(matrix(0, 100, 100), matrix(1, 100, 100))
#' 
#' arr = List_2_Array(lst, verbose = FALSE)
#'


List_2_Array = function(data, verbose = FALSE) {
  
  if (class(data) != 'list') stop('the data should be a list') 
  if (sum(unlist(lapply(data, is.matrix))) != length(data)) stop('the sublists should be of type matrix')
  if (sum(unlist(lapply(data, nrow)) == nrow(data[[1]])) != length(data) || sum(unlist(lapply(data, ncol)) == ncol(data[[1]])) != length(data)) stop('dimension mismatch of columns or rows of the sublists')
  
  if (verbose) {
    
    start = Sys.time()
    
    out = list_2array_convert(data)
    
    end = Sys.time()
    
    t = end - start
    
    cat('\n'); cat('time to complete :', t, attributes(t)$units, '\n'); cat('\n');}
  
  else {
    
    out = list_2array_convert(data)
  }
  
  return(out)
}



#' image augmentations of a matrix, data frame, array or a list of 3-dimensional arrays
#' 
#' 
#' @param image a matrix, data frame, array or list of 3-dimensional arrays
#' @param flip_mode a character string ('horizontal', 'vertical')
#' @param crop_width an integer specifying the new width of the image, after the image is cropped. Corresponds to the image-rows.
#' @param crop_height an integer specifying the new height of the image, after the image is cropped. Corresponds to the image-columns.
#' @param resiz_width an integer specifying the new width of the image, after the image is resized. Corresponds to the image-rows.
#' @param resiz_height an integer specifying the new height of the image, after the image is resized. Corresponds to the image-columns.
#' @param resiz_method a string specifying the interpolation method when resizing an image ('nearest', 'bilinear')
#' @param shift_rows a positive or negative integer specifying the direction that the rows should be shifted
#' @param shift_cols a positive or negative integer specifying the direction that the columns should be shifted
#' @param rotate_angle an integer specifying the rotation angle of the image
#' @param rotate_method a string specifying the interpolation method when rotating an image ('nearest', 'bilinear')
#' @param zca_comps an integer specifying the number of components to keep by zca whitening, when svd is performed 
#' @param zca_epsilon a float specifying the regularization parameter by zca whitening
#' @param image_thresh the threshold parameter, by image thresholding, should be between 0 and 1 if the data is normalized or between 0-255 otherwise
#' @param padded_value either a numeric value or a numeric vector of length equal to N of an N-dimensional array. If it's not equal to 0 then the values of the shifted rows or columns will be filled with the user-defined padded_value. Applies only to the shift_rows and shift_cols parameters.
#' @param verbose a boolean (TRUE, FALSE). If TRUE, then the total time of the preprocessing task will be printed.
#' @return the output is of the same type with the input (in case of a data frame it returns a matrix)
#' @author Lampros Mouselimis
#' @details
#' This function takes advantage of various methods to accomplish image augmentations. The order of the preprocessing steps, in case that all transformations are applied to an image,
#' is : 1st flip image, 2nd crop image, 3rd resize image, 4th shift rows or columns, 5th rotate image, 6th zca-whitening and 7th image-thresholding.
#' @export
#' @examples
#' 
#' \dontrun{
#' 
#' # a matrix
#' object = matrix(1, 10, 10)
#' 
#' res = Augmentation(object, resiz_width = 8, resiz_height = 8, rotate_angle = 40)
#' 
#' 
#' # an array
#' object = array(0, dim = c(10, 10, 3))
#' 
#' res = Augmentation(object, resiz_width = 8, resiz_height = 8, rotate_angle = 30)
#' 
#' 
#' # an array (multiple matrices)
#' object = array(0, dim = c(10, 10, 10))
#' 
#' res = Augmentation(object, resiz_width = 8, resiz_height = 8, rotate_angle = 20)
#' 
#' 
#' # a list of 3-dimensional arrays
#' object = list(array(0, dim = c(10, 10, 3)), array(0, dim = c(10, 10, 3)))                
#' 
#' res = Augmentation(object, resiz_width = 8, resiz_height = 8, rotate_angle = 40)
#' }


Augmentation = function(image, flip_mode = NULL, crop_width = NULL, crop_height = NULL, resiz_width = 0, resiz_height = 0, resiz_method = "nearest", shift_rows = 0,
                        
                        shift_cols = 0, rotate_angle = 0, rotate_method = "nearest", zca_comps = 0, zca_epsilon = 0.0, image_thresh = 0.0, padded_value = 0, verbose = FALSE) {
  
  
  if (!class(image) %in% c('data.frame', 'matrix', 'array', 'list')) stop('the image parameter should be either a matrix, data frame, array or a list')
  if ((length(crop_height) < 2 && !is.null(crop_height)) || (length(crop_width) < 2 && !is.null(crop_width))) stop('crop_height and crop_width should be a sequence of numeric values')
  if (length(resiz_width) != 1 || length(resiz_height) != 1) stop('resiz_height and resiz_width should be a single value')
  if (!resiz_method %in% c('nearest', 'bilinear')) stop("valid resizing methods are 'nearest', 'bilinear'")
  if (rotate_angle > 360.0 || rotate_angle < 0.0) stop("valid angles to rotate an image are values greater than 0 and less than 360")
  if (!rotate_method %in% c('nearest', 'bilinear')) stop("valid rotation methods are 'nearest', 'bilinear'")
  if (class(image) == 'data.frame') image = as.matrix(image)
  if (ncol(image) <= length(crop_height) && class(image) %in% c('data.frame', 'matrix', 'array')) stop("the length of the crop_height sequence should be less than the initial height of the image")
  if (nrow(image) <= length(crop_width) && class(image) %in% c('data.frame', 'matrix', 'array')) stop("the length of the crop_width sequence should be less than the initial width of the image")
  flag_comps = 0
  if (inherits(image, c('data.frame', 'matrix', 'array'))) {
    flag_comps = dim(image)[1] - 1}
  if (class(image) == "list") {
    flag_comps = dim(image[[1]])[1] - 1
  }
  if (!is.null(crop_width)) flag_comps = length(crop_width)
  if (resiz_width != 0) flag_comps = resiz_width
  if (zca_comps > flag_comps) stop(paste('zca_comps should be greater or equal to 1 and less than or equal to', flag_comps, sep = ' '))
  if (is.null(crop_height)) crop_height = numeric(0)
  if (is.null(crop_width)) crop_width = numeric(0)
  if (!is.null(flip_mode) && !flip_mode %in% c("horizontal", "vertical")) stop("valid flip_mode is one of 'horizontal', 'vertical'")
  if (is.null(flip_mode)) flip_mode = ""
  
  if (verbose) start = Sys.time()
  
  if (class(image) == 'matrix') {
    
    if (length(padded_value) != 1) {
      
      stop("the padded_value parameter should be a numeric value", call. = F) 
    }
    
    out = augment_transf(image, flip_mode, crop_height, crop_width, resiz_width, resiz_height, resiz_method, shift_rows, shift_cols, rotate_angle, rotate_method, 
                         
                         zca_comps, zca_epsilon, image_thresh, padded_value)}
  
  if (class(image) == 'array' && !is.na(dim(image)[3])) {             # here arrays can be >= 3-dimensional ( besides RGB-images also an array of matrices )
    
    if (length(padded_value) == 1) {
      
      padded_value = rep(padded_value, dim(image)[3])}
    
    else {
      
      if (length(padded_value) > 1 && length(padded_value) != dim(image)[3]) {
        
        stop(paste0("the padded_value parameter should be a vector of length equal to ", dim(image)[3]), call. = F)
      }
    }
    
    out = augment_transf_array(image, flip_mode, crop_height, crop_width, padded_value, resiz_width, resiz_height, resiz_method, shift_rows, shift_cols, rotate_angle, 
                               
                               rotate_method, zca_comps, zca_epsilon, image_thresh)}
  
  if (class(image) == 'list') {
    
    if ( sum(unlist(lapply(image, function(x) class(x) == 'array' && dim(x)[3] == 3))) != length(image) ) stop('the list should consist of 3-dimensional arrays')
    if ( sum(unlist(lapply(image, function(x) nrow(x) <= length(crop_width)))) ) stop('the length of the crop_width sequence should be less than or equal to the initial width of each of the images')
    if ( sum(unlist(lapply(image, function(x) ncol(x) <= length(crop_height)))) ) stop('the length of the crop_height sequence should be less than or equal to the initial height of each of the images')
    
    if (length(padded_value) == 1) {
      
      padded_value = rep(padded_value, 3)}
    
    else {
      
      if (length(padded_value) > 1 && length(padded_value) != 3) {
        
        stop("the padded_value parameter should be a vector of length 3", call. = F)
      }
    }
    
    out = augment_array_list(image, flip_mode, crop_height, crop_width, padded_value, resiz_width, resiz_height, resiz_method, shift_rows, shift_cols, rotate_angle, rotate_method, 
                             
                             zca_comps, zca_epsilon, image_thresh)
  }
  
  if (verbose) {
    
    end = Sys.time()
    
    t = end - start
    
    cat('\n'); cat('time to complete :', t, attributes(t)$units, '\n'); cat('\n');
  }
  
  return(out)
}



#' normalize a vector, matrix or array (in the range between 0 and 1)
#'
#' @param x either a vector, matrix, data frame or array
#' @return either a normalized vector, matrix, or array
#' @author Lampros Mouselimis
#' @details
#' This is a helper function which normalizes all pixel values of the object to the range between 0 and 1. The function takes either a vector, matrix, data frame or 
#' array as input and returns a normalized object of the same type (in case of data frame it returns a matrix).
#' @export
#' @examples
#' 
#' # vector
#' x = 1:10
#' 
#' res = NormalizeObject(x)
#' 
#' 
#' # matrix
#' x = matrix(runif(100), 10, 10)
#' 
#' res = NormalizeObject(x)
#' 
#' 
#' # data frame
#' x = data.frame(matrix(runif(100), 10, 10))
#' 
#' res = NormalizeObject(x)
#' 
#' 
#' # array
#' x = array(runif(100), dim = c(10, 10, 3))               
#' 
#' res = NormalizeObject(x)
#'

NormalizeObject = function(x) {
  
  if (is.data.frame(x)) x = as.matrix(x)
  
  if (is.vector(x, mode = 'numeric')) {
    
    out = (x - min(x))/(max(x) - min(x))}
  
  else if (class(x) == 'matrix') {
    
    out = Normalize_matrix(x)}
  
  else if (is.array(x) && !is.na(dim(x)[3]) && dim(x)[3] == 3) { 
    
    out = Normalize_array(x)}
  
  else {
    
    stop('invalid data type, use one of vector, matrix, data frame or array')
  }
  
  return(out)
}



#' minimum and maximum values of vector, matrix, data frame or array
#'
#' @param x either a vector, matrix, data frame or array
#' @return a list
#' @author Lampros Mouselimis
#' @details
#' This helper function returns the minimum and maximum values of a vector, 2-dimensional or 3-dimensional objects. In case of a vector, matrix or data frame it returns a single value for 
#' the minimum and maximum of the object. In case of an array it returns the minimum and maximum values for each slice of the array.
#' @export
#' @examples
#' 
#' # vector
#' x = 1:10
#' 
#' res = MinMaxObject(x)
#' 
#' 
#' # matrix
#' x = matrix(runif(100), 10, 10)
#' 
#' res = MinMaxObject(x)
#' 
#' 
#' # data frame
#' x = data.frame(matrix(runif(100), 10, 10))
#' 
#' res = MinMaxObject(x)
#' 
#' 
#' # array
#' x = array(runif(100), dim = c(10, 10, 3))               
#' 
#' res = MinMaxObject(x)
#'


MinMaxObject = function(x) {
  
  if (is.data.frame(x)) x = as.matrix(x)
  
  if (is.vector(x, mode = 'numeric')) {
    
    out = list(min = min(x), max = max(x))}
  
  else if (class(x) == 'matrix') {
    
    out = MinMaxMatrix(x)}
  
  else if (is.array(x) && !is.na(dim(x)[3]) && dim(x)[3] == 3) { 
    
    out = MinMaxArray(x)}
  
  else {
    
    stop('invalid data type, use one of vector, matrix, data frame or array')
  }
  
  return(out)
}


#' convolution
#'
#' @param image either a matrix, data frame or array
#' @param kernel a kernel in form of a matrix
#' @param mode the convolution mode (one of 'same', 'full')
#' @return either a matrix or an array, depending on the input data
#' @author Lampros Mouselimis
#' @details
#' This function performs convolution using a kernel matrix. When mode 'same' the output object has the same dimensions with the input, whereas
#' when mode 'full' the rows and columns of the output object equals : ROWS = nrow(image) + nrow(kernel) - 1 and COLUMNS = ncol(image) + ncol(kernel) - 1
#' @export
#' @examples
#' 
#' 
#' # kernel
#' x = matrix(1, nrow = 4, ncol = 4) / 16   # uniform
#' 
#' 
#' # matrix
#' image_matrix = matrix(runif(100), 10, 10)
#' 
#' res = convolution(image_matrix, x, "same")
#' 
#' 
#' # array
#' image_array = array(runif(100), dim = c(10, 10, 3))
#' 
#' res = convolution(image_array, x, "same")
#' 



convolution = function(image, kernel, mode = "same") {
  
  if (class(kernel) != 'matrix') stop('the kernel should be a matrix')
  if (!mode %in% c('same', 'full')) stop('the mode should one of same, full')
  if (class(image) == 'data.frame') image = as.matrix(image)
  
  if (class(image) == 'matrix') {
    
    out = conv2d(image, kernel, mode)}
  
  else if (class(image) == 'array' && !is.na(dim(image)[3]) && dim(image)[3] == 3) {
    
    out = conv3d(image, kernel, mode)}
  
  else {
    
    stop('invalid type of image, choose one of data frame, matrix or 3 dimensional array')
  }
  
  return(out)
}

