

#' Get Affine Transform
#'
#' @param original_points a matrix object that corresponds to the original points
#' @param transformed_points a matrix object that corresponds to the transformed points
#' @return a matrix
#' @references
#' https://github.com/OlehOnyshchak/ImageTransformations/blob/master/AffineTransformation.ipynb
#' @export
#' @examples
#'
#' require(OpenImageR)
#'
#' r = 600
#' c = 600
#' offset = 50
#'
#' original_points = matrix(data = c(0, 0, r, 0, 0, c),
#'                          nrow = 3,
#'                          ncol = 2,
#'                          byrow = TRUE)
#'
#' transformed_points = matrix(data = c(offset, 0, r, offset, 0, c-offset),
#'                             nrow = 3,
#'                             ncol = 2,
#'                             byrow = TRUE)
#'
#' M_aff = getAffineTransform(original_points = original_points,
#'                            transformed_points = transformed_points)
#' M_aff

getAffineTransform = function(original_points, transformed_points) {
  return(get_affine_transform(original_points, transformed_points))
}


#' Warp Affine
#'
#' @param img either a matrix or a 3-dimensional array with a range of values between 0 and 255
#' @param M a matrix corresponding to the transformation matrix
#' @param R a value corresponding to the destination number of rows
#' @param C a value corresponding to the destination number of columns
#' @param threads an integer specifying the number of threads to run in parallel. This parameter applies only if the input "img" parameter is of type matrix.
#' @param verbose a boolean. If TRUE then information will be printed in the console
#' @return either a matrix or a 3-dimensional array
#' @references
#' https://github.com/OlehOnyshchak/ImageTransformations/blob/master/AffineTransformation.ipynb
#' @export
#' @examples
#'
#' require(OpenImageR)
#'
#' path = system.file("tmp_images", "landscape.jpg", package = "OpenImageR")
#' img = readImage(path)
#' img = img * 255
#'
#' #.............................
#' # compute the affine transform
#' #.............................
#'
#' r = ncol(img)
#' c = nrow(img)
#' offset = 50
#'
#' original_points = matrix(data = c(0, 0, r, 0, 0, c),
#'                          nrow = 3,
#'                          ncol = 2,
#'                          byrow = TRUE)
#'
#' transformed_points = matrix(data = c(offset, 0, r, offset, 0, c-offset),
#'                             nrow = 3,
#'                             ncol = 2,
#'                             byrow = TRUE)
#'
#' M_aff = getAffineTransform(original_points = original_points,
#'                            transformed_points = transformed_points)
#'
#' #..............
#' # 2-dimensional
#' #..............
#'
#' img_2d = rgb_2gray(img)
#'
#' res_2d = warpAffine(img = img_2d,
#'                     M = M_aff,
#'                     R = r,
#'                     C = c,
#'                     threads = 1,
#'                     verbose = TRUE)
#'
#' # imageShow(res_2d)
#'
#' #..............
#' # 3-dimensional
#' #..............
#'
#' res_3d = warpAffine(img = img,
#'                     M = M_aff,
#'                     R = r,
#'                     C = c,
#'                     verbose = TRUE)
#'
#' # imageShow(res_3d)
#'

warpAffine = function(img, M, R, C, threads = 1, verbose = FALSE) {

  if (verbose) t_start = Sys.time()
  if (!inherits(img, c('matrix', 'array'))) stop("The input 'img' parameter must be either a 'matrix' or '3-dimensional array'!", call. = F)

  if (inherits(img, 'matrix')) {
    res_ = warpAffine_2d(img, M, R, C, threads)
  }
  else {
    if (dim(img)[3] != 3) stop("In case that the input parameter 'img' is of type array it has to be 3-dimensional!", call. = F)
    res_ = warp_affine_3d(img, M, R, C)
  }

  if (verbose) {
    t_end = Sys.time()
    t = t_end - t_start
    cat(paste(c('time to complete :', round(t, digits = 4), attributes(t)$units), collapse = ' '), '\n')
  }

  return(res_)
}

