#=======================================================================================================

#' calculation of the 'average hash' of an image
#'
#' This function calculates the average hash of an image
#'
#' @param gray_image a (2-dimensional) matrix or data frame
#' @param hash_size an integer specifying the hash size (should be less than number of rows or columns of the gray_image)
#' @param MODE one of 'hash' (returns the hash of the image), 'binary' (returns binary identifier of the image)
#' @param resize corresponds to one of 'nearest', 'bilinear' (resizing method)
#' @return either a hash-string or a binary vector
#' @details
#' The function is a modification of the 'average_hash' function of the imagehash package [ please consult the COPYRIGHT file ]. The average hash works
#' in the following way : 1st convert to grayscale, 2nd, reduce the size of an image (for instance to an 8x8 image, to further simplify
#' the number of computations), 3rd average the resulting colors (for an 8x8 image we average 64 colors), 4th compute the bits by comparing if each color value
#' is above or below the mean, 5th construct the hash.
#' @export
#' @examples
#'
#' image = readImage(system.file("tmp_images", "1.png", package = "OpenImageR"))
#'
#' image = rgb_2gray(image)
#'
#' res_hash = average_hash(image, hash_size = 8, MODE = 'hash')
#'
#' res_binary = average_hash(image, hash_size = 8, MODE = 'binary')
#'


average_hash = function(gray_image, hash_size = 8, MODE = 'hash', resize = 'nearest') {

  if (!resize %in% c('nearest', 'bilinear')) stop('invalid resizing method')
  if (length(dim(gray_image)) != 2 || sum(dim(gray_image)) == 0) stop('the image should be a non empty 2-dimensional matrix or data frame)')
  if (hash_size >= nrow(gray_image) || hash_size >= ncol(gray_image)) stop('the hash size should be less than the original dimensions of the image')

  if (MODE == 'hash') {

    diff = average_hash_string(gray_image, hash_size = hash_size, resize_method = resize)
    out = binary_to_hex(diff)}

  if (MODE == 'binary') {

    out = average_hash_binary(gray_image, hash_size = hash_size, resize_method = resize)
  }

  return(out)
}


#' calculation of the 'phash' of an image
#'
#'
#' This function calculates the phash of an image
#'
#' @param gray_image a (2-dimensional) matrix or data frame
#' @param hash_size an integer specifying the hash size (hash_size * highfreq_factor should be less than number of rows or columns of the gray_image)
#' @param highfreq_factor an integer specyfing the highfrequency factor (hash_size * highfreq_factor should be less than number of rows or columns of the gray_image)
#' @param MODE one of 'hash' (returns the hash of the image), 'binary' (returns binary identifier of the image)
#' @param resize corresponds to one of 'nearest', 'bilinear' (resizing method)
#' @return either a hash-string or a binary vector
#' @details
#' The function is a modification of the 'phash' function of the imagehash package [ please consult the COPYRIGHT file ]. The phash algorithm
#' extends the average_hash by using the discrete cosine transform.
#' @export
#' @examples
#'
#' image = readImage(system.file("tmp_images", "2.jpg", package = "OpenImageR"))
#'
#' image = rgb_2gray(image)
#'
#' res_hash = phash(image, hash_size = 6, highfreq_factor = 3, MODE = 'hash')
#'
#' res_binary = phash(image, hash_size = 6, highfreq_factor = 3, MODE = 'binary')
#'


phash = function(gray_image, hash_size = 8, highfreq_factor = 4, MODE = 'hash', resize = 'nearest') {

  if (!resize %in% c('nearest', 'bilinear')) stop('invalid resizing method')
  if (length(dim(gray_image)) != 2 || sum(dim(gray_image)) == 0) stop('the image should be a non empty 2-dimensional matrix or data frame)')
  if (hash_size * highfreq_factor >= nrow(gray_image) || hash_size * highfreq_factor >= ncol(gray_image)) stop('the hash_size * highfreq_factor should be less than the original dimensions of the image')

  if (MODE == 'hash') {

    diff = phash_string(gray_image, hash_size = hash_size, highfreq_factor = highfreq_factor, resize_method = resize)
    out = binary_to_hex(diff)}

  if (MODE == 'binary') {

    out = phash_binary(gray_image, hash_size = hash_size, highfreq_factor = highfreq_factor, resize_method = resize)
  }

  return(out)
}



#' calculation of the 'dhash' of an image
#'
#' This function calculates the dhash of an image
#'
#' @param gray_image a (2-dimensional) matrix or data frame
#' @param hash_size an integer specifying the hash size (should be less than number of rows or columns of the gray_image)
#' @param MODE one of 'hash' (returns the hash of the image), 'binary' (returns binary identifier of the image)
#' @param resize corresponds to one of 'nearest', 'bilinear'
#' @return either a hash-string or a binary vector
#' @details
#' The function is a modification of the 'dhash' function of the imagehash package [ please consult the COPYRIGHT file ]. In comparison to
#' average_hash and phash, the dhash algorithm takes into consideration the difference between adjacent pixels.
#' @export
#' @examples
#'
#' image = readImage(system.file("tmp_images", "3.jpeg", package = "OpenImageR"))
#'
#' image = rgb_2gray(image)
#'
#' res_hash = dhash(image, hash_size = 8, MODE = 'hash')
#'
#' res_binary = dhash(image, hash_size = 8, MODE = 'binary')
#'


dhash = function(gray_image, hash_size = 8, MODE = 'hash', resize = 'nearest') {

  if (!resize %in% c('nearest', 'bilinear')) stop('invalid resizing method')
  if (length(dim(gray_image)) != 2 || sum(dim(gray_image)) == 0) stop('the image should be a non empty 2-dimensional matrix or data frame)')
  if (hash_size + 1 >= nrow(gray_image) || hash_size + 1 >= ncol(gray_image)) stop('the hash size + 1 should be less than the original dimensions of the image')

  if (MODE == 'hash') {

    diff = dhash_string(gray_image, hash_size = hash_size, resize_method = resize)
    out = binary_to_hex(diff)}

  if (MODE == 'binary') {

    out = dhash_binary(gray_image, hash_size = hash_size, resize_method = resize)
  }

  return(out)
}



#' if-else function for hashing
#'
#' @keywords internal
#'

switch_hashing = function(object, width, height, hash_size, highfreq_factor, method, mode, threads, resize) {

  if (mode == 'binary') {

    if (inherits(object, 'matrix')) {

      out = hash_image(object, width, height, resize, hash_size, highfreq_factor, method, threads)
    }
    else if (inherits(object, 'array')) {

      out = hash_image_cube(object, resize, hash_size, highfreq_factor, method, threads)
    }
    else {
      stop("The 'object' parameter must be either of type matrix or 3-dimensional array where the third dimension is equal to 3")
    }
  }

  else if (mode == 'hash') {

    if (inherits(object, 'matrix')) {

      out = hash_image_hex(object, width, height, resize, hash_size, highfreq_factor, method, threads)
    }
    else if (inherits(object, 'array')) {

      out = hash_image_cube_hex(object, resize, hash_size, highfreq_factor, method, threads)
    }
    else {
      stop("The 'object' parameter must be either of type matrix or 3-dimensional array where the third dimension is equal to 3")
    }
  }

  else {

    stop("invalid mode, use one of 'binary', 'hash'")
  }

  return(out)
}



#' calculate the binary or the hexadecimal hash for a matrix, array or a folder of images for the average_hash, phash or dhash functions
#'
#'
#' This function takes either a matrix, array or a folder and returns either the binary hash features or the hashes (as a character vector)
#'
#' @param object a matrix, a data frame, a 3-dimensional array  (where the third dimension is equal to 3) or a path to a folder of files (images)
#' @param rows a number specifying the number of rows of the matrix
#' @param columns a number specifying the number of columns of the matrix
#' @param hash_size an integer specifying the hash size.  IF method = 'phash' : the hash_size * highfreq_factor should be less than number of rows or columns of the gray_image.
#' IF method = 'dhash' or 'average_hash' :  the hash_size should be less than number of rows or columns of the gray_image
#' @param highfreq_factor an integer specyfing the highfrequency factor (IF method = 'phash' : the hash_size * highfreq_factor should be less than number of rows or columns of the gray_image)
#' @param method one of 'phash', 'average_hash', 'dhash'
#' @param mode one of 'binary', 'hash'
#' @param threads the number of cores to run in parallel
#' @param resize corresponds to one of 'nearest', 'bilinear' (resizing method)
#' @return If the input is a matrix, data frame or array this function returns a matrix (if mode = 'binary') or a character vector (if mode = 'hex_hash'). If the input is a path to
#' a folder the function returns a list of length 2, the 1st sublist is a vector with the names of the image files (the order of the files in the vector corresponds to the order of
#' the rows of the output matrix), the 2nd sublist is a matrix (if mode = 'binary') or a character vector (if mode = 'hex_hash').
#' @details
#' This function calculates the binary hash or the hexadecimal hash for various types of objects.
#' @export
#' @examples
#'
#' path = paste0(system.file("tmp_images", "same_type", package = "OpenImageR"), '/')
#'
#' res_phash = hash_apply(path, method = 'phash', mode = 'binary')
#'


hash_apply = function(object, rows = 28, columns = 28, hash_size = 8, highfreq_factor = 3, method = 'phash',

                      mode = 'binary', threads = 1, resize = 'nearest') {

  if (method == 'phash') {
    method = 1}
  else if (method == 'average_hash') {
    method = 2}
  else if (method == 'dhash') {
    method = 3}
  else {
    stop('invalid method')
  }

  if (!resize %in% c('nearest', 'bilinear')) stop('invalid resizing method')
  if (threads < 1) stop('threads should be at least 1')
  if (inherits(object, 'data.frame')) object = as.matrix(object)

  try_err_files = inherits(tryCatch(normalizePath(object, mustWork = T), error = function(e) e), "error")

  start = Sys.time()

  if (inherits(object, "character") && try_err_files == F) {

    str_SPL = strsplit(object, "")[[1]]

    if (!str_SPL[nchar(object)] %in% c("/", "\\")) stop('the folder path should end in slash')

    lst_files = list.files(object)

    flag_type = unlist(strsplit(lst_files[1], '[.]'))[length(unlist(strsplit(lst_files[1], '[.]')))]

    if (!flag_type %in% c("png", "jpg", "jpeg", "tiff", "tif", "TIFF", "TIF")) stop('supported image types are .png, .jpeg, .jpg, .tiff (or .tif, .TIFF, .TIF)')

    tmp_lst = lapply(1:length(lst_files), function(y) func_transform(lst_files[y], object, flag_type, T))

    conv_to_array = list_2array_convert(tmp_lst)

    tmp_out = switch_hashing(conv_to_array, columns, rows, hash_size, highfreq_factor, method, mode, threads, resize)

    tmp_out = list(files = lst_files, hash = tmp_out)
  }

  else if (inherits(object, 'matrix') && try_err_files == T) {

    if (is.null(columns) || is.null(rows)) stop('give the corresponding rows and columns for each image-row of the matrix')

    tmp_out = switch_hashing(object, columns, rows, hash_size, highfreq_factor, method, mode, threads, resize)}

  else if (inherits(object, 'array') && try_err_files == T) {

    tmp_out = switch_hashing(object, columns, rows, hash_size, highfreq_factor, method, mode, threads, resize)}

  else {

    stop('valid object types are matrix, data.frame, array or a character path to folder-of-files (images)')
  }

  end = Sys.time()

  t = end - start

  cat('\n'); cat('time to complete :', t, attributes(t)$units, '\n'); cat('\n');

  return(tmp_out)
}


#' secondary function for invariant_hash
#'
#' @keywords internal

switch_invariant = function(method, gray_image, MODE, hash_size, highfreq_factor, resize) {

  if (method == 'phash') {

    out = phash(gray_image, hash_size, highfreq_factor, MODE, resize)}

  else if (method == 'average_hash') {

    out = average_hash(gray_image, hash_size, MODE, resize)}

  else if (method == 'dhash') {

    out = dhash(gray_image, hash_size, MODE, resize)}

  else {

    stop('invalid method')
  }

  return(out)
}



#' invariant hashing (caclulation of the hamming or the levenshtein distance when the image is flipped, rotated or cropped)
#'
#'
#' flip-rotate-crop an image and caclulate the hamming or the levenshtein distance for phash, average_hash, dhash
#' @param image a 2-dimensional matrix or data frame (only gray-scale images are valid)
#' @param new_image a new image to be compared with the previous input image
#' @param method one of 'phash', 'average_hash', 'dhash'
#' @param mode one of 'binary', 'hash'
#' @param hash_size an integer specifying the hash size.  IF method = 'phash' : the hash_size * highfreq_factor should be less than number of floor(rows * 0.8) or floor(columns * 0.8) of the gray_image
#' IF method = 'dhash' or 'average_hash' :  the hash_size should be less than number of floor(rows * 0.8) or floor(columns * 0.8) of the gray_image
#' @param highfreq_factor an integer specyfing the highfrequency factor (IF method = 'phash' : the hash_size * highfreq_factor should be less than number of floor(rows * 0.8) or
#' floor(columns * 0.8) of the gray_image)
#' @param resize corresponds to one of 'nearest', 'bilinear' (resizing method)
#' @param flip if TRUE the new_image will be flipped both horizontal and vertical
#' @param rotate if TRUE the new_image will be rotated for a specified angle (see angle_bidirectional)
#' @param angle_bidirectional a float specifying the angle that the images should be rotated in both directions. For instance, if angle_bidirectional = 10 then the image will be rotated for 10 and
#' 350 (360-10) degrees.
#' @param crop if TRUE the new_image will be cropped 10 or 20 percent (equally spaced horizontally and vertically)
#' @return If flip, rotate and crop are all FALSE then the function returns either the hamming distance (if mode = 'binary') or the levenshtein distance (if mode = 'hash') for the two images.
#' If any of the flip, rotate, crop is TRUE then it returns the MIN, MAX of the hamming distance (if mode = 'binary') or the MIN,MAX of the levenshtein distance (if mode = 'hash').
#' @details
#' This function performs the following transformations : flips an image (no-flip, horizonal-flip, vertical-flip), rotates an image (no-angle, angle_bidirectional, 360-angle_bidirectional) and
#' crops an image (no-crop, 10-percent-crop, 20-percent-crop). Depending on the type of mode ('binary', 'hash'), after each transformation the hamming or the levenshtein distance between the two images is calculated.
#' @export
#' @examples
#'
#' \dontrun{
#'
#' path1 = system.file("tmp_images", "1.png", package = "OpenImageR")
#'
#' path2 = system.file("tmp_images", "2.jpg", package = "OpenImageR")
#'
#' image1 = rgb_2gray(readImage(path1))
#'
#' image2 = rgb_2gray(readImage(path2))
#'
#' res1 = invariant_hash(image1, image2, hash_size = 3, flip = TRUE, crop = FALSE)
#'
#' res2 = invariant_hash(image1, image2, mode = 'hash', hash_size = 3, angle_bidirectional = 10)
#' }


invariant_hash = function(image, new_image, method = 'phash', mode = 'binary', hash_size = 8, highfreq_factor = 4, resize = 'nearest', flip = T, rotate = T, angle_bidirectional = 10, crop = T) {

  if (!is.logical(flip) || !is.logical(rotate) || !is.logical(crop)) stop('flip, rotate or crop should be of type boolean')
  if (length(dim(image)) != 2 || length(dim(new_image)) != 2) stop('convert both images to gray scale')
  if (!mode %in% c('binary', 'hash')) stop("valid mode is one of 'binary', 'hash'")
  if (rotate && (is.null(angle_bidirectional) || angle_bidirectional >= 360 || angle_bidirectional <= 0)) stop('if rotate = TRUE then the angle_bidirectional should a number greater than 0 and less than 360')

  ham_dist = function(x1, x2) { sum(x1 != x2)/length(x1) }

  if (inherits(image, 'data.frame')) image = as.matrix(image)
  if (inherits(new_image, 'data.frame')) new_image = as.matrix(new_image)

  if (crop && (floor(nrow(image) * 0.8) < hash_size * highfreq_factor || floor(ncol(image) * 0.8) < hash_size * highfreq_factor) && (method == 'phash')) stop("the value of the hash_size * highfreq_factor should be less than floor(image * 0.8) which is the max. dims for a cropped image")
  if (crop && (floor(nrow(new_image) * 0.8) < hash_size * highfreq_factor || floor(ncol(new_image) * 0.8) < hash_size * highfreq_factor) && (method == 'phash')) stop("the value of the hash_size * highfreq_factor should be less than floor(new_image * 0.8) which is the max. dims for a cropped new_image")

  if (crop && (floor(nrow(image) * 0.8) <= hash_size || floor(ncol(image) * 0.8) <= hash_size) && (method == 'average_hash')) stop("the hash size should be less than floor(image * 0.8) which is the max. dims for a cropped image")
  if (crop && (floor(nrow(new_image) * 0.8) <= hash_size || floor(ncol(new_image) * 0.8) <= hash_size) && (method == 'average_hash')) stop("the hash size should be less than floor(new_image * 0.8) which is the max. dims for a cropped new_image")

  if (crop && (floor((nrow(image)-1) * 0.8) <= hash_size || floor((ncol(image)-1) * 0.8) <= hash_size) && (method == 'dhash')) stop("the hash size should be less than floor((image-1) * 0.8) which is the max. dims for a cropped image")
  if (crop && (floor((nrow(new_image)-1) * 0.8) <= hash_size || floor((ncol(new_image)-1) * 0.8) <= hash_size) && (method == 'dhash')) stop("the hash size should be less than floor((new_image-1) * 0.8) which is the max. dims for a cropped new_image")

  image_res = switch_invariant(method, image, mode, hash_size, highfreq_factor, resize)

  tmp_lst = list()

  flag_flip = flag_rotate = flag_crop = F

  if (flip) { tmp_lst[['flip']] = c('not_flip', 'horizontal', 'vertical'); flag_flip = T}
  if (rotate) { tmp_lst[['rotate']] = c(0, angle_bidirectional, 360 - angle_bidirectional); flag_rotate = T}
  if (crop) { tmp_lst[['crop']] = c(0, 0.1, 0.2); flag_crop = T }

  if (length(tmp_lst) == 0) {

    new_image_res = switch_invariant(method, new_image, mode, hash_size, highfreq_factor, resize)

    if (mode == 'binary') { res_out = ham_dist(image_res, new_image_res) }                          # hamming distance
    if (mode == 'hash') { res_out = levenshtein_dist(image_res, new_image_res) }                    # levenshtein distance

    return(res_out)}

  else {

    exp_grid = expand.grid(tmp_lst, stringsAsFactors = F)

    tmp_binary = rep(NA, nrow(exp_grid))

    for (i in 1:nrow(exp_grid)) {

      tmp_image = new_image

      if (flag_flip && exp_grid[i, 'flip'] != 'not_flip') {

        tmp_image = flipImage(tmp_image, exp_grid[i, 'flip'])}

      if (flag_rotate && exp_grid[i, 'rotate'] != 0) {

        tmp_image = rotateImage(tmp_image, angle = exp_grid[i, 'rotate'], method = 'bilinear', threads = 1)}

      if (flag_crop && exp_grid[i, 'crop'] != 0) {

        tmp_image = cropImage(tmp_image, floor(nrow(tmp_image) * (1 - exp_grid[i, 'crop'])), floor(ncol(tmp_image) * (1 - exp_grid[i, 'crop'])), type = 'equal_spaced')
      }

      if (mode == 'binary') { tmp_binary[i] = ham_dist(image_res, switch_invariant(method, tmp_image, mode, hash_size, highfreq_factor, resize)) }                          # hamming distance
      if (mode == 'hash') { tmp_binary[i] = levenshtein_dist(image_res, switch_invariant(method, tmp_image, mode, hash_size, highfreq_factor, resize)) }                    # levenshtein distance
    }

    return(data.frame(min = min(tmp_binary), max = max(tmp_binary)))
  }
}

