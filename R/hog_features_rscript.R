
#' calculate the HOG (Histogram of oriented gradients) for an image 
#' 
#' 
#' The function is a modification of the 'findHOGFeatures' function of the SimpleCV package [ please consult the COPYRIGHT file ]
#' The function takes either an RGB (it will be converted to gray) or a gray image and returns a vector of the HOG descriptors. 
#' The main purpose of the function is to create a vector of features, which can be used in classification tasks.
#' 
#' @param image matrix or 3-dimensional array
#' @param cells the number of divisions ( cells )
#' @param orientations number of orientation bins
#' @return a numeric vector
#' @details
#' This function takes either a matrix, a data frame or a 3-dimensional array and returns a vector with the HOG-descriptors (histogram of oriented gradients).
#' @export
#' @examples
#' 
#' path = system.file("tmp_images", "1.png", package = "OpenImageR")
#' 
#' image = readImage(path)
#' 
#' res = HOG(image, cells = 3, orientations = 6)
#' 


HOG = function(image, cells = 3, orientations = 6) {
  
  if (is.data.frame(image)) image = as.matrix(image)
  
  if (is.matrix(image)) {
    
    res = hog_cpp(image, n_divs = cells, n_bins = orientations)}
  
  else if (is.array(image) && !is.na(dim(image)[3]) && dim(image)[3] == 3) {
    
    image = rgb_2gray(image)
  
    res = hog_cpp(image, n_divs = cells, n_bins = orientations)}
  
  else {
    
    stop('valid types of input are matrix, data frame and 3-dimensional array')
  }
  
  return(as.vector(res))
}



#' secondary function for HOG_apply
#'
#' @keywords internal


func_transform = function(image, folder_path, flag_type, RGB_2gray = F) {
  
  if (flag_type == "png") {
    
    img = png::readPNG(paste(folder_path, image, sep = ""))}
  
  else if (flag_type == "jpg" || flag_type == "jpeg") {
    
    img = jpeg::readJPEG(paste(folder_path, image, sep = ""))}

  else if (flag_type %in% c("tiff", "tif", "TIFF", "TIF")) {
    
    img = tiff::readTIFF(paste(folder_path, image, sep = ""))}
  
  else {
    
    stop('supported image types are .png, .jpeg, .jpg, .tiff (or .tif, .TIFF, .TIF)')
  }
  
  if (RGB_2gray) {
    
    img = rgb_2gray(img)
  }
  
  return(img)
}




#' calculate the HOG (Histogram of oriented gradients) for a matrix, array or a folder of images
#' 
#' @param object a matrix, a data frame, a 3-dimensional array or a path to a folder of files (images)
#' @param cells the number of divisions ( cells )
#' @param orientations number of orientation bins
#' @param rows a value specifying the number of rows of each image-row of the matrix (required if object is a matrix)
#' @param columns a value specifying the number of columns of each image-row of the matrix (required if object is a matrix)
#' @param threads the number of parallel cores to use
#' @return If the input is a matrix, data frame or array it returns a matrix of the hog descriptors. If the input is a path to a folder it returns a list of length 2, 
#' the 1st sublist is a vector with the names of the image files (the order of the files in the vector corresponds to the order of the rows of the output matrix),
#' the 2nd sublist is the matrix of the hog descriptors.
#' @details
#' This function takes as input either a matrix, a data frame, a 3-dimensional array or a character path to a folder of files (images). It returns the HOG-descriptors 
#' (histogram of oriented gradients) for each row (if matrix or data frame), for each array-slice (if array) or for each file (if path to a folder of images).
#' @export
#' @examples
#' 
#' MATR = matrix(runif(75), ncol = 25, nrow = 5)
#' 
#' res = HOG_apply(MATR, cells = 3, orientations = 5, rows = 5, columns = 5, threads = 1)
#' 
#' 
#' ARRAY = array(5, dim = c(10, 10, 3))
#' 
#' res = HOG_apply(ARRAY, cells = 3, orientations = 6, threads = 1)
#' 
#' 
#' FOLDER_path = paste0(system.file("tmp_images", "same_type", package = "OpenImageR"), '/')
#' 
#' res = HOG_apply(FOLDER_path, cells = 3, orientations = 6, threads = 1)
#'


HOG_apply = function(object, cells = 3, orientations = 6, rows = NULL, columns = NULL, threads = 1) {
  
  if (threads < 1) stop('threads should be at least 1')
  if (is.matrix(object) && (is.null(columns) || is.null(rows))) stop('give number of rows and columns of the matrix')
  if (cells < 1 || orientations < 1) stop("The 'cells' and 'orientations' arguments should be greater than 0")
  if (is.data.frame(object)) object = as.matrix(object)
  
  try_err_files = inherits(tryCatch(normalizePath(object, mustWork = T), error = function(e) e), "error")
  
  start = Sys.time()

  if (class(object) == "character" && try_err_files == F) {
    
    str_SPL = strsplit(object, "")[[1]]
    
    if (!str_SPL[nchar(object)] %in% c("/", "\\")) stop('the folder path should end in slash')
    
    lst_files = list.files(object)
    
    flag_type = unlist(strsplit(lst_files[1], '[.]'))[length(unlist(strsplit(lst_files[1], '[.]')))]

    if (!flag_type %in% c("png", "jpg", "jpeg", "tiff", "tif", "TIFF", "TIF")) stop('supported image types are .png, .jpeg, .jpg, .tiff (or .tif, .TIFF, .TIF)')
    
    tmp_lst = lapply(1:length(lst_files), function(y) func_transform(lst_files[y], object, flag_type, T))
    
    conv_to_array = list_2array_convert(tmp_lst)
    
    tmp_out = HOG_array(conv_to_array, n_divs = cells, n_bins = orientations, threads = threads)
    
    out = list(files = lst_files, hog = tmp_out)}
  
  else if (class(object) == 'matrix' && try_err_files == T) {
    
    if (length(object[1, ]) != columns * rows) stop('rows * columns must be equal to the length of each row')
    
    out = HOG_matrix(object, columns, rows, n_divs = cells, n_bins = orientations, threads = threads)}
  
  else if (class(object) == 'array' && try_err_files == T) {
    
    out = HOG_array(object, n_divs = cells, n_bins = orientations, threads = threads)}
  
  else {
    
    stop('valid object types are matrix, data.frame, array or a character path to folder-of-files (images)')
  }
  
  end = Sys.time()
  
  t = end - start
  
  cat('\n'); cat('time to complete :', t, attributes(t)$units, '\n'); cat('\n');
  
  return(out)
}



