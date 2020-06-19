
context("superpixels")



#-----------------------
# 'superpixels' function
#-----------------------

testthat::test_that("the 'slic' function returns the expected output ( for 3-dimensional data )", {

  path = system.file("tmp_images", "slic_im.png", package = "OpenImageR")

  im = readImage(path)

  res = superpixels(input_image = im, method = "slic", superpixel = 200, compactness = 20,
                    return_slic_data = T, return_lab_data = T,
                    return_labels = T, write_slic = "", verbose = FALSE)

  nams = c("slic_data", "labels", "lab_data")

  testthat::expect_true( all(names(res) %in% nams) && all(unlist(lapply(res, function(x) length(as.vector(x)) > 0))) )
})



testthat::test_that("the 'slico' function returns the expected output ( for 3-dimensional data )", {

  path = system.file("tmp_images", "slic_im.png", package = "OpenImageR")

  im = readImage(path)

  res = superpixels(input_image = im, method = "slico", superpixel = 200, compactness = 20,
                    return_slic_data = T, return_lab_data = T,
                    return_labels = T, write_slic = "", verbose = FALSE)

  nams = c("slic_data", "labels", "lab_data")

  testthat::expect_true( all(names(res) %in% nams) && all(unlist(lapply(res, function(x) length(as.vector(x)) > 0))) )
})


testthat::test_that("the 'slic' function returns the expected output ( for 2-dimensional data )", {

  path = system.file("tmp_images", "slic_im.png", package = "OpenImageR")

  im = readImage(path)

  im = im[,,1]

  res = superpixels(input_image = im, method = "slic", superpixel = 200, compactness = 20,
                    return_slic_data = T, return_lab_data = T,
                    return_labels = T, write_slic = "", verbose = FALSE)

  nams = c("slic_data", "labels", "lab_data")

  testthat::expect_true( all(names(res) %in% nams) && all(unlist(lapply(res, function(x) length(as.vector(x)) > 0))) && inherits(res$slic_data, 'matrix') && inherits(res$lab_data, 'matrix') )
})



testthat::test_that("the 'slico' function returns the expected output ( for 2-dimensional data )", {

  path = system.file("tmp_images", "slic_im.png", package = "OpenImageR")

  im = readImage(path)

  im = im[,,1]

  res = superpixels(input_image = im, method = "slico", superpixel = 200, compactness = 20,
                    return_slic_data = T, return_lab_data = T,
                    return_labels = T, write_slic = "", verbose = FALSE)

  nams = c("slic_data", "labels", "lab_data")

  testthat::expect_true( all(names(res) %in% nams) && all(unlist(lapply(res, function(x) length(as.vector(x)) > 0))) && inherits(res$slic_data, 'matrix') && inherits(res$lab_data, 'matrix') )
})


testthat::test_that("the 'slic' function returns the expected output ( for 2-dimensional data )", {

  path = system.file("tmp_images", "slic_im.png", package = "OpenImageR")

  im = readImage(path)

  im = im[,,1]

  res = superpixels(input_image = im, method = "slic", superpixel = 200, compactness = 20,
                    return_slic_data = F, return_lab_data = F,
                    return_labels = F, write_slic = "", verbose = FALSE)

  nams = c("slic_data", "labels", "lab_data")

  testthat::expect_true( all(names(res) %in% nams) && all(unlist(lapply(res, function(x) length(as.vector(x)) == 0))) )
})


testthat::test_that("the 'slic' function returns an error if the input data is not a matrix or an array", {

  path = system.file("tmp_images", "slic_im.png", package = "OpenImageR")

  im = readImage(path)

  testthat::expect_error( superpixels(input_image = list(im), method = "slic", superpixel = 200, compactness = 20,
                                      return_slic_data = F, return_lab_data = F,
                                      return_labels = F, write_slic = "", verbose = FALSE) )
})


#---------------------------
# 'superpixel_bbox' function
#---------------------------

testthat::test_that("the 'superpixel_bbox' function returns the bounding box for each unique superpixel label", {

  path = system.file("tmp_images", "slic_im.png", package = "OpenImageR")

  im = readImage(path)

  res = superpixels(input_image = im, method = "slico", superpixel = 200, compactness = 20,
                    return_slic_data = F, return_lab_data = F,
                    return_labels = T, write_slic = "", verbose = FALSE)

  bbox = superpixel_bbox(res$labels, non_overlapping_superpixels = FALSE)

  unq_lbs = unique(as.vector(res$labels))

  testthat::expect_true( inherits(bbox, 'matrix') && ncol(bbox) == 7 && nrow(bbox) == length(unq_lbs) )
})



#----------------------------------
# 'superpixel_bbox_subset' function
#----------------------------------

testthat::test_that("the 'superpixel_bbox_subset' function returns the bounding box for a subset of superpixel segments", {

  path = system.file("tmp_images", "slic_im.png", package = "OpenImageR")

  im = readImage(path)

  res = superpixels(input_image = im, method = "slico", superpixel = 200, compactness = 20,
                    return_slic_data = F, return_lab_data = F,
                    return_labels = T, write_slic = "", verbose = FALSE)

  sbs = c(0, 10, 20, 30, 50)

  bbox = superpixel_bbox_subset(res$labels, superpixel_subset = sbs)

  testthat::expect_true( inherits(bbox, 'matrix') && ncol(bbox) == 6 && nrow(bbox) == 1 )
})



#--------------------
# 'padding' function
#--------------------

testthat::test_that("the 'padding' function returns the correct output for a matrix", {

  mt = matrix(runif(100), 10, 10)

  res_mt = padding(mt, 15, 20, fill_value = -1)

  testthat::expect_true( inherits(res_mt, 'list') && nrow(res_mt$data) == 15 && ncol(res_mt$data) == 20 && (-1 %in% as.vector(res_mt$data)) && all(names(res_mt) %in% c("data", "padded_start", "padded_end", "padded_left", "padded_right")) )
})


testthat::test_that("the 'padding' function returns the correct output for an array", {

  lst = list(matrix(1, 10, 10), matrix(2, 10, 10))

  arr = List_2_Array(lst, verbose = FALSE)

  MEAN = mean(as.vector(runif(10)))

  res_arr = padding(arr, 15, 20, fill_value = MEAN)

  testthat::expect_true( inherits(res_arr, 'list') && nrow(res_arr$data) == 15 && ncol(res_arr$data) == 20 && dim(res_arr$data)[3] == dim(arr)[3] && (MEAN %in% as.vector(res_arr$data)) && all(names(res_arr) %in% c("data", "padded_start", "padded_end", "padded_left", "padded_right")) )
})


testthat::test_that("the 'padding' function gives an error if the input data is not a matrix or array", {

  lst = list(matrix(1, 10, 10), matrix(2, 10, 10))

  testthat::expect_error( padding(lst, 15, 20, fill_value = 0.0) )
})


testthat::test_that("the 'padding' function gives an error if the new rows or new columns are smaller than the dimensions of the input data", {

  mt = matrix(1, 10, 10)

  testthat::expect_error( padding(mt, 8, 12, fill_value = 0.0) )
})


#----------------------
# 'RGB_to_Lab' function
#----------------------

testthat::test_that("the 'RGB_to_Lab' function returns the expected output", {

  set.seed(1)
  im = array(sample(1:255, 675, replace = TRUE), c(15, 15, 3))
  res = RGB_to_Lab(im)
  init_dat = as.vector(im)
  vec_res = as.vector(res)

  testthat::expect_true( max(init_dat) != max(vec_res) && min(init_dat) != min(vec_res) && median(init_dat) != median(vec_res)  )
})


#----------------------
# 'RGB_to_HSV' function
#----------------------

testthat::test_that("the 'RGB_to_HSV' function returns the expected output", {

  set.seed(1)
  im = array(sample(1:255, 675, replace = TRUE), c(15, 15, 3))
  res = RGB_to_HSV(im)
  init_dat = as.vector(im)
  vec_res = as.vector(res)

  testthat::expect_true( max(init_dat) != max(vec_res) && min(init_dat) != min(vec_res) && median(init_dat) != median(vec_res)  )
})

