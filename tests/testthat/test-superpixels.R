
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

