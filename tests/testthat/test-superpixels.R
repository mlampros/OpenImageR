
context("superpixels")



#-----------------------
# 'superpixels' function
#-----------------------

testthat::test_that("the 'slic' function returns the expected output", {

  path = system.file("tmp_images", "slic_im.png", package = "OpenImageR")
  
  im = readImage(path)

  res = superpixels(input_image = im, method = "slic", superpixel = 200, compactness = 20,
                    return_slic_data = T, return_lab_data = T,
                    return_labels = T, write_slic = "", verbose = FALSE)
  
  nams = c("slic_data", "labels", "lab_data")
  
  testthat::expect_true( names(res) %in% nams && all(unlist(lapply(res, function(x) length(as.vector(x)) > 0))) )
})



testthat::test_that("the 'slico' function returns the expected output", {

  path = system.file("tmp_images", "slic_im.png", package = "OpenImageR")
  
  im = readImage(path)
  
  res = superpixels(input_image = im, method = "slico", superpixel = 200, compactness = 20,
                    return_slic_data = T, return_lab_data = T,
                    return_labels = T, write_slic = "", verbose = FALSE)
  
  nams = c("slic_data", "labels", "lab_data")
  
  testthat::expect_true( names(res) %in% nams && all(unlist(lapply(res, function(x) length(as.vector(x)) > 0))) )
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

