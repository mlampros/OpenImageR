context('Gamma threshold functions')

# thresholding


testthat::test_that("in case that image is not an array or matrix it returns an error", {
  
  image_array = matrix(runif(25), 5, 5)
  
  testthat::expect_error(image_thresholding(image_array, thresh = 0.0))
})


testthat::test_that("in case that image is not an array or matrix it returns an error", {
  
  image_array = list(image_array = matrix(runif(25), 5, 5))
  
  testthat::expect_error(image_thresholding(image_array, thresh = 0.5))
})


testthat::test_that("in case that image is a data frame it returns a matrix", {
  
  image_array = as.data.frame(matrix(runif(25), 5, 5))
  
  testthat::expect_true(is.matrix(image_thresholding(image_array, thresh = 0.5)))
})

testthat::test_that("in case that image is a matrix it returns a matrix", {
  
  image_array = matrix(runif(25), 5, 5)
  
  testthat::expect_true(is.matrix(image_thresholding(image_array, thresh = 0.5)))
})

testthat::test_that("in case that image is an array it returns a matrix", {
  
  image_array = array(unlist(list(matrix(runif(25), 5, 5), matrix(runif(25), 5, 5), matrix(runif(25), 5, 5))), dim = c(5, 5, 3))
  
  testthat::expect_true(is.matrix(image_thresholding(image_array, thresh = 0.5)))
})

# gamma correction

testthat::test_that("in case that image is not an array or matrix it returns an error", {
  
  image_array = list(image_array = matrix(runif(25), 5, 5))
  
  testthat::expect_error(gamma_correction(image_array, gamma = 1.5))
})

testthat::test_that("in case that image is an array it returns an array", {
  
  image_array = array(unlist(list(matrix(runif(25), 5, 5), matrix(runif(25), 5, 5), matrix(runif(25), 5, 5))), dim = c(5, 5, 3))
  
  testthat::expect_true(is.array(gamma_correction(image_array, gamma = 1.5)))
})




