context("Create gaussian kernel")


testthat::test_that("it returns a matrix", {
  
  testthat::expect_true(is.matrix(gaussian_kernel(4)))
})


testthat::test_that("the image dimensions match the 'xy_length' argument", {
  
  xy_length = 5
  
  tmp = dim(gaussian_kernel(xy_length))
  
  testthat::expect_true(all(tmp == xy_length))
})


testthat::test_that("the 'xy_length' argument is an integer greater than 0", {
  
  xy_length = 0
  
  testthat::expect_error(gaussian_kernel(xy_length))
})


testthat::test_that("the 'range_gauss' argument is greater or equal to 0.0", {
  
  xy_length = 5
  
  range_gauss = 0.0
  
  testthat::expect_error(gaussian_kernel(xy_length, range_gauss = range_gauss))
})