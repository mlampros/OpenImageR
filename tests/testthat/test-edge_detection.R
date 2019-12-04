context('Edge detection function')


testthat::test_that("in case of an array input (image) the edge detection function returns an array", {
  
  image_array = array(unlist(list(matrix(runif(25), 5, 5), matrix(runif(25), 5, 5), matrix(runif(25), 5, 5))), dim = c(5, 5, 3))

  testthat::expect_true(is.array(edge_detection(image_array, method = 'LoG', conv_mode = 'same', approx = T, gaussian_dims = 5, sigma = 1.0, range_gauss = 2, laplacian_type = 1)))
})

testthat::test_that("in case of a matrix input (image) the edge detection function returns a matrix", {
  
  image_array = matrix(runif(25), 5, 5)
  
  testthat::expect_true( inherits(edge_detection(image_array, method = 'LoG', conv_mode = 'same', approx = F, gaussian_dims = 5, sigma = 1.0, range_gauss = 2, laplacian_type = 1), 'matrix') )
})


testthat::test_that("in case of a NULL method returns an error", {
  
  image_array = matrix(runif(25), 5, 5)
  
  testthat::expect_error(edge_detection(image_array, method = NULL, conv_mode = 'same', approx = F, gaussian_dims = 5, sigma = 1.0, range_gauss = 2, laplacian_type = 1))
})


testthat::test_that("in case of an unknown method returns an error", {
  
  image_array = matrix(runif(25), 5, 5)
  
  testthat::expect_error(edge_detection(image_array, method = 'some_unknown_method', conv_mode = 'same', approx = T, gaussian_dims = 5, sigma = 1.0, range_gauss = 2, laplacian_type = 1))
})

testthat::test_that("in case that the 'conv_mode' argument is NULL it returns an error", {
  
  image_array = matrix(runif(25), 5, 5)
  
  testthat::expect_error(edge_detection(image_array, method = 'LoG', conv_mode = NULL, approx = F, gaussian_dims = 5, sigma = 1.0, range_gauss = 2, laplacian_type = 1))
})


testthat::test_that("in case that the 'conv_mode' argument is not one of 'same', 'full' it returns an error", {
  
  image_array = matrix(runif(25), 5, 5)
  
  testthat::expect_error(edge_detection(image_array, method = 'LoG', conv_mode = 'invalid', approx = F, gaussian_dims = 5, sigma = 1.0, range_gauss = 2, laplacian_type = 1))
})


testthat::test_that("in case that the 'approx' argument is not a boolean it returns an error", {
  
  image_array = matrix(runif(25), 5, 5)
  
  testthat::expect_error(edge_detection(image_array, method = 'LoG', conv_mode = 'same', approx = NULL, gaussian_dims = 5, sigma = 1.0, range_gauss = 2, laplacian_type = 1))
})

testthat::test_that("in case that the image-data is not a matrix or an array it returns an error", {
  
  image_array = list(matrix(runif(25), 5, 5))
  
  testthat::expect_error(edge_detection(image_array, method = 'LoG', conv_mode = 'same', approx = F, gaussian_dims = 5, sigma = 1.0, range_gauss = 2, laplacian_type = 1))
})


# test various kernel filter


testthat::test_that("in case of an array input (image) the edge detection function returns an array", {
  
  image_array = array(unlist(list(matrix(runif(25), 5, 5), matrix(runif(25), 5, 5), matrix(runif(25), 5, 5))), dim = c(5, 5, 3))
  
  testthat::expect_true(is.array(edge_detection(image_array, method = 'Sobel', conv_mode = 'same', approx = F, gaussian_dims = 5, sigma = 1.0, range_gauss = 2, laplacian_type = 1)))
})

testthat::test_that("in case of an array input (image) the edge detection function returns an array", {
  
  image_array = array(unlist(list(matrix(runif(25), 5, 5), matrix(runif(25), 5, 5), matrix(runif(25), 5, 5))), dim = c(5, 5, 3))
  
  testthat::expect_true(is.array(edge_detection(image_array, method = 'Prewitt', conv_mode = 'same', approx = F, gaussian_dims = 5, sigma = 1.0, range_gauss = 2, laplacian_type = 1)))
})

testthat::test_that("in case of an matrix input (image) the edge detection function returns a matrix", {
  
  image_array = matrix(runif(25), 5, 5)
  
  testthat::expect_true( inherits(edge_detection(image_array, method = 'Roberts_cross', conv_mode = 'same', approx = T, gaussian_dims = 5, sigma = 1.0, range_gauss = 2, laplacian_type = 1), 'matrix') )
})


testthat::test_that("in case of an data frame input (image) the edge detection function returns a matrix", {
  
  image_array = as.data.frame(matrix(runif(25), 5, 5))
  
  testthat::expect_true( inherits(edge_detection(image_array, method = 'Roberts_cross', conv_mode = 'full', approx = T, gaussian_dims = 5, sigma = 1.0, range_gauss = 2, laplacian_type = 1), 'matrix') )
})

testthat::test_that("in case of an array input (image) the edge detection function returns an array", {
  
  image_array = array(unlist(list(matrix(runif(25), 5, 5), matrix(runif(25), 5, 5), matrix(runif(25), 5, 5))), dim = c(5, 5, 3))
  
  testthat::expect_true(is.array(edge_detection(image_array, method = 'Frei_chen', conv_mode = 'same', approx = F, gaussian_dims = 5, sigma = 1.0, range_gauss = 2, laplacian_type = 1)))
})


testthat::test_that("in case of an array input (image) the edge detection function returns an array", {
  
  image_array = array(unlist(list(matrix(runif(25), 5, 5), matrix(runif(25), 5, 5), matrix(runif(25), 5, 5))), dim = c(5, 5, 3))
  
  testthat::expect_true(is.array(edge_detection(image_array, method = 'Scharr', conv_mode = 'full', approx = T, gaussian_dims = 5, sigma = 1.0, range_gauss = 2, laplacian_type = 1)))
})
