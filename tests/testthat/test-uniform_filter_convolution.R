context('Uniform filter convolution')


# uniform filter

testthat::test_that("in case of an array input (image) the uniform_filter function returns an array", {
  
  image_array = array(unlist(list(matrix(runif(25), 5, 5), matrix(runif(25), 5, 5), matrix(runif(25), 5, 5))), dim = c(5, 5, 3))
  
  testthat::expect_true(is.array(uniform_filter(image_array, c(3,3), conv_mode = 'same')))
})

testthat::test_that("in case of a matrix input (image) the uniform_filter function returns a matrix", {
  
  image_array = matrix(runif(25), 5, 5)
  
  testthat::expect_true(is.matrix(uniform_filter(image_array, c(3,3), conv_mode = 'full')))
})

testthat::test_that("in case of a another type of image returns an error", {
  
  image_array = list(matrix(runif(25), 5, 5))
  
  testthat::expect_error(uniform_filter(image_array, c(3,3), conv_mode = 'same'))
})

testthat::test_that("in case that the 'size' argument is not a vector it returns an error", {
  
  image_array = matrix(runif(25), 5, 5)
  
  testthat::expect_error(uniform_filter(image_array, NULL, conv_mode = 'same'))
})

testthat::test_that("in case that the 'conv_mode' argument is NULL it returns an error", {
  
  image_array = matrix(runif(25), 5, 5)
  
  testthat::expect_error(uniform_filter(image_array, c(3,3), conv_mode = NULL))
})


testthat::test_that("in case that the 'conv_mode' argument is not one of 'same', 'full' it returns an error", {
  
  image_array = matrix(runif(25), 5, 5)
  
  testthat::expect_error(uniform_filter(image_array, c(3,3), conv_mode = 'invalid'))
})



# convolution


testthat::test_that("if image is not a data frame, matrix or array it returns an error", {
  
  image_array = list(matrix(runif(25), 5, 5))
  
  kern = matrix(1, 4, 4) / 16
  
  testthat::expect_error( convolution(image_array, kern, mode = "same") )
})


testthat::test_that("if mode is not one of same, full it returns an error", {
  
  image_array = matrix(runif(25), 5, 5)
  
  kern = matrix(1, 4, 4) / 16
  
  testthat::expect_error( convolution(image_array, kern, mode = "invalid") )
})


testthat::test_that("if the kernel is not a matrix it returns an error", {
  
  image_array = matrix(runif(25), 5, 5)
  
  kern = as.vector(matrix(1, 4, 4) / 16)
  
  testthat::expect_error( convolution(image_array, kern, mode = "same") )
})


testthat::test_that("in case of a matrix as input it returns a correct output", {
  
  image_array = matrix(runif(25), 5, 5)
  
  kern = matrix(1, 4, 4) / 16
  
  res = convolution(image_array, kern, mode = "same") 
  
  testthat::expect_true( inherits(res, 'matrix') && nrow(image_array) == nrow(res) && ncol(image_array) == ncol(res) )
})


testthat::test_that("in case of an array as input it returns a correct output", {
  
  image_array = array(unlist(list(matrix(runif(25), 5, 5), matrix(runif(25), 5, 5), matrix(runif(25), 5, 5))), dim = c(5, 5, 3))
  
  kern = matrix(1, 4, 4) / 16
  
  res = convolution(image_array, kern, mode = "same") 
  
  testthat::expect_true( inherits(res, 'array') && nrow(image_array) == nrow(res) && ncol(image_array) == ncol(res) && !is.na(dim(res)[3]) )
})


