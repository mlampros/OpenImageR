context('Rotate image')


# rotateFixed function  [ rotates an image for 90, 180, 270 degrees ]


testthat::test_that("in case that image is not an array, matrix or data frame it returns an error", {
  
  image_array = list(image_array = matrix(runif(25), 5, 5))
  
  testthat::expect_error( rotateFixed(image_array, angle = 90) )
})

testthat::test_that("in case that the angle is not 90, 180, 270 it returns an error", {
  
  image_array = matrix(runif(25), 5, 5)
  
  testthat::expect_error( rotateFixed(image_array, angle = 100) )
})


# test different angles [ for matrix/data.frame ]

testthat::test_that("in case of an angle it returns a matrix", {
  
  image_array = matrix(runif(25), 5, 5)
  
  testthat::expect_true(is.matrix(rotateFixed(image_array, angle = 90)))
})

testthat::test_that("in case of an angle it returns a matrix", {
  
  image_array = matrix(runif(25), 5, 5)
  
  testthat::expect_true(is.matrix(rotateFixed(image_array, angle = 180)))
})


testthat::test_that("in case of an angle it returns a matrix", {
  
  image_array = data.frame(matrix(runif(25), 5, 5))
  
  testthat::expect_true(is.matrix(rotateFixed(image_array, angle = 270)))
})


# test different angles [ for array ]

testthat::test_that("in case of an angle it returns a array", {
  
  image_array = array(runif(100), dim = c(10,10,3))
  
  testthat::expect_true(is.array(rotateFixed(image_array, angle = 90)))
})

testthat::test_that("in case of an angle it returns a array", {
  
  image_array = array(runif(100), dim = c(10,10,3))
  
  testthat::expect_true(is.array(rotateFixed(image_array, angle = 180)))
})


testthat::test_that("in case of an angle it returns a array", {
  
  image_array = array(runif(100), dim = c(10,10,3))
  
  testthat::expect_true(is.array(rotateFixed(image_array, angle = 270)))
})


# rotateImage function     [ rotate image in a range between 0.0 and 360.0 degrees ]

 
testthat::test_that("in case of a gray image (2-dimensional) when the angle value is below 0.0 degrees it returns an error", {
  
  image_array =  matrix(runif(30), 5, 5)
  
  testthat::expect_error( rotateImage(image_array, -1, threads = 1) )
})


testthat::test_that("in case of a gray image (2-dimensional) when the angle value is greater than 360.0 degrees it returns an error", {
  
  image_array =  matrix(runif(30), 5, 5)
  
  testthat::expect_error( rotateImage(image_array, 360.5, threads = 1) )
})


testthat::test_that("in case of a gray image (2-dimensional) if the input image is invalid it returns an error", {
  
  image_array =  list(matrix(runif(30), 5, 5))
  
  testthat::expect_error( rotateImage(image_array, 60, threads = 1) )
})


testthat::test_that("in case of a gray image (2-dimensional) if the number of threads is less than 1 it returns an error", {
  
  image_array =  matrix(runif(30), 5, 5)
  
  testthat::expect_error( rotateImage(image_array, 60, threads = 0) )
})


testthat::test_that("in case of a gray image (2-dimensional) if the method is invalid it returns an error", {
  
  image_array =  matrix(runif(30), 5, 5)
  
  testthat::expect_error( rotateImage(image_array, 60, method = 'invalid', threads = 1) )
})


testthat::test_that("in case of a gray image (2-dimensional) if the mode is invalid it returns an error", {
  
  image_array =  matrix(runif(30), 5, 5)
  
  testthat::expect_error( rotateImage(image_array, 60, mode = 'invalid', threads = 1) )
})

testthat::test_that("in case of a gray image (2-dimensional) if the mode is invalid it returns an error", {
  
  image_array = array(runif(100), dim = c(10,10,3))
  
  testthat::expect_error( rotateImage(image_array, 60, mode = 'invalid', threads = 1) )
})

testthat::test_that("in case of a gray image (2-dimensional) it returns a matrix with the same dimensions of the initial image, 20 degrees", {
  
  image_array =  matrix(runif(30), 5, 5)
  
  res = rotateImage(image_array, 20, method = 'nearest', mode = 'full', threads = 1)
  
  testthat::expect_true(is.matrix(res) && nrow(res) != nrow(image_array) && ncol(res) != ncol(image_array))
})

testthat::test_that("in case of a gray image (2-dimensional) it returns a matrix with the same dimensions of the initial image, 20 degrees", {
  
  image_array =  matrix(runif(30), 5, 5)
  
  res = rotateImage(image_array, 20, method = 'nearest', mode = 'same', threads = 1)
  
  testthat::expect_true(is.matrix(res) && nrow(res) == nrow(image_array) && ncol(res) == ncol(image_array))
})


testthat::test_that("in case of a gray image (2-dimensional) it returns a matrix with the same dimensions of the initial image, 20 degrees", {
  
  image_array =  matrix(runif(30), 5, 5)
  
  res = rotateImage(image_array, 20, method = 'bilinear', mode = 'full', threads = 1)
  
  testthat::expect_true(is.matrix(res) && nrow(res) != nrow(image_array) && ncol(res) != ncol(image_array))
})

testthat::test_that("in case of a gray image (2-dimensional) it returns a matrix with the same dimensions of the initial image, 20 degrees", {
  
  image_array =  matrix(runif(30), 5, 5)
  
  res = rotateImage(image_array, 20, method = 'bilinear', mode = 'same', threads = 1)
  
  testthat::expect_true(is.matrix(res) && nrow(res) == nrow(image_array) && ncol(res) == ncol(image_array))
})

testthat::test_that("in case of a gray image (2-dimensional) it returns a matrix with the same dimensions of the initial image, 90 degrees", {
  
  image_array =  matrix(runif(30), 5, 5)
  
  res = rotateImage(image_array, 90, threads = 1)
  
  testthat::expect_true(is.matrix(res) && nrow(res) == nrow(image_array) && ncol(res) == ncol(image_array))
})


testthat::test_that("in case of an RGB image (3-dimensional) it returns a 3-dimensional array", {
  
  image_array = array(runif(100), dim = c(10,10,3))
  
  res = rotateImage(image_array, 20, method = 'nearest', mode = 'same', threads = 1)
  
  testthat::expect_true(is.array(res) && dim(res)[1] == dim(image_array)[1] && dim(res)[2] == dim(image_array)[2] && dim(res)[3] == dim(image_array)[3])
})

testthat::test_that("in case of an RGB image (3-dimensional) it returns a 3-dimensional array", {
  
  image_array = array(runif(100), dim = c(10,10,3))
  
  res = rotateImage(image_array, 20, method = 'nearest', mode = 'full', threads = 1)
  
  testthat::expect_true(is.array(res) && dim(res)[1] != dim(image_array)[1] && dim(res)[2] != dim(image_array)[2] && dim(res)[3] == dim(image_array)[3])
})

testthat::test_that("in case of an RGB image (3-dimensional) it returns a 3-dimensional array", {
  
  image_array = array(runif(100), dim = c(10,10,3))
  
  res = rotateImage(image_array, 20, method = 'bilinear', mode = 'same', threads = 1)
  
  testthat::expect_true(is.array(res) && dim(res)[1] == dim(image_array)[1] && dim(res)[2] == dim(image_array)[2] && dim(res)[3] == dim(image_array)[3])
})

testthat::test_that("in case of an RGB image (3-dimensional) it returns a 3-dimensional array", {
  
  image_array = array(runif(100), dim = c(10,10,3))
  
  res = rotateImage(image_array, 20, method = 'bilinear', mode = 'full', threads = 1)
  
  testthat::expect_true(is.array(res) && dim(res)[1] != dim(image_array)[1] && dim(res)[2] != dim(image_array)[2] && dim(res)[3] == dim(image_array)[3])
})