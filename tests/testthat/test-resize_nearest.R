context("Resize image")


testthat::test_that("in case that the method is not valid it returns an error", {

  height = width = 15

  image_array = matrix(runif(100), 10, 10)

  testthat::expect_error(resizeImage(image_array, width, height, method = 'invalid'))
})


testthat::test_that("in case of a data frame the resizeImage function returns a matrix with dimensions equal to the height, width", {

  height = width = 15

  image_array = as.data.frame(matrix(runif(100), 10, 10))

  matr = resizeImage(image_array, width, height)

  testthat::expect_true(is.matrix(matr) && nrow(matr) == width && ncol(matr) == width)
})


testthat::test_that("in case of a matrix the resizeImage function returns a matrix with dimensions equal to the height, width", {

  height = width = 15

  image_array = matrix(runif(100), 10, 10)

  matr = resizeImage(image_array, width, height, method = 'nearest')

  testthat::expect_true(is.matrix(matr) && nrow(matr) == width && ncol(matr) == width)
})


testthat::test_that("in case of an array the resizeImage function returns an array with rows and cols of each array equal to the height, width", {

  height = width = 15

  image_array = array(unlist(list(matrix(runif(100), 10, 10), matrix(runif(100), 10, 10), matrix(runif(100), 10, 10))), dim = c(10, 10, 3))

  matr = resizeImage(image_array, width, height, method = 'nearest')

  testthat::expect_true(is.array(matr) && mean(apply(matr, 3, nrow)) == width && mean(apply(matr, 3, ncol)) == width)
})


testthat::test_that("in case of a matrix the resizeImage function returns a matrix with dimensions equal to the height, width", {

  height = width = 15

  image_array = matrix(runif(100), 10, 10)

  matr = resizeImage(image_array, width, height, method = 'bilinear')

  testthat::expect_true(is.matrix(matr) && nrow(matr) == width && ncol(matr) == width)
})


testthat::test_that("in case of an array the resizeImage function returns an array with rows and cols of each array equal to the height, width", {

  height = width = 15

  image_array = array(unlist(list(matrix(runif(100), 10, 10), matrix(runif(100), 10, 10), matrix(runif(100), 10, 10))), dim = c(10, 10, 3))

  matr = resizeImage(image_array, width, height, method = 'bilinear')

  testthat::expect_true(is.array(matr) && mean(apply(matr, 3, nrow)) == width && mean(apply(matr, 3, ncol)) == width)
})


testthat::test_that("in case of an invalid type of image the function returns an error", {

  height = width = 15

  image_array = list(matrix(runif(25), 5, 5))

  testthat::expect_error(resizeImage(image_array, width, height))
})


testthat::test_that("in case of width less than 1.0 the function returns an error", {

  height = 20
  width = 0.5

  image_array = as.data.frame(matrix(runif(100), 10, 10))

  testthat::expect_error(resizeImage(image_array, width, height))
})


testthat::test_that("in case of height less than 1.0 the function returns an error", {

  height = 0.5
  width = 20

  image_array = as.data.frame(matrix(runif(100), 10, 10))

  testthat::expect_error(resizeImage(image_array, width, height))
})



testthat::test_that("the resizeImage() function gives multiple warnings in case of a 3-band image and method 'nearest'", {

  ROWS = 644         # for the dimensions that give the warning see the following issue: https://github.com/mlampros/OpenImageR/issues/21
  COLS = 969
  BANDS = 3
  resize_w_h = 8

  image_array = array(runif(ROWS * COLS * BANDS), dim = c(ROWS, COLS, BANDS))

  testthat::expect_warning(resizeImage(image = image_array, width = resize_w_h, height = resize_w_h, method = 'nearest'))
})


testthat::test_that("the resizeImage() function gives warnings in case of a single-band image and method 'nearest", {

  ROWS = 644        # for the dimensions that give the warning see the following issue: https://github.com/mlampros/OpenImageR/issues/21
  COLS = 969
  resize_w_h = 8

  image_matrix = matrix(runif(ROWS * COLS), nrow = ROWS, ncol = COLS)

  testthat::expect_warning(resizeImage(image_matrix, width = resize_w_h, height = resize_w_h, method = 'nearest'))
})

