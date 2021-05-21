context("Crop image")

# crop_image_secondary function

testthat::test_that("the crop_image_secondary function returns an error if height and width is greater than the initial dimensions of the image", {

  out_array = matrix(runif(100), 10, 10)

  testthat::expect_error(crop_image_secondary(out_array, 11, 11))
})


testthat::test_that("the crop_image_secondary function returns the correct dimensions of the output image", {

  out_array = matrix(runif(100), 10, 10)

  height = 5
  width = 5

  res = crop_image_secondary(out_array, height, width)

  testthat::expect_true(dim(res)[2] == height && dim(res)[1] == width)
})

# cropImage function


testthat::test_that("the cropImage function returns an error if height, width are not given", {

  out_array = matrix(runif(100), 10, 10)

  height = NULL
  width = NULL

  testthat::expect_error(cropImage(out_array, width, height, type = 'equal_spaced'))
})


testthat::test_that("the cropImage function returns an error if the type is incorrect", {

  out_array = matrix(runif(100), 10, 10)

  height = 5
  width = 5

  testthat::expect_error(cropImage(out_array, width, height, type = NULL))
})


testthat::test_that("the cropImage function returns an error if the type is incorrect", {

  out_array = matrix(runif(100), 10, 10)

  height = 5
  width = 5

  testthat::expect_error(cropImage(out_array, width, height, type = 'NULL'))
})


testthat::test_that("the cropImage function returns an error if height and width are not are not of type numeric", {

  out_array = matrix(runif(100), 10, 10)

  height = list(1, 5)
  width = list(1, 5)

  testthat::expect_error(cropImage(out_array, width, height, type = 'equal_spaced'))
})


testthat::test_that("the cropImage function returns an error if height and width are not are not of type numeric", {

  out_array = matrix(runif(100), 10, 10)

  height = matrix(5)
  width = matrix(5)

  testthat::expect_error(cropImage(out_array, width, height, type = 'equal_spaced'))
})


testthat::test_that("the cropImage function returns an error if height and width are not are not of type numeric", {

  out_array = matrix(runif(100), 10, 10)

  height = data.frame(5)
  width = data.frame(5)

  testthat::expect_error(cropImage(out_array, width, height, type = 'equal_spaced'))
})


testthat::test_that("the cropImage function returns an error if the type is equal_spaced and height and width are not of length 1", {

  out_array = matrix(runif(100), 10, 10)

  height = c(1:5)
  width = c(1:5)

  testthat::expect_error(cropImage(out_array, width, height, type = 'equal_spaced'))
})


testthat::test_that("the cropImage function returns an error if the type is user_defined and height and width are of length 1", {

  out_array = matrix(runif(100), 10, 10)

  height = 5
  width = 5

  testthat::expect_error(cropImage(out_array, width, height, type = 'user_defined'))
})


testthat::test_that("the cropImage function returns an error if the type is user_defined and data is not an array or matrix", {

  out_array = list(matrix(runif(100), 10, 10))

  height = 1:5
  width = 1:5

  testthat::expect_error(cropImage(out_array, width, height, type = 'user_defined'))
})


testthat::test_that("the cropImage function returns an error if the type is equal_spaced and data is not an array or matrix", {

  out_array = list(matrix(runif(100), 10, 10))

  height = 5
  width = 5

  testthat::expect_error(cropImage(out_array, width, height, type = 'equal_spaced'))
})


testthat::test_that("the cropImage function returns a matrix with the correct dimensions if the type is equal_spaced ", {

  out_array = matrix(runif(100), 10, 10)

  height = 5
  width = 5

  res = cropImage(out_array, width, height, type = 'equal_spaced')

  testthat::expect_true(is.matrix(res) && ncol(res) == height && nrow(res) == width)
})


testthat::test_that("the cropImage function returns a matrix with the correct dimensions if the type is user_defined ", {

  out_array = matrix(runif(100), 10, 10)

  height = 1:5
  width = 1:6

  res = cropImage(out_array, width, height, type = 'user_defined')

  testthat::expect_true(is.matrix(res) && ncol(res) == length(height) && nrow(res) == length(width))
})


testthat::test_that("the cropImage function returns a matrix with the correct dimensions if the type is equal_spaced ", {

  out_array = array(runif(300), dim = c(10, 10, 3))

  height = 5
  width = 5

  res = cropImage(out_array, width, height, type = 'equal_spaced')

  testthat::expect_true(is.array(res) && mean(apply(res, 3, ncol)) == height && mean(apply(res, 3, nrow)) == width)
})


testthat::test_that("the cropImage function returns a matrix with the correct dimensions if the type is user_defined ", {

  out_array = array(runif(300), dim = c(10, 10, 3))

  height = 1:5
  width = 1:6

  res = cropImage(out_array, width, height, type = 'user_defined')

  testthat::expect_true(is.array(res) && mean(apply(res, 3, ncol)) == length(height) && mean(apply(res, 3, nrow)) == length(width))
})
