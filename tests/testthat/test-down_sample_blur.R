context('Down sample using gaussian blur')


testthat::test_that("in case of an array input (image) the down_sample_image function returns an array", {

  image_array = array(unlist(list(matrix(runif(25), 5, 5), matrix(runif(25), 5, 5), matrix(runif(25), 5, 5))), dim = c(5, 5, 3))

  testthat::expect_true(is.array(down_sample_image(image_array, 2, T)))
})


testthat::test_that("in case of an array input (image) the down_sample_image function returns an array, if gaussian_blur = FALSE", {
  
  image_array = array(unlist(list(matrix(runif(25), 5, 5), matrix(runif(25), 5, 5), matrix(runif(25), 5, 5))), dim = c(5, 5, 3))
  
  testthat::expect_true(is.array(down_sample_image(image_array, 2, F)))
})

testthat::test_that("in case of a matrix input (image) the down_sample_image function returns an matrix", {

  image_array = matrix(runif(25), 5, 5)

  testthat::expect_true(is.matrix(down_sample_image(image_array, 2, T)))
})


testthat::test_that("in case of a matrix input (image) the down_sample_image function returns an matrix, if gaussian_blur = FALSE", {
  
  image_array = matrix(runif(25), 5, 5)
  
  testthat::expect_true(is.matrix(down_sample_image(image_array, 2, T)))
})


testthat::test_that("in case of a data frame input (image) the down_sample_image function returns an matrix", {

  image_array = as.data.frame(matrix(runif(25), 5, 5))

  testthat::expect_true(is.matrix(down_sample_image(image_array, 2, F)))
})


testthat::test_that("in case of a factor less than 1 it returns an error", {

  image_array = matrix(runif(25), 5, 5)

  testthat::expect_error(down_sample_image(image_array, factor = 0.5, gaussian_blur = T))
})

testthat::test_that("in case of gaussian_blur = TRUE it returns an error", {

  image_array = matrix(runif(25), 5, 5)

  testthat::expect_error(down_sample_image(image_array, 2, gaussian_blur = NULL))
})

testthat::test_that("in case of an false range_gauss it returns an error", {

  image_array = matrix(runif(25), 5, 5)

  testthat::expect_error(down_sample_image(image_array, 2, gaussian_blur = T, range_gauss = 0))
})


testthat::test_that("in case of a false image input it returns an error", {

  image_array = list(matrix(runif(25), 5, 5))

  testthat::expect_error(down_sample_image(image_array, factor = 2, gaussian_blur = T, range_gauss = 1.0))
})

