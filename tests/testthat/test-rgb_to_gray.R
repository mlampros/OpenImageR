context('Convert RGB image to gray')


testthat::test_that("it returns an error if the image is not 3-dimensional", {

  out_array = matrix(1:100, 10, 10)

  testthat::expect_error(rgb_2gray(out_array))
})


testthat::test_that("the function takes a 3-dimensional array and it returns a 2-dimensional matrix", {

  out_array = array(runif(300), dim = c(10, 10, 3))

  res = rgb_2gray(out_array)

  testthat::expect_true(is.matrix(res) && nrow(res) == dim(out_array)[1] && ncol(res) == dim(out_array)[2])
})
