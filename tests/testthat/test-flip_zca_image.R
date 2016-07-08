
MATR = matrix(runif(1000), 100, 100)

DF = as.data.frame(MATR)

ARRAY = array(unlist(list(matrix(runif(1000), 100, 100), matrix(runif(1000), 100, 100), matrix(runif(1000), 100, 100))), dim = c(100, 100, 3))


context('Flip and zca whiten image')


# flip image

testthat::test_that("the function flipImage returns an error if mode is invalid", {

  testthat::expect_error( flipImage(MATR, mode = 'invalid') )
})


testthat::test_that("the function flipImage returns an error if image type is invalid", {
  
  testthat::expect_error( flipImage(list(MATR), mode = 'horizontal') )
})


testthat::test_that("the function flipImage takes a matrix and returns a matrix of the same dimensions", {
  
  res = flipImage(MATR, mode = 'horizontal')
  
  testthat::expect_true( is.matrix(res) && ncol(res) == ncol(MATR) && nrow(res) == nrow(MATR) )
})


testthat::test_that("the function flipImage takes a data frame and returns a matrix of the same dimensions", {
  
  res = flipImage(as.data.frame(MATR), mode = 'horizontal')
  
  testthat::expect_true( is.matrix(res) && ncol(res) == ncol(MATR) && nrow(res) == nrow(MATR) )
})


testthat::test_that("the function flipImage takes an array and returns an array of the same dimensions", {
  
  res = flipImage(ARRAY, mode = 'vertical')
  
  testthat::expect_true( is.array(res) && mean(apply(res, 3, ncol)) == ncol(ARRAY[,,1]) && mean(apply(res, 3, nrow)) == nrow(ARRAY[,,1]) )
})



# zca whitening


testthat::test_that("the function ZCAwhiten returns an error if epsilon is negative", {
  
  testthat::expect_error( ZCAwhiten(MATR, k = 5, epsilon = -0.1) )
})


testthat::test_that("the function ZCAwhiten returns an error if k is negative, and image is matrix", {
  
  testthat::expect_error( ZCAwhiten(MATR, k = -1, epsilon = 0.1) )
})


testthat::test_that("the function ZCAwhiten returns an error if k is negative, and image is ARRAY", {
  
  testthat::expect_error( ZCAwhiten(ARRAY, k = -1, epsilon = 0.1) )
})


testthat::test_that("the function ZCAwhiten returns an error if k greater than the number of columns, and image is matrix", {
  
  testthat::expect_error( ZCAwhiten(MATR, k = ncol(MATR) + 1, epsilon = 0.1) )
})


testthat::test_that("the function ZCAwhiten returns an error if k greater than the number of columns, and image is ARRAY", {
  
  testthat::expect_error( ZCAwhiten(ARRAY, k = ncol(MATR) + 1, epsilon = 0.1) )
})


testthat::test_that("the function ZCAwhiten returns an error the image type is invalid", {
  
  testthat::expect_error( ZCAwhiten(list(ARRAY), k = ncol(MATR) + 1, epsilon = 0.1) )
})


testthat::test_that("the function ZCAwhiten takes a data frame and returns a matrix with the correct dimensions", {
  
  res = ZCAwhiten(as.data.frame(MATR), k = 10, epsilon = 0.1)
  
  testthat::expect_true( is.matrix(res) && ncol(MATR) == ncol(res) && nrow(MATR) == nrow(res) )
})


testthat::test_that("the function ZCAwhiten takes a matrix and returns a matrix with the correct dimensions", {
  
  res = ZCAwhiten(MATR, k = 10, epsilon = 0.1)
  
  testthat::expect_true( is.matrix(res) && ncol(MATR) == ncol(res) && nrow(MATR) == nrow(res) )
})


testthat::test_that("the function ZCAwhiten takes an array and returns an array with the correct dimensions", {
  
  res = ZCAwhiten(ARRAY, k = 10, epsilon = 0.1)
  
  testthat::expect_true( is.array(res) && mean(apply(res, 3, ncol)) == ncol(ARRAY[,,1]) && mean(apply(res, 3, nrow)) == nrow(ARRAY[,,1]) )
})
