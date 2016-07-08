MATR = matrix(runif(1000), 100, 100)

DF = as.data.frame(MATR)

ARRAY = array(unlist(list(matrix(runif(1000), 100, 100), matrix(runif(1000), 100, 100), matrix(runif(1000), 100, 100))), dim = c(100, 100, 3))


context('Deletion and erosion tests')


testthat::test_that("the function delationErosion returns an error if the threads is less than 1", {
  
  testthat::expect_error( delationErosion(MATR, c(3,3), method = 'delation', threads = 0) )
})

testthat::test_that("the function delationErosion returns an error if the method is invalid", {
  
  testthat::expect_error( delationErosion(MATR, c(3,3), method = 'invalid', threads = 1) )
})

testthat::test_that("the function delationErosion returns an error if the filter isn't in the correct form", {
  
  testthat::expect_error( delationErosion(MATR, list(c(3,3)), method = 'delation', threads = 1) )
})

testthat::test_that("the function delationErosion returns an error if the filter isn't in the correct form", {
  
  testthat::expect_error( delationErosion(MATR, c(0,3), method = 'delation', threads = 1) )
})

testthat::test_that("the function delationErosion returns an error if the filter isn't in the correct form", {
  
  testthat::expect_error( delationErosion(MATR, c(3,0), method = 'delation', threads = 1) )
})

testthat::test_that("the function delationErosion returns an error if the filter isn't in the correct form", {
  
  testthat::expect_error( delationErosion(MATR, c(nrow(MATR),3), method = 'delation', threads = 1) )
})

testthat::test_that("the function delationErosion returns an error if the filter isn't in the correct form", {
  
  testthat::expect_error( delationErosion(MATR, c(4,ncol(MATR)), method = 'delation', threads = 1) )
})

testthat::test_that("the function delationErosion returns an error if the filter isn't in the correct form", {
  
  testthat::expect_error( delationErosion(MATR, c(4,3,5), method = 'delation', threads = 1) )
})

testthat::test_that("the function delationErosion returns an error if the filter isn't in the correct form", {
  
  testthat::expect_error( delationErosion(MATR, c('4','3'), method = 'delation', threads = 1) )
})

testthat::test_that("the function delationErosion returns an error if the image isn't one of matrix , data frame or array", {
  
  testthat::expect_error( delationErosion(list(MATR), c(4,4), method = 'delation', threads = 1) )
})

testthat::test_that("the function delationErosion takes a data frame as input and returns a matrix, method = 'delation'", {
  
  res = delationErosion(as.data.frame(MATR), c(4,4), method = 'delation', threads = 1)
  
  testthat::expect_true( is.matrix(res) && nrow(res) == nrow(MATR) && ncol(res) == ncol(MATR) )
})

testthat::test_that("the function delationErosion takes a matrix as input and returns a matrix, method = 'delation'", {
  
  res = delationErosion(MATR, c(4,4), method = 'delation', threads = 1)
  
  testthat::expect_true( is.matrix(res) && nrow(res) == nrow(MATR) && ncol(res) == ncol(MATR) )
})

testthat::test_that("the function delationErosion takes a matrix as input and returns a matrix, method = 'delation'", {
  
  res = delationErosion(ARRAY, c(4,4), method = 'delation', threads = 1)
  
  testthat::expect_true( is.array(res) && nrow(res) == nrow(ARRAY) && ncol(res) == ncol(ARRAY) && length(dim(ARRAY)) == length(dim(res)) )
})


testthat::test_that("the function delationErosion takes a data frame as input and returns a matrix, method = 'erosion'", {
  
  res = delationErosion(as.data.frame(MATR), c(4,4), method = 'erosion', threads = 1)
  
  testthat::expect_true( is.matrix(res) && nrow(res) == nrow(MATR) && ncol(res) == ncol(MATR) )
})

testthat::test_that("the function delationErosion takes a matrix as input and returns a matrix, method = 'erosion'", {
  
  res = delationErosion(MATR, c(4,4), method = 'erosion', threads = 1)
  
  testthat::expect_true( is.matrix(res) && nrow(res) == nrow(MATR) && ncol(res) == ncol(MATR) )
})

testthat::test_that("the function delationErosion takes a matrix as input and returns a matrix, method = 'erosion'", {
  
  res = delationErosion(ARRAY, c(4,4), method = 'erosion', threads = 1)
  
  testthat::expect_true( is.array(res) && nrow(res) == nrow(ARRAY) && ncol(res) == ncol(ARRAY) && length(dim(ARRAY)) == length(dim(res)) )
})

testthat::test_that("the function delationErosion takes a matrix as input and returns a matrix, method = 'erosion', RANGE of values between 0 and 255", {
  
  MATR1 = matrix(sample(c(0,255), 10000, replace = T), 100, 100)
  
  res = delationErosion(MATR1, c(4,4), method = 'erosion', threads = 1)
  
  testthat::expect_true( is.matrix(res) && nrow(res) == nrow(MATR) && ncol(res) == ncol(MATR) )
})