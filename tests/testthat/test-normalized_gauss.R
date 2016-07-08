context('Output of normalized gaussian kernel')


testthat::test_that("it returns a matrix", {
  
  tmp_matr = matrix(runif(25), 5, 5)
  
  testthat::expect_true(is.matrix(norm_range_gauss(tmp_matr, max_range = -1, min_range = 1)))
})

testthat::test_that("the data is a matrix", {
  
  data = list(matrix(runif(25), 5, 5))
  
  testthat::expect_error(norm_range_gauss(data, max_range = -1, min_range = 1))
})