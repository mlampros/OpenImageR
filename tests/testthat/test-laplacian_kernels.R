context('Laplacian kernels')


testthat::test_that("it returns a matrix", {
  
  testthat::expect_true(is.matrix(laplacian_kernels(type = 1)))
})

testthat::test_that("type is a number between 1 and 4", {
  
  testthat::expect_error(laplacian_kernels(type = 5))
})

# test various kernel-types


testthat::test_that("it returns a matrix", {
  
  testthat::expect_true(is.matrix(laplacian_kernels(type = 2)))
})

testthat::test_that("it returns a matrix", {
  
  testthat::expect_true(is.matrix(laplacian_kernels(type = 3)))
})

testthat::test_that("it returns a matrix", {
  
  testthat::expect_true(is.matrix(laplacian_kernels(type = 4)))
})
