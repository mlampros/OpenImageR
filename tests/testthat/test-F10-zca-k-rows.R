
# F10: the covariance of ZCAwhiten() is nrow x nrow, so k is limited by the number of rows, not of columns

testthat::test_that("ZCAwhiten() checks k against the number of rows", {

  set.seed(1)
  wide = matrix(runif(50), 5, 10)
  testthat::expect_error(ZCAwhiten(wide, k = 8, epsilon = 0.1), "nrow")
  testthat::expect_error(ZCAwhiten(array(runif(150), c(5, 10, 3)), k = 8, epsilon = 0.1), "nrow")
  testthat::expect_equal(dim(ZCAwhiten(wide, k = 5, epsilon = 0.1)), c(5, 10))

  tall = matrix(runif(50), 10, 5)
  testthat::expect_equal(dim(ZCAwhiten(tall, k = 8, epsilon = 0.1)), c(10, 5))
  testthat::expect_equal(dim(ZCAwhiten(array(runif(150), c(10, 5, 3)), k = 8, epsilon = 0.1)), c(10, 5, 3))
})
