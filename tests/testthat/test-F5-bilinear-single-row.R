
context("F5: bilinear resize of a single row, as at version 1.3.0")

testthat::test_that("a single row that is reduced to half its columns or less keeps working", {

  m = matrix(as.numeric(2:7) * 10, 1, 6)

  testthat::expect_equal(resizeImage(m, 1, 3, 'bilinear'), matrix(c(30, 50, 70), 1, 3))
})

testthat::test_that("an array of single rows keeps working", {

  a = array(as.numeric(2:7) * 10, c(1, 6, 3))
  r = resizeImage(a, 1, 3, 'bilinear')

  testthat::expect_equal(dim(r), c(1, 3, 3))
  testthat::expect_equal(r[1, , 2], c(30, 50, 70))
})

testthat::test_that("hash_apply() with 'bilinear' on single-row resizes returns and does not end the R session", {

  r = hash_apply(matrix(as.numeric(2:129), 4, 32), rows = 1, columns = 32, hash_size = 2, method = 'average_hash', resize = 'bilinear')

  testthat::expect_equal(dim(r), c(4, 4))
})

testthat::test_that("a single row that is not reduced enough, a single column and an empty image stay errors", {

  m = matrix(as.numeric(2:7) * 10, 1, 6)

  testthat::expect_error(resizeImage(m, 3, 12, 'bilinear'), "bilinear")
  testthat::expect_error(resizeImage(matrix(1:6 + 0.0, 6, 1), 3, 1, 'bilinear'), "bilinear")
  testthat::expect_error(suppressWarnings(resizeImage(matrix(numeric(0), 0, 6), 1, 3, 'bilinear')), "bilinear")
})
