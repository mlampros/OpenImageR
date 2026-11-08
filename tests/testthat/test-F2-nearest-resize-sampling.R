
context("F2: nearest-neighbour resize sampling")

testthat::test_that("an identity nearest resize returns the input unchanged", {

  m = matrix(as.numeric(1:100), 10, 10)

  testthat::expect_identical(resizeImage(m, 10, 10, 'nearest'), m)
})

testthat::test_that("downsampling picks evenly spaced rows and columns (10 -> 5 picks 1 3 5 7 9)", {

  m = matrix(as.numeric(1:100), 10, 10)
  r = resizeImage(m, 5, 5, 'nearest')

  testthat::expect_equal(r, m[c(1, 3, 5, 7, 9), c(1, 3, 5, 7, 9)])
})

testthat::test_that("upsampling keeps the last row and column of the input", {

  m = matrix(as.numeric(1:16), 4, 4)
  r = resizeImage(m, 8, 8, 'nearest')

  testthat::expect_equal(r, m[rep(1:4, each = 2), rep(1:4, each = 2)])
})

testthat::test_that("arrays are resized like their slices", {

  a = array(as.numeric(1:300), c(10, 10, 3))
  r = resizeImage(a, 10, 10, 'nearest')

  testthat::expect_equal(r, a)
})

testthat::test_that("Augmentation() without arguments is the identity", {

  m = matrix(as.numeric(1:100), 10, 10)

  testthat::expect_equal(sum(Augmentation(m) != m), 0)
})
