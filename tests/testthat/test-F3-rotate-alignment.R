
context("F3: rotateImage() alignment and edges")

testthat::test_that("rotating by 0 degrees in mode 'same' with 'nearest' returns the input", {

  m = matrix(as.numeric(1:400), 20, 20)

  testthat::expect_equal(rotateImage(m, 0, 'nearest', 'same'), m)
})

testthat::test_that("rotating by 0 degrees with 'nearest' keeps non-square and odd shapes", {

  m = matrix(as.numeric(1:(21 * 13)), 21, 13)                      # 21 -> canvas 29 (even difference), 13 -> canvas 18 (odd difference)

  testthat::expect_equal(rotateImage(m, 0, 'nearest', 'same'), m)
})

testthat::test_that("rotating an array by 0 degrees in mode 'same' with 'nearest' returns the input", {

  a = array(as.numeric(1:(20 * 20 * 3)), c(20, 20, 3))

  testthat::expect_equal(rotateImage(a, 0, 'nearest', 'same'), a)
})

testthat::test_that("'bilinear' at 0 degrees is not shifted and reads the last row and column", {

  flat = matrix(1, 21, 13)

  testthat::expect_equal(rotateImage(flat, 0, 'bilinear', 'same'), flat)

  # the weights of the interpolation are not exact at whole pixels (see F6), so this
  # bounds the error: a ramp with steps 1 and 20 gives at most (1 + 20) / 4 = 5.25

  m = matrix(as.numeric(1:400), 20, 20)

  testthat::expect_lte(max(abs(rotateImage(m, 0, 'bilinear', 'same') - m)), 5.25)
})

testthat::test_that("'nearest' in mode 'full' samples source row 0 and column 0", {

  rf = rotateImage(matrix(1, 20, 20), 90, 'nearest', 'full')

  testthat::expect_equal(sum(rowSums(rf) == 0), 8)                 # 28 rows of canvas - 20 rows of image
})
