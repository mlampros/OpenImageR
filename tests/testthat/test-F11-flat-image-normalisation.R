
# F11: the min-max normalisers must not divide by zero on a flat (constant) input

testthat::test_that("edge_detection() of a flat image is 0 (no edges), never NaN", {

  for (meth in c('Frei_chen', 'LoG', 'Prewitt', 'Roberts_cross', 'Scharr', 'Sobel')) {

    out = edge_detection(matrix(0, 10, 10), method = meth, conv_mode = 'same')
    testthat::expect_false(any(is.nan(out)))
    testthat::expect_true(all(out == 0))
  }

  out = edge_detection(array(0.5, c(10, 10, 3)), method = 'Sobel', conv_mode = 'same')
  testthat::expect_false(any(is.nan(out)))
})

testthat::test_that("the normalisers return 0 (or min_value) for a flat input", {

  testthat::expect_equal(NormalizeObject(matrix(2, 3, 3)), matrix(0, 3, 3))
  testthat::expect_equal(NormalizeObject(rep(2, 4)), rep(0, 4))
  testthat::expect_equal(NormalizeObject(array(2, c(3, 3, 3))), array(0, c(3, 3, 3)))
  testthat::expect_equal(norm_matrix_range(matrix(1, 3, 3)), matrix(-1, 3, 3))

  arr = array(c(rep(5, 9), 1:9, rep(0, 9)), c(3, 3, 3))           # flat, ramp, flat slices
  out = NormalizeObject(arr)
  testthat::expect_equal(out[, , 1], matrix(0, 3, 3))
  testthat::expect_equal(out[, , 2], matrix((0:8) / 8, 3, 3))
})
