
context("F24: RGB_to_HSV() black pixels")

testthat::test_that("an all-black image gives hue 0, saturation 0 and value 0", {

  res = RGB_to_HSV(array(0, c(2, 2, 3)))

  testthat::expect_equal(sum(is.nan(res)), 0)
  testthat::expect_equal(res, array(0, c(2, 2, 3)))
})

testthat::test_that("one black pixel leaves the other pixels unchanged", {

  set.seed(1)
  a = array(runif(4 * 4 * 3, 0.1, 1), c(4, 4, 3))
  b = a
  b[2, 3, ] = 0

  res_a = RGB_to_HSV(a)
  res_b = RGB_to_HSV(b)

  testthat::expect_equal(as.vector(res_b[2, 3, ]), c(0, 0, 0))
  testthat::expect_equal(res_b[-2, -3, ], res_a[-2, -3, ])
})
