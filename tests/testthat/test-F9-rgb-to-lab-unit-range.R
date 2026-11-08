
# F9: RGB_to_Lab() must give the same result for a [0, 1] image as for the same image in 0-255

testthat::test_that("RGB_to_Lab() of a [0, 1] image equals RGB_to_Lab() of the 0-255 image", {

  set.seed(1)
  a255 = array(sample(0:255, 15 * 15 * 3, replace = TRUE), c(15, 15, 3))
  a01 = a255 / 255

  lab01 = RGB_to_Lab(a01)
  lab255 = RGB_to_Lab(a255)

  testthat::expect_equal(lab01, lab255, tolerance = 1e-10)
  testthat::expect_gt(length(unique(as.vector(lab01[, , 1]))), 100)
})

testthat::test_that("RGB_to_Lab() keeps the results of 0-255 inputs", {

  # 0-255 values keep the integer truncation of earlier versions: 12.7 and 200.9 give the result of 12 and 200
  x = array(c(255, 12.7, 0, 0, 200.9, 0, 0, 64, 0), c(3, 1, 3))
  lab = RGB_to_Lab(x)
  ref = RGB_to_Lab(array(c(255, 12, 0, 0, 200, 0, 0, 64, 0), c(3, 1, 3)))

  testthat::expect_equal(lab, ref)
  testthat::expect_equal(lab[3, 1, ], c(0, 0, 0))
})
