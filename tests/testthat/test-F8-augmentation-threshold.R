
# F8: Augmentation(image_thresh = ) must map values above the threshold to 1, as image_thresholding() does
# (the expected values are computed first and the input is passed as a fresh copy, because Augmentation() can
# overwrite a double matrix passed to it)

testthat::test_that("Augmentation(image_thresh = ) matches image_thresholding()", {

  x = matrix(c(0.2, 0.8, 0.3, 0.9), 2, 2)
  testthat::expect_equal(Augmentation(x + 0, image_thresh = 0.5), matrix(c(0, 1, 0, 1), 2, 2))

  set.seed(1); y = matrix(runif(100), 10, 10)
  exp_y = image_thresholding(y, 0.4)
  testthat::expect_equal(Augmentation(y + 0, image_thresh = 0.4), exp_y)

  arr = array(runif(10 * 10 * 3), c(10, 10, 3))
  exp_arr = lapply(1:3, function(i) image_thresholding(arr[, , i], 0.6))
  out = Augmentation(arr + 0, image_thresh = 0.6)
  for (i in 1:3) testthat::expect_equal(out[, , i], exp_arr[[i]])
})
