
# F57: Augmentation() on a double matrix wrote its result into the caller's matrix (augment_transf() received R's memory through arma::mat&)

testthat::test_that("Augmentation() leaves the input matrix unchanged", {

  set.seed(1)
  y = matrix(runif(20 * 20), 20, 20)
  y_copy = y + 0

  out = Augmentation(y, image_thresh = 0.5)
  testthat::expect_equal(y, y_copy)
  testthat::expect_true(all(out %in% c(0, 1)))

  out = Augmentation(y, flip_mode = 'horizontal', shift_rows = 3, rotate_angle = 30)
  testthat::expect_equal(y, y_copy)
})

testthat::test_that("Augmentation() leaves an input array and a list of arrays unchanged", {

  set.seed(2)
  a = array(runif(10 * 10 * 3), c(10, 10, 3))
  a_copy = a + 0

  out = Augmentation(a, image_thresh = 0.5, flip_mode = 'vertical')
  testthat::expect_equal(a, a_copy)

  l = list(a, a + 0.1)
  l_copy = lapply(l, function(z) z + 0)
  out = Augmentation(l, image_thresh = 0.5)
  testthat::expect_equal(l, l_copy)
})
