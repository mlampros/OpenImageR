
# F7: Augmentation() must rotate with rotate_method, not with resiz_method

testthat::test_that("Augmentation() rotates with rotate_method", {

  set.seed(1); x = matrix(runif(400), 20, 20)

  for (rm in c('nearest', 'bilinear')) {

    for (zm in c('nearest', 'bilinear')) {

      testthat::expect_equal(Augmentation(x, rotate_angle = 30, rotate_method = rm, resiz_method = zm), rotateImage(x, 30, rm, 'same'))
    }
  }

  arr = array(runif(20 * 20 * 3), c(20, 20, 3))
  testthat::expect_equal(Augmentation(arr, rotate_angle = 30, rotate_method = 'bilinear', resiz_method = 'nearest'), rotateImage(arr, 30, 'bilinear', 'same'))
})
