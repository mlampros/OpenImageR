testthat::test_that("'bilinear' keeps no black pixel on a flat image at multiples of 90 degrees", {

  for (n in c(20, 100)) {

    flat = matrix(1, n, n)

    for (a in c(90, 180, 270, 360)) {

      testthat::expect_equal(rotateImage(flat, a, 'bilinear', 'same'), flat)
    }
  }
})

testthat::test_that("'bilinear' keeps no black pixel on a random image and on the slices of an array", {

  set.seed(1)
  m = matrix(runif(64 * 64) + 1, 64, 64)                # values in [1, 2]: a 0 can only be a pixel that was dropped

  testthat::expect_equal(sum(rotateImage(m, 360, 'bilinear', 'same') == 0), 0)

  rgb = array(1, c(100, 100, 3))

  testthat::expect_equal(sum(rotateImage(rgb, 360, 'bilinear', 'same') == 0), 0)
})

testthat::test_that("Augmentation() with 'bilinear' keeps no black pixel at 180 degrees", {

  flat = matrix(1, 100, 100)

  testthat::expect_equal(sum(Augmentation(flat, rotate_angle = 180, resiz_method = 'bilinear', rotate_method = 'bilinear') == 0), 0)
})
