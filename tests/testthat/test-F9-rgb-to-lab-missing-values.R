# F9 (reopened after REVIEW2_2026-11-08_1512.md): the [0, 1] check of RGB_to_Lab() must not fail on NA values

testthat::test_that("RGB_to_Lab() of an image with an NA gives NA for that pixel and the usual values for the others", {

  x = array(c(0.5, NA, 0.2, 0.1, 0.3, 0.4), c(2, 1, 3))           # pixel 1 = (0.5, 0.2, 0.3), pixel 2 = (NA, 0.1, 0.4)
  lab = RGB_to_Lab(x)                                               # at a22c8da: "missing value where TRUE/FALSE needed"

  ref = RGB_to_Lab(array(c(0.5, 0.2, 0.3), c(1, 1, 3)))
  testthat::expect_equal(lab[1, 1, ], ref[1, 1, ])
  testthat::expect_true(is.na(lab[2, 1, 1]))

  y = array(c(200, NA, 30, 10, 60, 90), c(2, 1, 3))               # a 0-255 image with an NA
  lab255 = RGB_to_Lab(y)
  testthat::expect_equal(lab255[1, 1, ], RGB_to_Lab(array(c(200, 30, 60), c(1, 1, 3)))[1, 1, ])
  testthat::expect_true(is.na(lab255[2, 1, 1]))
})
