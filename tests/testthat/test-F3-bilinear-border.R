testthat::test_that("'bilinear' covers the same cells as 'nearest' at any angle", {

  flat = matrix(1, 100, 100)

  for (a in c(10, 30, 45, 200)) {

    testthat::expect_equal(rotateImage(flat, a, 'bilinear', 'full') != 0, rotateImage(flat, a, 'nearest', 'full') != 0)
  }
})

testthat::test_that("'bilinear' turns the image the same way as 'nearest' at right angles", {

  m = matrix(as.numeric(1:400), 20, 20)

  for (a in c(90, 180, 270)) {

    testthat::expect_lte(max(abs(rotateImage(m, a, 'bilinear', 'same') - rotateImage(m, a, 'nearest', 'same'))), 5.25)
  }
})
