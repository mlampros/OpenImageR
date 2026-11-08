
context("F5: bilinear resize speed and values")

bilinear_reference = function(x, width, height) {

  rf = seq_len(width) * (nrow(x) / width)
  cf = seq_len(height) * (ncol(x) / height)
  r = pmin(pmax(floor(rf), 1), nrow(x) - 1)
  cc = pmin(pmax(floor(cf), 1), ncol(x) - 1)
  dr = rf - r
  dc = cf - cc

  outer(1 - dr, 1 - dc) * x[r, cc] + outer(dr, 1 - dc) * x[r + 1, cc] +
    outer(1 - dr, dc) * x[r, cc + 1] + outer(dr, dc) * x[r + 1, cc + 1]
}

testthat::test_that("bilinear resize matches the reference for downsampling, upsampling and mixed shapes", {

  set.seed(1)

  for (shape in list(c(10, 10, 10, 10), c(10, 10, 5, 5), c(10, 10, 15, 15), c(7, 13, 30, 3), c(2, 2, 9, 9), c(50, 40, 33, 77))) {

    m = matrix(runif(shape[1] * shape[2], 2, 255), shape[1], shape[2])        # values above 1, which resizeImage() does not scale

    testthat::expect_equal(resizeImage(m, shape[3], shape[4], 'bilinear'), bilinear_reference(m, shape[3], shape[4]))
  }
})

testthat::test_that("bilinear resize of an array resizes every slice", {

  set.seed(2)
  a = array(runif(20 * 30 * 3, 2, 255), c(20, 30, 3))
  res = resizeImage(a, 41, 17, 'bilinear')

  for (i in 1:3) {

    testthat::expect_equal(res[, , i], bilinear_reference(a[, , i], 41, 17))
  }
})

testthat::test_that("bilinear resize of a single row or column stays an error", {

  testthat::expect_error(resizeImage(matrix(as.numeric(1:6), 1, 6), 3, 12, 'bilinear'))
  testthat::expect_error(resizeImage(matrix(as.numeric(1:6), 6, 1), 12, 3, 'bilinear'))
})

testthat::test_that("bilinear resize of a 1024 x 1024 matrix to 2048 x 2048 takes less than 2 seconds", {

  testthat::skip_on_cran()

  g = matrix(0.5, 1024, 1024)

  elapsed = system.time(res <- resizeImage(g, 2048, 2048, 'bilinear'))[["elapsed"]]

  testthat::expect_lt(elapsed, 2)                # the index-matrix version took 5.55 s (FINDINGS F5)
  testthat::expect_equal(dim(res), c(2048L, 2048L))
})
