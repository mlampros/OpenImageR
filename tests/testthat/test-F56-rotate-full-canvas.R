
# F56: rotateImage(mode = 'full') must keep the whole non-square image at 90 and 270 degrees (D17: the output dimensions change)

testthat::test_that("rotateImage() mode 'full' keeps every value of a non-square image at right angles", {

  for (sz in list(c(5, 8), c(8, 5), c(5, 100), c(13, 7))) {

    x = matrix(as.numeric(seq_len(sz[1] * sz[2])), sz[1], sz[2])
    d = floor(sqrt(sz[1]^2 + sz[2]^2))

    for (a in c(90, 270)) {

      r = rotateImage(x, a, 'nearest', 'full')
      testthat::expect_equal(dim(r), c(d, d))
      testthat::expect_setequal(setdiff(as.vector(r), 0), as.vector(x))
    }
  }

  arr = array(as.numeric(seq_len(5 * 8 * 3)), c(5, 8, 3))
  ra = rotateImage(arr, 90, 'nearest', 'full')
  testthat::expect_equal(dim(ra), c(9, 9, 3))
  for (i in 1:3) testthat::expect_setequal(setdiff(as.vector(ra[, , i]), 0), as.vector(arr[, , i]))
})

testthat::test_that("rotateImage() mode 'full' keeps the canvas size of square images", {

  x = matrix(runif(100), 10, 10)
  testthat::expect_equal(dim(rotateImage(x, 30, 'bilinear', 'full')), c(14, 14))
  testthat::expect_equal(dim(rotateImage(array(runif(300), c(10, 10, 3)), 30, 'nearest', 'full')), c(14, 14, 3))
})
