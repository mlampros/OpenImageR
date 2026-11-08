
# F62: HOG_apply() on an image with a NaN pixel threw an Armadillo error inside the OpenMP loop, which ended the R process

testthat::test_that("HOG_apply() on an array with a NaN pixel gives an R error", {

  set.seed(1)
  a = array(runif(20 * 20 * 4), c(20, 20, 4))
  a[5, 5, 2] = NaN

  for (th in c(1, 2)) {
    testthat::expect_error(HOG_apply(a, threads = th), "NA, NaN or infinite")
  }
})

testthat::test_that("HOG_apply() on a matrix of images with a NaN pixel gives an R error", {

  set.seed(2)
  m = matrix(runif(4 * 400), 4, 400)               # 4 images of 20 x 20, one per row
  m[3, 45] = NaN

  for (th in c(1, 2)) {
    testthat::expect_error(HOG_apply(m, rows = 20, columns = 20, threads = th), "NA, NaN or infinite")
  }
})

testthat::test_that("HOG_apply() without NaN gives the same result with 1 and 2 threads", {

  set.seed(3)
  a = array(runif(20 * 20 * 4), c(20, 20, 4))

  testthat::expect_equal(HOG_apply(a, threads = 2), HOG_apply(a, threads = 1))
})
