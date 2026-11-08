
context("F41: superpixels() with very small or very large 'superpixel' values")

run_slic = function(rows, cols, k) {

  set.seed(1)
  im = array(stats::runif(rows * cols * 3), c(rows, cols, 3)) * 255

  superpixels(im, method = "slic", superpixel = k, compactness = 20, return_labels = TRUE)$labels
}

testthat::test_that("slic returns complete labels for the values that crashed the R session", {

  cases = list(c(481, 321, 1), c(481, 321, 2), c(481, 321, 4), c(481, 321, 5),
               c(300, 500, 1), c(300, 500, 2), c(300, 500, 4), c(300, 500, 5), c(300, 500, 6),
               c(37, 211, 1), c(37, 211, 2), c(37, 211, 3), c(37, 211, 4), c(37, 211, 5),
               c(100, 100, 3), c(60, 80, 1), c(60, 80, 2), c(60, 80, 4), c(60, 80, 5))

  for (z in cases) {

    labels = run_slic(z[1], z[2], z[3])

    testthat::expect_equal(dim(labels), c(z[1], z[2]))
    testthat::expect_true(all(is.finite(labels)) && min(labels) == 0)
  }
})

testthat::test_that("slic works with more superpixels than twice the pixels", {

  labels = run_slic(30, 40, 3000)

  testthat::expect_equal(dim(labels), c(30, 40))
  testthat::expect_equal(min(labels), 0)
})

testthat::test_that("invalid 'superpixel' values give an R error", {

  im = array(stats::runif(30 * 40 * 3), c(30, 40, 3)) * 255

  for (method in c("slic", "slico")) {

    for (k in list(0, -1, 0.5, NA, NA_real_, 1e10, "10", c(10, 20))) {

      testthat::expect_error(superpixels(im, method = method, superpixel = k, return_labels = TRUE), "superpixel")
    }
  }
})
