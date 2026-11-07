
context("F43: slico with a hex grid that holds more seeds than 'superpixel'")

run_slico = function(rows, cols, k) {

  set.seed(1)
  im = array(stats::runif(rows * cols * 3), c(rows, cols, 3)) * 255

  superpixels(im, method = "slico", superpixel = k, compactness = 20, return_labels = TRUE)$labels
}

testthat::test_that("slico returns labels where it stopped with the 'K' error", {

  cases = list(c(523, 645, 10), c(523, 645, 200), c(37, 211, 1), c(1, 1200, 10), c(1200, 1, 10),
               c(10, 1200, 10), c(2, 1200, 10), c(30, 40, 1000))

  for (z in cases) {

    labels = run_slico(z[1], z[2], z[3])

    testthat::expect_equal(dim(labels), c(z[1], z[2]))
    testthat::expect_true(all(is.finite(labels)) && min(labels) == 0)
  }
})

testthat::test_that("slico returns labels where a seed ended without pixels", {

  for (k in c(1200, 2000)) {

    labels = run_slico(30, 40, k)

    testthat::expect_equal(dim(labels), c(30, 40))
    testthat::expect_equal(min(labels), 0)
  }
})
