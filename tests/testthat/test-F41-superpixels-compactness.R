
context("F41: superpixels(method = 'slic') with a compactness that gives a non-finite weight")

testthat::test_that("slic does not crash on a compactness that gives a non-finite weight", {

  set.seed(1)
  a = array(runif(30 * 40 * 3), c(30, 40, 3)) * 255

  for (cc in c(NA, NaN, Inf, 1e308)) {

    labels = superpixels(a, method = "slic", superpixel = 20, compactness = cc, return_labels = TRUE)$labels

    testthat::expect_equal(dim(labels), c(30, 40))
    testthat::expect_equal(min(labels), 0)
    testthat::expect_false(anyNA(labels))
  }
})
