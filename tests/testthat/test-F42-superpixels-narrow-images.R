
context("F42: superpixels() on narrow or elongated images")

run_superpixels = function(rows, cols, k, method) {

  set.seed(1)
  im = array(stats::runif(rows * cols * 3), c(rows, cols, 3)) * 255

  superpixels(im, method = method, superpixel = k, compactness = 20, return_labels = TRUE)$labels
}

testthat::test_that("slic returns complete labels on the shapes that crashed the R session", {

  cases = list(c(100, 1200, 10), c(1200, 100, 10), c(1, 1200, 10), c(1200, 1, 10), c(10, 1200, 10),
               c(2, 1200, 10), c(7, 100, 50))

  for (z in cases) {

    labels = run_superpixels(z[1], z[2], z[3], "slic")

    testthat::expect_equal(dim(labels), c(z[1], z[2]))
    testthat::expect_true(all(is.finite(labels)) && min(labels) == 0)
  }
})

testthat::test_that("slico does not crash the R session on these shapes (F43 covers the R errors it may still give)", {

  cases = list(c(1, 1200, 10), c(1200, 1, 10), c(10, 1200, 10), c(2, 1200, 10), c(30, 40, 2000))

  for (z in cases) {

    res = tryCatch(run_superpixels(z[1], z[2], z[3], "slico"), error = function(e) conditionMessage(e))

    if (is.character(res)) {

      testthat::expect_match(res, "'K' parameter|'clustersize\\[k\\]'")
    }
    else {

      testthat::expect_equal(dim(res), c(z[1], z[2]))
    }
  }
})
