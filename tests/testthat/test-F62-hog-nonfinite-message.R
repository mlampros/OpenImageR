
# F62 (review 2026-11-08): a NaN, NA or infinite pixel must give a clear R error from hog_cpp() itself, not depend on
# converting NaN to int (undefined behaviour, an out-of-range bin on x86-64 and bin 0 on arm64)

testthat::test_that("HOG() and HOG_apply() name non-finite pixels in their error", {

  set.seed(1)
  x = matrix(runif(20 * 20), 20, 20)

  for (v in c(NaN, NA, Inf, -Inf)) {

    y = x; y[5, 5] = v
    testthat::expect_error(HOG(y), "NA, NaN or infinite")

    a = array(runif(20 * 20 * 4), c(20, 20, 4)); a[5, 5, 2] = v
    testthat::expect_error(HOG_apply(a, threads = 2), "NA, NaN or infinite")

    m = matrix(runif(4 * 400), 4, 400); m[3, 45] = v
    testthat::expect_error(HOG_apply(m, rows = 20, columns = 20, threads = 2), "NA, NaN or infinite")
  }
})

testthat::test_that("HOG() of a finite image is unchanged (values at commit 258c231)", {

  set.seed(4)
  x = matrix(runif(20 * 20), 20, 20)

  testthat::expect_equal(sum(HOG(x)), 1.13690573836, tolerance = 1e-9)
})
