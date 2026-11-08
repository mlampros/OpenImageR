# F19 (reopened after REVIEW2_2026-11-08_1512.md): as in 401bab4 (arma::max()/min() over the window), a NaN cell of a window is skipped

morph_reference_nan = function(x, kr, kc, method) {    # window from (i, j) down and right; cells outside the image count as the fill value; NaN skipped

  fill = if (method == 'dilation') 0 else if (max(x, na.rm = TRUE) > 1) 255 else 1
  p = matrix(fill, nrow(x) + kr - 1, ncol(x) + kc - 1)
  p[seq_len(nrow(x)), seq_len(ncol(x))] = x
  FUN = if (method == 'dilation') max else min
  out = x
  for (i in seq_len(nrow(x))) for (j in seq_len(ncol(x))) {
    out[i, j] = FUN(p[i:(i + kr - 1), j:(j + kc - 1)], na.rm = TRUE)
  }
  out
}

testthat::test_that("dilationErosion() skips a NaN cell wherever it is in the window", {

  x = matrix(c(0.2, NaN, 0.5, 0.9, 0.1, 0.3, 0.7, 0.4, 0.6), 3, 3)
  testthat::expect_equal(dilationErosion(x, c(2, 2), 'dilation')[2, 1], 0.5)      # 401bab4: 0.5 (REVIEW2_2026-11-08_1512.md); a22c8da: NaN

  set.seed(2)
  y = matrix(runif(12 * 10), 12, 10)
  y[sample(length(y), 15)] = NaN
  for (m in c('dilation', 'erosion')) {
    for (flt in list(c(2, 2), c(3, 3), c(1, 4), c(4, 1))) {
      testthat::expect_equal(dilationErosion(y, flt, m), morph_reference_nan(y, flt[1], flt[2], m))
    }
  }
})
