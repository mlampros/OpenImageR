
# F19: dilationErosion() allocated a kernel-sized matrix for every pixel; the result must stay the same and the speed improve

morph_reference = function(x, kr, kc, method) {    # window from (i, j) down and right; cells outside the image count as the fill value

  fill = if (method == 'dilation') 0 else if (max(x) > 1) 255 else 1
  p = matrix(fill, nrow(x) + kr - 1, ncol(x) + kc - 1)
  p[seq_len(nrow(x)), seq_len(ncol(x))] = x
  FUN = if (method == 'dilation') pmax else pmin
  out = x
  first = TRUE
  for (k in seq_len(kr)) for (f in seq_len(kc)) {
    s = p[k:(k + nrow(x) - 1), f:(f + ncol(x) - 1)]
    out = if (first) s else FUN(out, s)
    first = FALSE
  }
  out
}

testthat::test_that("dilationErosion() equals the reference", {

  set.seed(1)
  x = matrix(runif(30 * 20), 30, 20)
  y = matrix(sample(0:255, 30 * 20, TRUE) + 0, 30, 20)

  for (m in c('dilation', 'erosion')) {
    for (flt in list(c(3, 3), c(2, 5), c(5, 1))) {
      testthat::expect_equal(dilationErosion(x, flt, m), morph_reference(x, flt[1], flt[2], m))
      testthat::expect_equal(dilationErosion(y, flt, m), morph_reference(y, flt[1], flt[2], m))
    }
  }

  a = array(runif(20 * 20 * 3), c(20, 20, 3))
  out = dilationErosion(a, c(3, 3), 'erosion')
  for (i in 1:3) testthat::expect_equal(out[, , i], morph_reference(a[, , i], 3, 3, 'erosion'))
})

testthat::test_that("dilationErosion() is faster than the shifted-matrix R reference", {

  testthat::skip_on_cran()

  set.seed(2); x = matrix(runif(1024 * 1024), 1024, 1024)
  t_pkg = min(sapply(1:3, function(i) system.time(dilationErosion(x, c(3, 3), 'dilation'))[["elapsed"]]))
  t_ref = min(sapply(1:3, function(i) system.time(morph_reference(x, 3, 3, 'dilation'))[["elapsed"]]))

  testthat::expect_lt(t_pkg / t_ref, 1)
})
