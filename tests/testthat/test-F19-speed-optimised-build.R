
# F19: the speed test must also fail on the old per-pixel kernel code in an optimised (-O2) build

morph_reference_O2 = function(x, kr, kc) {        # dilation: window from (i, j) down and right, cells outside the image count as 0

  p = matrix(0, nrow(x) + kr - 1, ncol(x) + kc - 1)
  p[seq_len(nrow(x)), seq_len(ncol(x))] = x
  out = NULL
  for (k in seq_len(kr)) for (f in seq_len(kc)) {
    s = p[k:(k + nrow(x) - 1), f:(f + ncol(x) - 1)]
    out = if (is.null(out)) s else pmax(out, s)
  }
  out
}

testthat::test_that("dilationErosion() is well below the time of the shifted-matrix R reference", {

  testthat::skip_on_cran()

  set.seed(2); x = matrix(runif(1024 * 1024), 1024, 1024)
  t_pkg = min(sapply(1:3, function(i) system.time(dilationErosion(x, c(3, 3), 'dilation'))[["elapsed"]]))
  t_ref = min(sapply(1:3, function(i) system.time(morph_reference_O2(x, 3, 3))[["elapsed"]]))

  # ratio on one machine (REVIEW2_2026-11-08_1630.md): before the fix (27ab8f9) 1.04-1.05 in an -O2 build and 3.07-3.11 under load_all;
  # after it 0.27 (-O2) and 0.39-0.42 (load_all). A threshold of 1 did not fail before the fix in an -O2 build
  testthat::expect_lt(t_pkg / t_ref, 0.6)
})
