
# F45: rotateImage() 'bilinear' must use real bilinear weights: no shift at right angles and the exact pixel at whole positions

testthat::test_that("rotateImage() 'bilinear' does not shift a centred disk at right angles", {

  n = 101; g = expand.grid(r = 1:n, c = 1:n)
  k = matrix(as.numeric((g$r - 51)^2 + (g$c - 51)^2 <= 20^2), n, n)

  for (a in c(0, 90, 180, 270)) {

    x = rotateImage(k, a, 'bilinear', 'same')
    testthat::expect_equal(c(sum(row(x) * x), sum(col(x) * x)) / sum(x) - 51, c(0, 0), tolerance = 1e-9)
  }
})

testthat::test_that("rotateImage() 'bilinear' equals 'nearest' at right angles and the input at 0 degrees", {

  m = matrix(as.numeric(1:400), 20, 20)

  testthat::expect_equal(rotateImage(m, 0, 'bilinear', 'same'), m)

  for (a in c(90, 180, 270)) {

    testthat::expect_equal(rotateImage(m, a, 'bilinear', 'same'), rotateImage(m, a, 'nearest', 'same'))
  }
})

testthat::test_that("rotateImage() 'bilinear' equals a bilinear reference at 30 degrees", {

  set.seed(1); n = 15; m = 12; x = matrix(runif(n * m), n, m)
  out = rotateImage(x, 30, 'bilinear', 'full')
  mm = nrow(out); nn = ncol(out); thet = 30 * pi / 180

  ref = matrix(0, mm, nn)
  for (t in 0:(mm - 1)) for (s in 0:(nn - 1)) {

    n0 = (t + 0.5 - mm / 2) * cos(thet) + (s + 0.5 - nn / 2) * sin(thet) + n / 2 - 0.5
    m0 = -(t + 0.5 - mm / 2) * sin(thet) + (s + 0.5 - nn / 2) * cos(thet) + m / 2 - 0.5
    if (floor(n0 + 0.5 + 1e-9) < 0 || floor(n0 + 0.5 + 1e-9) >= n || floor(m0 + 0.5 + 1e-9) < 0 || floor(m0 + 0.5 + 1e-9) >= m) next

    n1 = floor(n0); m1 = floor(m0); fn = n0 - n1; fm = m0 - m1
    px = function(r, c) x[min(max(r, 0), n - 1) + 1, min(max(c, 0), m - 1) + 1]      # the pixels read stay inside the image
    ref[t + 1, s + 1] = (1 - fn) * (1 - fm) * px(n1, m1) + (1 - fn) * fm * px(n1, m1 + 1) + fn * (1 - fm) * px(n1 + 1, m1) + fn * fm * px(n1 + 1, m1 + 1)
  }

  testthat::expect_equal(out, ref, tolerance = 1e-12)
})
