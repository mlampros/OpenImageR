
# F6: the bilinear rotation must read all four neighbours; the (n2, m2) one (below right of the source position) was never read

testthat::test_that("rotateImage() 'bilinear' reads the neighbour below right of the source position", {

  n = 31; m = 31; r0 = 16; c0 = 16                             # one bright pixel (1-based), all others 0
  x = matrix(0, n, m); x[r0, c0] = 1

  for (a in c(20, 30, 45, 120)) {

    out = rotateImage(x, a, 'bilinear', 'full')
    mm = nrow(out); nn = ncol(out); thet = a * pi / 180

    # source position (0-based, pixel centres) of every output pixel, as in inner_bilinear()
    t = row(out) - 1; s = col(out) - 1
    n0 = (t + 0.5 - mm / 2) * cos(thet) + (s + 0.5 - nn / 2) * sin(thet) + n / 2 - 0.5
    m0 = -(t + 0.5 - mm / 2) * sin(thet) + (s + 0.5 - nn / 2) * cos(thet) + m / 2 - 0.5

    # output pixels whose (n2, m2) neighbour is the bright pixel: it has a positive weight there
    sel = n0 > (r0 - 2) + 1e-6 & n0 < (r0 - 1) - 1e-6 & m0 > (c0 - 2) + 1e-6 & m0 < (c0 - 1) - 1e-6

    testthat::expect_true(any(sel))
    testthat::expect_true(all(out[sel] > 0))
  }
})
