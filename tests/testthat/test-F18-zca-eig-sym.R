
# F18: ZCAwhiten() decomposes the symmetric covariance with eig_sym instead of an svd; the result must not change

zca_reference = function(x, k, epsilon) {                    # the svd version of OpenImageR 1.3.0, in R

  xc = sweep(x, 2, colMeans(x))
  s = svd(tcrossprod(xc) / ncol(x))
  u = s$u[, 1:k, drop = FALSE]
  (u %*% diag(1 / sqrt(s$d[1:k] + epsilon), k) %*% t(u)) %*% xc
}

testthat::test_that("ZCAwhiten() equals the svd-based reference", {

  set.seed(1)
  for (cs in list(c(30, 30, 5), c(30, 30, 30), c(40, 15, 10), c(40, 15, 30), c(12, 25, 12))) {

    x = matrix(runif(cs[1] * cs[2]), cs[1], cs[2])
    testthat::expect_equal(ZCAwhiten(x, cs[3], 0.1), zca_reference(x, cs[3], 0.1), tolerance = 1e-10)
  }
})

testthat::test_that("ZCAwhiten() is not slowed down by a full svd", {

  testthat::skip_on_cran()

  set.seed(2); x = matrix(runif(512 * 512), 512, 512)
  t_pkg = min(sapply(1:3, function(i) system.time(ZCAwhiten(x, 20, 0.1))[["elapsed"]]))
  xc = sweep(x, 2, colMeans(x)); s = tcrossprod(xc) / ncol(x)
  t_eig = min(sapply(1:3, function(i) system.time(eigen(s, symmetric = TRUE))[["elapsed"]]))

  # measured on this machine (reference BLAS): 3.03 x the time of R's eigen() at commit 401bab4, 1.57 x after the fix
  testthat::expect_lt(t_pkg / t_eig, 2.4)
})
