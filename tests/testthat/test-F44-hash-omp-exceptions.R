
# F44: an error raised inside the OpenMP loops of hash_apply() ended the R process ("terminate called after throwing an instance of
# 'Rcpp::exception'"); it must reach R as an ordinary error. Before the fix this file crashed the R session.

testthat::test_that("hash_apply() errors inside the parallel loops become R errors", {

  bad_m = matrix(as.numeric(2:13), 4, 3)              # 3 x 1 images: the 'bilinear' resize needs at least 2 columns

  for (thr in c(1, 2)) {
    for (md in c('binary', 'hash')) {

      testthat::expect_error(utils::capture.output(hash_apply(bad_m, rows = 3, columns = 1, hash_size = 2, method = 'average_hash', mode = md,
                                                              resize = 'bilinear', threads = thr)), "'bilinear' method needs")
    }
  }

  ok = NULL
  utils::capture.output(ok <- hash_apply(matrix(runif(4 * 64), 4, 64), rows = 8, columns = 8, hash_size = 2, method = 'average_hash', resize = 'bilinear'))
  testthat::expect_equal(dim(ok), c(4, 4))           # R is still usable afterwards
})
