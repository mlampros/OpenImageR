# F44 (reopened after REVIEW2_2026-11-08_1512.md): building an Rcpp::exception in a worker thread is not thread-safe, so with many
# images and threads the R process could still abort; an input error must be raised on the main thread, before the parallel loop

testthat::test_that("hash_apply() input errors with many images and threads are R errors", {

  bad = matrix(as.numeric(seq_len(5000 * 3)), 5000, 3)       # 5000 images of 3 x 1: the 'bilinear' resize needs at least 2 columns

  for (r in 1:20) {
    for (md in c('binary', 'hash')) {
      testthat::expect_error(utils::capture.output(hash_apply(bad, rows = 3, columns = 1, hash_size = 2, method = 'average_hash', mode = md,
                                                              resize = 'bilinear', threads = 8)), "'bilinear' method needs")
    }
  }

  ok = NULL
  utils::capture.output(ok <- hash_apply(matrix(runif(40 * 64), 40, 64), rows = 8, columns = 8, hash_size = 2, method = 'average_hash',
                                         resize = 'bilinear', threads = 8))
  testthat::expect_equal(dim(ok), c(40, 4))
})
