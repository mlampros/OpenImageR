
# F14: hash_apply(mode = 'hash') must run in parallel (the whole loop body was in an omp critical section)

testthat::test_that("hash_apply(mode = 'hash') gives the same hashes with 1 and 4 threads", {

  set.seed(1)
  m = matrix(runif(50 * 32 * 32), 50, 32 * 32)
  a = array(runif(32 * 32 * 50), c(32, 32, 50))

  for (meth in c('phash', 'average_hash', 'dhash')) {

    testthat::expect_identical(hash_apply(m, rows = 32, columns = 32, method = meth, mode = 'hash', threads = 4),
                               hash_apply(m, rows = 32, columns = 32, method = meth, mode = 'hash', threads = 1))
    testthat::expect_identical(hash_apply(a, method = meth, mode = 'hash', threads = 4),
                               hash_apply(a, method = meth, mode = 'hash', threads = 1))
  }
})

testthat::test_that("hash_apply(mode = 'hash') is faster with 4 threads than with 1", {

  testthat::skip_on_cran()
  testthat::skip_if(parallel::detectCores() < 4, "needs 4 cores")

  set.seed(2)
  a = array(runif(64 * 64 * 600), c(64, 64, 600))

  best = function(thr) min(sapply(1:3, function(k) system.time(hash_apply(a, method = 'phash', mode = 'hash', threads = thr))[["elapsed"]]))

  t1 = best(1)
  t4 = best(4)

  testthat::expect_lt(t4 / t1, 0.7)         # the serialised loop gave a ratio near 1 (11.9 s / 11.7 s in the audit)
})
