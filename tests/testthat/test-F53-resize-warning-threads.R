
# F53: the resize warning (Rcpp::warning) was raised from OpenMP worker threads of hash_apply(), which can end the R process

testthat::test_that("hash_apply() with threads = 2 on images whose resize warns gives the threads = 1 result", {

  set.seed(1)
  a = array(runif(49 * 49 * 3000), c(49, 49, 3000))       # 49 x 49 -> 8 x 8 makes resize_nearest_rcpp() warn ("the 'width' becomes 7")

  for (md in c('hash', 'binary')) {

    r1 = suppressWarnings(hash_apply(a, hash_size = 8, method = 'average_hash', mode = md, resize = 'nearest', threads = 1))
    r2 = suppressWarnings(hash_apply(a, hash_size = 8, method = 'average_hash', mode = md, resize = 'nearest', threads = 2))     # image 0 runs on the main thread (F44) and still warns

    testthat::expect_equal(r2, r1)
  }
})

testthat::test_that("resizeImage() still warns outside a parallel region", {

  testthat::expect_warning(OpenImageR:::resize_nearest_rcpp(matrix(runif(49 * 49), 49, 49), 8, 8), "becomes")
})
