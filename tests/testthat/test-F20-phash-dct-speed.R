
# F20: the DCT of phash recomputed its cosine vectors for every row and output element, and vec2mat() built an index matrix per image;
# the hashes must not change

quiet_hash = function(...) { out = NULL; utils::capture.output(out <- hash_apply(...)); out }

testthat::test_that("hash_apply() gives the hashes of commit 401bab4", {

  set.seed(7)
  a = array(runif(32 * 32 * 12), c(32, 32, 12))
  m = matrix(runif(12 * 32 * 32), 12, 32 * 32)

  # values printed by hash_apply() at commit 401bab4 (before the fix) for the same seed and inputs
  testthat::expect_identical(quiet_hash(a, method = "phash", mode = "hash"),
                             c("970a6f5ab294869e", "15652380fdd8cf17", "a1edb6d30705d117", "a526379e720c738d", "73221843bf6178af", "c964fa1f2e58f109",
                               "5b445c5eb45bca23", "bda525b867c151e4", "4513a496aec7a3d5", "c740adf39ca7630a", "23e78e60ebf62898", "1f4c94522dcd675a"))
  testthat::expect_identical(quiet_hash(m, rows = 32, columns = 32, method = "phash", mode = "hash"),
                             c("47785806ee9d83d3", "f1478a65a9d1ed48", "c7d5f84b11c127b4", "2d8703dc05f78a79", "3957c8ba0898d5f9", "17b059396be2cc95",
                               "652347c3073975b5", "49bcf4de1b06c4d1", "09bdb2f3c627c8b0", "09cab3426ebe4c6e", "2de1aac31036b67e", "6f5b8e53284e6aa4"))
  testthat::expect_identical(quiet_hash(m, rows = 32, columns = 32, method = "dhash", mode = "hash"),
                             c("4a2d293962a95195", "b68bafb2a9a9cd1a", "baba359b5893ab4b", "2a6325a02b6316b7", "cd4acaad69e66aaa", "ab2b93562a5b6732",
                               "8e39d26535a53b55", "5695892d2595a995", "6a6daa32f2342d35", "656e494cdaa82ae7", "4d54b2c52aac6538", "6825958d6644a74b"))
})

testthat::test_that("phash of 2000 small images is not dominated by the DCT set-up", {

  testthat::skip_on_cran()

  set.seed(5); a = array(runif(28 * 28 * 2000), c(28, 28, 2000))
  t_pkg = min(sapply(1:3, function(i) system.time(quiet_hash(a, hash_size = 5, highfreq_factor = 4, method = 'phash', mode = 'binary'))[["elapsed"]]))

  D = cos(pi / 20 * outer(0:19, (0:19) + 0.5))           # the same 2-D DCT with a precomputed matrix, in R
  t_ref = min(sapply(1:3, function(i) system.time(for (k in 1:2000) D %*% a[1:20, 1:20, k] %*% t(D))[["elapsed"]]))

  # measured on this machine under devtools::load_all(): 3.026 s / 0.062 s = 48.8 at commit 401bab4, 0.66 s / 0.064 s = 10.3 after the fix
  testthat::expect_lt(t_pkg / t_ref, 25)
})
