
# F13: an error raised by superpixels() after its pixel buffers were allocated must not leak them

testthat::test_that("superpixels() with an invalid method does not grow the process memory", {

  testthat::skip_if_not(file.exists("/proc/self/status"), "needs /proc/self/status (Linux)")

  rss_mb = function() {
    st = readLines("/proc/self/status")
    as.numeric(gsub("[^0-9]", "", st[grepl("^VmRSS:", st)])) / 1024
  }

  set.seed(1)
  im = array(runif(3 * 600 * 600), c(600, 600, 3))
  for (i in 1:3) try(suppressWarnings(superpixels(im, method = "bad")), silent = TRUE)      # warm up
  invisible(gc())
  before = rss_mb()

  msg = character(40)
  for (i in 1:40) {
    msg[i] = tryCatch(suppressWarnings(superpixels(im, method = "bad")), error = function(e) conditionMessage(e))
  }

  invisible(gc())
  testthat::expect_true(all(grepl("Invalid 'method' parameter", msg)))
  growth = rss_mb() - before

  testthat::expect_lt(growth, 20)          # measured before the fix (commit 401bab4, this machine): 107.2 MB
})
