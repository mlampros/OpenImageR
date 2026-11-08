
# F16: gabor_feature_extraction() kept every complex convolution result in a list it never returned

testthat::test_that("gabor_feature_extraction() does not keep the complex results when plot_data = FALSE", {

  testthat::skip_on_cran()
  testthat::skip_if_not(file.exists("/proc/self/status"), "needs /proc/self/status (Linux)")

  field = function(name) { st = readLines("/proc/self/status"); as.numeric(gsub("[^0-9]", "", st[grepl(paste0("^", name, ":"), st)])) / 1024 }
  can_reset = tryCatch({ cat("5", file = "/proc/self/clear_refs"); TRUE }, error = function(e) FALSE)       # "5" resets the peak RSS (VmHWM)
  testthat::skip_if_not(can_reset, "cannot reset the peak RSS")

  set.seed(1); im = matrix(runif(256 * 256), 256, 256)
  gb = GaborFeatureExtract$new()
  invisible(gc())
  cat("5", file = "/proc/self/clear_refs")
  start = field("VmRSS")

  r = gb$gabor_feature_extraction(im, scales = 5, orientations = 8, gabor_rows = 13, gabor_columns = 13, downsample_gabor = FALSE,
                                  plot_data = FALSE, normalize_features = FALSE, threads = 1)

  peak = field("VmHWM") - start

  # measured on this machine for a 256 x 256 image, 5 scales x 8 orientations: 83.6 MB at commit 401bab4, 36.9 MB after the fix;
  # the dead list held 5 * 256 * 256 * 8 complex values = 40 MiB
  testthat::expect_lt(peak, 60)
  testthat::expect_equal(dim(r$gaborFeatures$energy_aptitude), c(1, 80))
})
