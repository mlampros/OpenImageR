
# F17: plot_gabor() and plot_multi_images() must restore the graphical parameters they change

testthat::test_that("plot_gabor() and plot_multi_images() restore par()", {

  f = tempfile(fileext = ".pdf")
  grDevices::pdf(f)
  on.exit({ grDevices::dev.off(); unlink(f) }, add = TRUE)

  before = graphics::par(c("mfrow", "mar"))

  gb = GaborFeatureExtract$new()
  set.seed(1)
  imgs = list(matrix(runif(100), 10, 10), matrix(runif(100), 10, 10), matrix(runif(100), 10, 10), matrix(runif(100), 10, 10))
  gb$plot_multi_images(imgs, par_ROWS = 2, par_COLS = 2)
  testthat::expect_equal(graphics::par(c("mfrow", "mar")), before)

  fb = gb$gabor_filter_bank(scales = 2, orientations = 2, gabor_rows = 7, gabor_columns = 7, plot_data = TRUE)
  gb$plot_gabor(fb$gabor_real, margin_btw_plots = 0.5)
  testthat::expect_equal(graphics::par(c("mfrow", "mar")), before)
})
