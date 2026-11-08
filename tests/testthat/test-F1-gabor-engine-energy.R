
context("F1: gabor_feature_engine local energy columns")

testthat::test_that("gabor_feature_engine() and gabor_feature_extraction() return the same local energy and mean amplitude", {

  set.seed(1)
  im_mt = matrix(sample(1:255, 20 * 12 * 15, TRUE), nrow = 20)

  gb = GaborFeatureExtract$new()
  ge = gb$gabor_feature_engine(img_data = im_mt, img_nrow = 12, img_ncol = 15, scales = 3, orientations = 5,
                               gabor_rows = 13, gabor_columns = 13, downsample_gabor = FALSE,
                               downsample_rows = NULL, downsample_cols = NULL, normalize_features = FALSE,
                               threads = 1, verbose = FALSE)

  gf = gb$gabor_feature_extraction(image = matrix(im_mt[1, ], 12, 15), scales = 3, orientations = 5,
                                   gabor_rows = 13, gabor_columns = 13, downsample_gabor = FALSE,
                                   downsample_rows = NULL, downsample_cols = NULL, normalize_features = FALSE,
                                   threads = 1, vectorize_magnitude = TRUE)

  testthat::expect_equal(ncol(ge$energy_aptitude), 30)
  testthat::expect_equal(sum(colSums(abs(ge$energy_aptitude)) == 0), 0)
  testthat::expect_equal(as.vector(ge$energy_aptitude[1, ]), as.vector(gf$gaborFeatures$energy_aptitude[1, ]))
})
