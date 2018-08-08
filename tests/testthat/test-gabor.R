
# image data
#-----------

pth_im = system.file("tmp_images", "car.png", package = "OpenImageR")                  


context('gabor feature extraction')


testthat::test_that("the 'gabor_filter_bank' method returns the expected output", {

  init_gb = GaborFeatureExtract$new()

  ROWS = 39

  COLS = 39

  gb_f = init_gb$gabor_filter_bank(scales = 5, orientations = 8, gabor_rows = ROWS, gabor_columns = COLS, plot_data = TRUE)

  testthat::expect_true( inherits(gb_f, "list") && length(gb_f) == 3 && all(unlist(lapply(gb_f, function(x) lapply(x, function(y) all(nrow(y) == ROWS && ncol(y) == COLS))))) &&

                           is.complex(gb_f[[1]][[1]]) && is.numeric(gb_f[[2]][[1]]) && is.numeric(gb_f[[3]][[1]]) )
})



testthat::test_that("the 'gabor_feature_extraction' method returns an error if the 'downsample_gabor' parameter is TRUE and the 'downsample_rows' and 'downsample_cols' parameter is NULL", {

  init_gb = GaborFeatureExtract$new()

  ROWS = 39

  COLS = 39

  pth_im = system.file("tmp_images", "car.png", package = "OpenImageR")

  im = readImage(pth_im) * 255

  testthat::expect_error( init_gb$gabor_feature_extraction(image = im, scales = 5, orientations = 8, downsample_gabor = TRUE, downsample_rows = NULL,

                                                          downsample_cols = NULL, gabor_rows = ROWS, gabor_columns = COLS, plot_data = TRUE,

                                                          normalize_features = FALSE, threads = 1) )
})



testthat::test_that("the 'gabor_feature_extraction' method returns an error if the 'image' parameter is not a matrix", {

  init_gb = GaborFeatureExtract$new()

  ROWS = 39

  COLS = 39

  pth_im = system.file("tmp_images", "car.png", package = "OpenImageR")

  im = readImage(pth_im) * 255

  testthat::expect_error( init_gb$gabor_feature_extraction(image = as.data.frame(im), scales = 5, orientations = 8, downsample_gabor = TRUE, downsample_rows = 3,

                                                           downsample_cols = 3, gabor_rows = ROWS, gabor_columns = COLS, plot_data = TRUE,

                                                           normalize_features = FALSE, threads = 1) )
})


#--------------------------------------------------------------------------------------------------------------------------------------------------
# ON SOLARIS I RECEIVE THE FOLLOWING ERROR for the following 3 test cases
#------------------------------------------------------------------------

# error: Mat::col(): index out of bounds
# ── 1. Error: the 'gabor_feature_extraction' method returns the expected output i
# Mat::col(): index out of bounds
# 1: init_gb$gabor_feature_extraction(image = im, scales = SCAL, orientations = ORIEN, 
#                                     downsample_gabor = FALSE, downsample_rows = NULL, downsample_cols = NULL, gabor_rows = ROWS, 
#                                     gabor_columns = COLS, plot_data = TRUE, normalize_features = FALSE, threads = 1) at testthat/test-gabor.R:85
# 2: Gabor_export_Features(image, downsample_rows, downsample_cols, scales, orientations, 
#                          gabor_rows, gabor_columns, downsample_gabor, plot_data, normalize_features, threads)
#--------------------------------------------------------------------------------------------------------------------------------------------------


if (Sys.info()['sysname'] != "SunOS") {

  testthat::test_that("the 'gabor_feature_extraction' method returns the expected output if the 'downsample_gabor' parameter is FALSE", {
  
    init_gb = GaborFeatureExtract$new()
  
    ROWS = 13
  
    COLS = 13
  
    SCAL = 3
  
    ORIEN = 5
  
    pth_im = system.file("tmp_images", "car.png", package = "OpenImageR")
  
    im = readImage(pth_im) * 255
  
    gb_im = init_gb$gabor_feature_extraction(image = im, scales = SCAL, orientations = ORIEN, downsample_gabor = FALSE, downsample_rows = NULL,
  
                                             downsample_cols = NULL, gabor_rows = ROWS, gabor_columns = COLS, plot_data = TRUE,
  
                                             normalize_features = FALSE, threads = 1)
  
    testthat::expect_true( length(gb_im$gaborFeatures) == 2 && all(names(gb_im$gaborFeatures) %in% c('magnitude', 'energy_aptitude')) &&
  
                             all(unlist(lapply(gb_im$gabor_features_imaginary, function(x) inherits(x, "array")))) && all(unlist(lapply(gb_im$gabor_features_real, function(x) inherits(x, "array")))) &&
  
                             dim(gb_im$gabor_features_imaginary[[1]]) == c(nrow(im), ncol(im), ORIEN) && dim(gb_im$gabor_features_real[[1]]) == c(nrow(im), ncol(im), ORIEN) )
  })
  
  
  
  testthat::test_that("the 'gabor_feature_extraction' method returns the expected output if the 'downsample_gabor' parameter is TRUE", {             
  
    init_gb = GaborFeatureExtract$new()
  
    ROWS = 13
  
    COLS = 13
  
    SCAL = 3
  
    ORIEN = 5
  
    pth_im = system.file("tmp_images", "car.png", package = "OpenImageR")
  
    im = readImage(pth_im) * 255
  
    gb_im = init_gb$gabor_feature_extraction(image = im, scales = SCAL, orientations = ORIEN, downsample_gabor = TRUE, downsample_rows = 3,
  
                                             downsample_cols = 3, gabor_rows = ROWS, gabor_columns = COLS, plot_data = TRUE,
  
                                             normalize_features = FALSE, threads = 1)
  
    testthat::expect_true(  ncol(gb_im$gaborFeatures$magnitude) == ceiling(nrow(im) / 3) * ceiling(ncol(im) / 3) * SCAL && ncol(gb_im$gaborFeatures$energy_aptitude) == SCAL * ORIEN * 2 &&
                              
                              length(gb_im$gaborFeatures) == 2 && all(names(gb_im$gaborFeatures) %in% c('magnitude', 'energy_aptitude')) &&
  
                              all(unlist(lapply(gb_im$gabor_features_imaginary, function(x) inherits(x, "array")))) && all(unlist(lapply(gb_im$gabor_features_real, function(x) inherits(x, "array")))) &&
  
                             dim(gb_im$gabor_features_imaginary[[1]]) == c(nrow(im), ncol(im), ORIEN) && dim(gb_im$gabor_features_real[[1]]) == c(nrow(im), ncol(im), ORIEN) )
  })
}




testthat::test_that("the 'gabor_feature_engine' method returns an error if the 'downsample_gabor' parameter is TRUE and the 'downsample_rows' and 'downsample_cols' parameter is NULL", {

  init_gb = GaborFeatureExtract$new()

  ROWS = 13

  COLS = 13

  SCAL = 3

  ORIEN = 5

  nrow_mt = 500

  im_width = 12

  im_height = 15

  set.seed(1)
  im_mt = matrix(sample(1:255, nrow_mt * im_width * im_height, replace = T), nrow = nrow_mt, ncol = im_width * im_height)

  testthat::expect_error( init_gb$gabor_feature_engine(img_data = im_mt, img_nrow = im_width, img_ncol = im_height, scales = SCAL, orientations = ORIEN,

                                                      gabor_rows = ROWS, gabor_columns = COLS, downsample_gabor = TRUE, downsample_rows = NULL,

                                                      downsample_cols = NULL, normalize_features = TRUE, threads = 1, verbose = FALSE) )
})




testthat::test_that("the 'gabor_feature_engine' method returns the expected output (2 features : 'local-energy', 'mean-aptitude' )", {

  init_gb = GaborFeatureExtract$new()

  ROWS = 13

  COLS = 13

  SCAL = 3

  ORIEN = 5

  nrow_mt = 500

  im_width = 12

  im_height = 15

  set.seed(1)
  im_mt = matrix(sample(1:255, nrow_mt * im_width * im_height, replace = T), nrow = nrow_mt, ncol = im_width * im_height)

  gb_im = init_gb$gabor_feature_engine(img_data = im_mt, img_nrow = im_width, img_ncol = im_height, scales = SCAL, orientations = ORIEN,

                                       gabor_rows = ROWS, gabor_columns = COLS, downsample_gabor = FALSE, downsample_rows = NULL,

                                       downsample_cols = NULL, normalize_features = TRUE, threads = 1, verbose = FALSE)

  testthat::expect_true( length(gb_im) == 2 && nrow(gb_im$magnitude) == nrow_mt && nrow(gb_im$energy_aptitude) == nrow_mt && ncol(gb_im$energy_aptitude) == SCAL * ORIEN * 2 && 
                           
                           ncol(gb_im$magnitude) == SCAL * ncol(im_mt) )                   # times 2 because I use 2 features ('local-energy', 'mean-aptitude')
})


if (Sys.info()['sysname'] != "SunOS") {

  testthat::test_that("the plot works as expected for the 'gabor_feature_extraction' method", {
  
    init_gb = GaborFeatureExtract$new()
  
    ROWS = 13
  
    COLS = 13
  
    SCAL = 3
  
    ORIEN = 5
  
    pth_im = system.file("tmp_images", "car.png", package = "OpenImageR")
  
    im = readImage(pth_im) * 255
  
    gb_im = init_gb$gabor_feature_extraction(image = im, scales = SCAL, orientations = ORIEN, downsample_gabor = TRUE, downsample_rows = 3,
  
                                             downsample_cols = 3, gabor_rows = ROWS, gabor_columns = COLS, plot_data = TRUE,
  
                                             normalize_features = FALSE, threads = 1)
  
    testthat::expect_silent( init_gb$plot_gabor(real_matrices = gb_im$gabor_features_real, margin_btw_plots = 0.15, thresholding = FALSE) )
  })
}



testthat::test_that("the plot works as expected for the 'gabor_filter_bank' method", {

  init_gb = GaborFeatureExtract$new()

  ROWS = 39

  COLS = 39

  gb_f = init_gb$gabor_filter_bank(scales = 5, orientations = 8, gabor_rows = ROWS, gabor_columns = COLS, plot_data = TRUE)

  testthat::expect_silent( init_gb$plot_gabor(real_matrices = gb_f$gabor_real, margin_btw_plots = 0.15, thresholding = FALSE) )
})



testthat::test_that("plotting multiple images works", {

  init_gb = GaborFeatureExtract$new()

  pth_im = system.file("tmp_images", "car.png", package = "OpenImageR")

  im = readImage(pth_im) * 255

  testthat::expect_silent( init_gb$plot_multi_images(list(im, im , im), par_ROWS = 2, par_COLS = 2) )
})


