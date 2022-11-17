

#' Gabor Feature Extraction
#'
#'
#' @param scales a numeric value. Number of scales (usually set to 5)     ( gabor_filter_bank function )
#' @param orientations a numeric value. Number of orientations (usually set to 8)     ( gabor_filter_bank function )
#' @param gabor_rows a numeric value. Number of rows of the 2-D Gabor filter (an odd integer number, usually set to 39 depending on the image size)     ( gabor_filter_bank function )
#' @param gabor_columns a numeric value. Number of columns of the 2-D Gabor filter (an odd integer number, usually set to 39 depending on the image size)       ( gabor_filter_bank function )
#' @param plot_data either TRUE or FALSE. If TRUE then data needed for plotting will be returned     ( gabor_filter_bank, gabor_feature_extraction functions )
#' @param image a 2-dimensional image of type matrix    ( gabor_feature_extraction function )
#' @param downsample_rows either NULL or a numeric value specifying the factor of downsampling along rows    ( gabor_feature_extraction function )
#' @param downsample_cols either NULL or a numeric value specifying the factor of downsampling along columns    ( gabor_feature_extraction function )
#' @param downsample_gabor either TRUE or FALSE. If TRUE then downsampling of data will take place. The \emph{downsample_rows} and \emph{downsample_cols} should be adjusted accordingly. Downsampling does not affect the output plots but the output \emph{gabor_features}     ( gabor_feature_extraction function )
#' @param normalize_features either TRUE or FALSE. If TRUE then the output gabor-features will be normalized to zero mean and unit variance    ( gabor_feature_extraction function )
#' @param threads a numeric value specifying the number of threads to use    ( gabor_feature_extraction function )
#' @param vectorize_magnitude either TRUE or FALSE. If TRUE the computed magnitude feature will be returned in the form of a vector, otherwise it will be returned as a list of matrices  ( gabor_feature_extraction function )
#' @param img_data a numeric matrix specifying the input data (gabor_feature_engine function)
#' @param img_nrow an integer specifying the number of rows of the input matrix (gabor_feature_engine function)
#' @param img_ncol an integer specifying the number of columns of the input matrix (gabor_feature_engine function)
#' @param real_matrices a list of 3-dimensional arrays (where the third dimension is equal to 3). These arrays correspond to the \emph{real part} of the complex output matrices      ( plot_gabor function )
#' @param margin_btw_plots a float between 0.0 and 1.0 specifying the margin between the multiple output plots      ( plot_gabor function )
#' @param thresholding either TRUE or FALSE. If TRUE then a threshold of 0.5 will be used to push values above 0.5 to 1.0 ( similar to otsu-thresholding )      ( plot_gabor function )
#' @param verbose either TRUE or FALSE. If TRUE then information will be printed in the console    ( gabor_feature_extraction, gabor_feature_engine functions )
#' @param list_images a list containing the images to plot  ( plot_multi_images function )
#' @param par_ROWS a numeric value specifying the number of rows of the plot-grid  ( plot_multi_images function )
#' @param par_COLS a numeric value specifying the number of columns of the plot-grid  ( plot_multi_images function )
#' @param axes a boolean. If TRUE then the X- and Y-range of values (axes) will appear in the output images  ( plot_multi_images function )
#' @param titles either NULL or a character vector specifying the main-titles of the output images. The length of this vector must be the same as the length of the input 'list_images' parameter  ( plot_multi_images function )
#'
#' @export
#' @details
#'
#' In case of an RGB image (3-dimensional where the third dimension is equal to 3) one can use the \emph{rgb_2gray()} to convert the image to a 2-dimensional one
#'
#' I added the option \emph{downsample_gabor} to the original matlab code based on the following question on stackoverflow : \emph{https://stackoverflow.com/questions/49119991/feature-extraction-with-gabor-filters}
#'
#' @references
#'
#' https://github.com/mhaghighat/gabor
#'
#' https://stackoverflow.com/questions/20608458/gabor-feature-extraction
#'
#' https://stackoverflow.com/questions/49119991/feature-extraction-with-gabor-filters
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom graphics image title par
#'
#' @section Methods:
#'
#' \describe{
#'  \item{\code{GaborFeatureExtract$new()}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{gabor_filter_bank(scales, orientations, gabor_rows, gabor_columns, plot_data = FALSE)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{gabor_feature_extraction(image, scales, orientations, gabor_rows, gabor_columns, downsample_gabor = FALSE, plot_data = FALSE,
#'                                       downsample_rows = NULL, downsample_cols = NULL, normalize_features = FALSE, threads = 1, vectorize_magnitude = TRUE)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{gabor_feature_engine(img_data, img_nrow, img_ncol, scales, orientations, gabor_rows, gabor_columns, downsample_gabor = FALSE,
#'                                   downsample_rows = NULL, downsample_cols = NULL, normalize_features = FALSE, threads = 1, verbose = FALSE)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{plot_gabor(real_matrices, margin_btw_plots = 0.15, thresholding = FALSE)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{plot_multi_images(list_images, par_ROWS, par_COLS)}}{}
#'
#'  \item{\code{--------------}}{}
#' }
#'
#' @usage # init <- GaborFeatureExtract$new()
#' @examples
#'
#' library(OpenImageR)
#'
#' init_gb = GaborFeatureExtract$new()
#'
#' # gabor-filter-bank
#' #------------------
#'
#' gb_f = init_gb$gabor_filter_bank(scales = 5, orientations = 8, gabor_rows = 39,
#'
#'                                  gabor_columns = 39, plot_data = TRUE)
#'
#'
#' # plot gabor-filter-bank
#' #-----------------------
#'
#' plt_f = init_gb$plot_gabor(real_matrices = gb_f$gabor_real, margin_btw_plots = 0.65,
#'
#'                            thresholding = FALSE)
#'
#'
#' # read image
#' #-----------
#'
#' pth_im = system.file("tmp_images", "car.png", package = "OpenImageR")
#'
#' im = readImage(pth_im) * 255
#'
#'
#' # gabor-feature-extract
#' #----------------------
#'
#' # gb_im = init_gb$gabor_feature_extraction(image = im, scales = 5, orientations = 8,
#'
#' #                                          downsample_gabor = TRUE, downsample_rows = 3,
#'
#' #                                          downsample_cols = 3, gabor_rows = 39, gabor_columns = 39,
#'
#' #                                          plot_data = TRUE, normalize_features = FALSE,
#'
#' #                                          threads = 6)
#'
#'
#' # plot real data of gabor-feature-extract
#' #----------------------------------------
#'
#' # plt_im = init_gb$plot_gabor(real_matrices = gb_im$gabor_features_real, margin_btw_plots = 0.65,
#'
#' #                             thresholding = FALSE)
#'
#'
#' # feature generation for a matrix of images (such as the mnist data set)
#' #-----------------------------------------------------------------------
#'
#' ROWS = 13; COLS = 13; SCAL = 3; ORIEN = 5; nrow_mt = 500; im_width = 12; im_height = 15
#'
#' set.seed(1)
#' im_mt = matrix(sample(1:255, nrow_mt * im_width * im_height, replace = TRUE), nrow = nrow_mt,
#'
#'                       ncol = im_width * im_height)
#'
#' # gb_ex = init_gb$gabor_feature_engine(img_data = im_mt, img_nrow = im_width, img_ncol = im_height,
#'
#' #                                      scales = SCAL, orientations = ORIEN, gabor_rows = ROWS,
#'
#' #                                      gabor_columns = COLS, downsample_gabor = FALSE,
#'
#' #                                      downsample_rows = NULL, downsample_cols = NULL,
#'
#' #                                      normalize_features = TRUE, threads = 1, verbose = FALSE)
#'
#'
#' # plot of multiple image in same figure
#' #---------------------------------------
#'
#' list_images = list(im, im, im)
#'
#' plt_multi = init_gb$plot_multi_images(list_images, par_ROWS = 2, par_COLS = 2)
#'

GaborFeatureExtract <- R6::R6Class("GaborFeatureExtract",

                                   lock_objects = FALSE,

                                   public = list(

                                     initialize = function() {

                                     },


                                     #-----------------------------
                                     # create a 'Gabor Filter Bank'
                                     #-----------------------------

                                     gabor_filter_bank = function(scales, orientations, gabor_rows, gabor_columns, plot_data = FALSE) {

                                       res = Gabor_Filter_Bank(scales, orientations, gabor_rows, gabor_columns, plot_data)

                                       private$ROWS = scales

                                       private$COLS = orientations

                                       private$flag_filter_bank = TRUE

                                       return(res)
                                     },



                                     #------------------------
                                     # create 'Gabor Features'
                                     #------------------------

                                     gabor_feature_extraction = function(image, scales, orientations, gabor_rows, gabor_columns, downsample_gabor = FALSE, plot_data = FALSE,

                                                                         downsample_rows = NULL, downsample_cols = NULL, normalize_features = FALSE, threads = 1, verbose = FALSE, vectorize_magnitude = TRUE) {

                                       if (verbose) { START = Sys.time() }

                                       if (downsample_gabor) {

                                         if (is.null(downsample_rows) || is.null(downsample_cols)) {            #  it is better that 'downsample_rows' and 'downsample_cols' is NULL so that the user is aware of the values that he will use to downsample the data in case that 'downsample_gabor' is TRUE

                                           stop("In case that the 'downsample_gabor' parameter is TRUE the 'downsample_rows' and 'downsample_cols' parameter should be non-NULL", call. = F)
                                         }
                                       }

                                       else {        # set default values in case that no downsampling takes place

                                         downsample_rows = 1
                                         downsample_cols = 1
                                       }

                                       if (!inherits(image, "matrix")) stop("The 'image' parameter should be a 2-dimensional image ( matrix )", call. = F)

                                       res = Gabor_export_Features(image, downsample_rows, downsample_cols, scales, orientations, gabor_rows, gabor_columns, downsample_gabor, plot_data, normalize_features, threads, vectorize_magnitude)

                                       #----------------------------------------------------------------------------------------------------------
                                       # the conversion of the vectors to matrices will be column-wise, because the armadillo 'vectorise' function
                                       # vectorizes the matrices column-wise. SEE line 1887 ('gabor_features_Magn' variable) and line 1778
                                       # ('gaborMagn_vec') for the details on why I use matrix(data, nrow, ncol, byrow = F) in the next two lines.
                                       # Moreover, line 1776 explains the computation of the magnitude based on the real and imaginary parts
                                       #----------------------------------------------------------------------------------------------------------

                                       if (!vectorize_magnitude) {     # convert to list of matrices
                                         res$gaborFeatures$magnitude = lapply(1:ncol(res$gaborFeatures$magnitude), function(x) matrix(res$gaborFeatures$magnitude[, x], nrow = nrow(image), ncol = ncol(image)))
                                       }

                                       private$flag_filter_bank = FALSE

                                       if (verbose) { END = Sys.time(); t = END - START; cat("Time to complete :", t, attr(t, "units"), "\n") }

                                       return(res)
                                     },



                                     #-------------------------------------------------------------------------------------------
                                     # feature engineering based on the 'gabor_features' of the 'gabor_feature_extraction' method
                                     #
                                     # this can be used with matrices of images such as the mnist data set (70000 x 784)
                                     #
                                     # it took 10 min to run the 70000 lines of mnist using 6 threads and 5 scales and 8 orientations
                                     # it took 10 min to run the 70000 lines of mnist using 6 threads and 5 scales and 8 orientations  WITH 'normalize_features' = TRUE
                                     #-------------------------------------------------------------------------------------------

                                     gabor_feature_engine = function(img_data, img_nrow, img_ncol, scales, orientations, gabor_rows, gabor_columns, downsample_gabor = FALSE,

                                                                     downsample_rows = NULL, downsample_cols = NULL, normalize_features = FALSE, threads = 1, verbose = FALSE) {

                                       if (verbose) { START = Sys.time() }

                                       if (downsample_gabor) {

                                         if (is.null(downsample_rows) || is.null(downsample_cols)) {            #  it is better that 'downsample_rows' and 'downsample_cols' is NULL so that the user is aware of the values that he will use to downsample the data in case that 'downsample_gabor' is TRUE

                                           stop("In case that the 'downsample_gabor' parameter is TRUE the 'downsample_rows' and 'downsample_cols' parameter should be non-NULL", call. = F)
                                         }
                                       }

                                       else {        # set default values in case that no downsampling takes place

                                         downsample_rows = 1
                                         downsample_cols = 1
                                       }

                                       res = Gabor_generate(img_data, img_nrow, img_ncol, downsample_rows, downsample_cols, scales, orientations, gabor_rows,

                                                            gabor_columns, downsample_gabor, normalize_features, threads)

                                       if (verbose) { END = Sys.time(); t = END - START; cat("Time to complete :", t, attr(t, "units"), "\n") }

                                       return(res)
                                     },



                                     #-----------------------------------------------------------------
                                     # function to plot the gabor-filters and gabor-output-real results
                                     #-----------------------------------------------------------------

                                     plot_gabor = function(real_matrices, margin_btw_plots = 0.65, thresholding = FALSE) {

                                       if (is.null(private$flag_filter_bank)) {

                                         grid_rows = length(real_matrices)

                                         grid_columns = dim(real_matrices[[1]])[3]
                                       }

                                       else {

                                         grid_rows = ifelse(private$flag_filter_bank, private$ROWS, length(real_matrices))

                                         grid_columns = ifelse(private$flag_filter_bank, private$COLS, dim(real_matrices[[1]])[3])
                                       }

                                       graphics::par(mfrow = c(grid_rows, grid_columns), mar = c(margin_btw_plots, margin_btw_plots, margin_btw_plots, margin_btw_plots))

                                       if (is.null(private$flag_filter_bank)) {

                                         for (i in 1:grid_rows) {

                                           for (j in 1:grid_columns) {

                                             gpl = NormalizeObject(real_matrices[[i]][,, j])                  # normalize so that negative values will be pushed between [0,1]

                                             gpl = rotateFixed(gpl, angle = 90)                               # rotate image 90 degrees   ( necessary otherwise the image() function does not show the image in the same angle as is the case in the original image )

                                             if (thresholding) {

                                               gpl = image_thresholding(gpl, thresh = 0.5)

                                               gpl = private$adjust_minority_class(gpl)
                                             }

                                             graphics::image(gpl, axes = FALSE, col = grey(seq(0, 1, length = 256)))

                                             graphics::title(main = paste("scale:", i, "&", "orientation:", j, sep = " "), font.main = 1)
                                           }
                                         }
                                       }

                                       else {

                                         if (private$flag_filter_bank) {

                                           grid_extend = matrix(1:(grid_rows * grid_columns), nrow = grid_rows, ncol = grid_columns)                               # make the grid ordering row-wise

                                           mesh_scales = meshgrid_y(grid_rows, grid_columns) + 1                                                                   # necessary for the title() function

                                           mesh_orient = meshgrid_x(grid_rows, grid_columns) + 1                                                                   # necessary for the title() function

                                           idx = unlist(lapply(1:nrow(grid_extend), function(x) grid_extend[x, ]))

                                           for (i in 1:(grid_rows * grid_columns)) {                                # this works in the same way as the for-loop in the 'gaborFilterBank()' rcpp function

                                             gpl = NormalizeObject(real_matrices[[idx[i]]])                         # normalize so that negative values will be pushed between [0,1]

                                             gpl = rotateFixed(gpl, angle = 90)                                     # rotate image 90 degrees   ( necessary otherwise the image() function does not show the image in the same angle as is the case in the original image )

                                             graphics::image(gpl, axes = FALSE, col = grey(seq(0, 1, length = 256)))

                                             graphics::title(main = paste("scale:", mesh_scales[idx[i]], "&", "orientation:", mesh_orient[idx[i]], sep = " "), font.main = 1)
                                           }
                                         }

                                         else {

                                           for (i in 1:grid_rows) {

                                             for (j in 1:grid_columns) {

                                               gpl = NormalizeObject(real_matrices[[i]][,, j])                  # normalize so that negative values will be pushed between [0,1]

                                               gpl = rotateFixed(gpl, angle = 90)                               # rotate image 90 degrees   ( necessary otherwise the image() function does not show the image in the same angle as is the case in the original image )

                                               if (thresholding) {

                                                 gpl = image_thresholding(gpl, thresh = 0.5)

                                                 gpl = private$adjust_minority_class(gpl)
                                               }

                                               graphics::image(gpl, axes = FALSE, col = grey(seq(0, 1, length = 256)))

                                               graphics::title(main = paste("scale:", i, "&", "orientation:", j, sep = " "), font.main = 1)
                                             }
                                           }
                                         }
                                       }
                                     },


                                     #-----------------------------------
                                     # plot images using a list of images
                                     #-----------------------------------

                                     plot_multi_images = function(list_images, par_ROWS, par_COLS, axes = FALSE, titles = NULL) {

                                       if (!is.null(titles)) {
                                         if (!is.vector(titles, mode = 'character')) {
                                           stop("The 'titles' parameter must be of type character!", call. = F)
                                         }
                                         if (length(list_images) != length(titles)) {
                                           stop("The length of the input 'list_images' parameter must be the same as the 'titles' parameter!", call. = F)
                                         }
                                       }
                                       graphics::par(mfrow = c(par_ROWS, par_COLS))

                                       out = lapply(seq_along(list_images), function(x) {

                                         gpl = OpenImageR::NormalizeObject(list_images[[x]])           # normalize so that negative values will be pushed between [0,1]
                                         gpl = OpenImageR::rotateFixed(gpl, angle = 90)                # rotate image 90 degrees

                                         graphics::image(gpl, axes = axes, col = grey(seq(0, 1, length = 256)))
                                         if (!is.null(titles)) {
                                           graphics::title(main = titles[x], font.main = 4)
                                         }
                                       })
                                     }

                                   ),

                                   private = list(

                                     flag_filter_bank = NULL,

                                     ROWS = NULL,

                                     COLS = NULL,


                                     # when I threshold the image I need the minority class to take always 1.0 (or 255)
                                     # and the majority class to take 0.0, so that the output images are consistent
                                     # across the grid of images
                                     #---------------------------------------------------------------------------------

                                     adjust_minority_class = function(img) {

                                       if (sum(img) > length(img[img == 0])) {

                                         img[img == 0] = 255

                                         img[img == 1] = 0
                                       }

                                       return(img)
                                     }
                                   )
)



