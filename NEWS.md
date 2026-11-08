
## OpenImageR 1.3.1

### Breaking changes

These functions now return different values for inputs that worked before, because the old values were wrong:

* *gabor_feature_engine()* of *GaborFeatureExtract*: the first half of the columns of *energy_aptitude* now holds the local energy and the second half the mean amplitude, as in *gabor_feature_extraction()*. Before, the first half held the mean amplitude and the second half was all zeros. Recompute saved gabor feature matrices. (F1)
* *resizeImage(method = 'nearest')*: rows and columns are now sampled evenly, floor(i / scale), and the last row and column are kept (10 -> 5 picks rows 1 3 5 7 9, before 1 2 4 6 7), and a resize to the same size keeps every pixel (a [0,1] image is still multiplied by 255 unless *normalize_pixels = TRUE*). *Augmentation()* resizes only when asked to, so without arguments it returns the input. *average_hash()*, *phash()*, *dhash()*, *invariant_hash()* and *hash_apply()* with *resize = 'nearest'* change as well. Redo resized outputs and recompute stored hash values. (F2)
* *rotateImage()*: pixel centres are now rotated, so 'nearest' in mode 'same' is exact at 0, 90, 180, 270 and 360 degrees (0 cells differ on 8 x 8, 20 x 20 and 21 x 21 images; 20 x 20 at 180 degrees: 123 cells before), and in mode 'full' it keeps every pixel of a square image at 90, 180 and 270 degrees (8 x 8, 13 x 13, 20 x 20). 'nearest' reads the first row and column, and 'bilinear' reads the last row and column. 'bilinear' no longer leaves black pixels at these angles (100 x 100 image of ones: 0 cells equal to 0 at 90, 180, 270 and 360 degrees, before 199 at each angle). With the bilinear weights of F45 (below), 'bilinear' in mode 'same' is exact at these angles too (in mode 'full' an odd canvas difference puts pixel centres on half positions: 8 x 8 at 90 degrees differs from 'nearest' by up to 4.5, REVIEW2_2026-11-08_1512.md). Rotations in *Augmentation()* and *invariant_hash()* change as well. Redo rotated outputs and recompute stored *invariant_hash()* values. (F3)
* *RGB_to_HSV()*: black pixels now have hue 0 and saturation 0 (before, hue 1.570796 and saturation NaN). Redo saved HSV data of images with black pixels. (F24)
* *rotateImage(method = 'bilinear')*: the interpolation now reads all four neighbours (the one below right was never read) and uses real bilinear weights, so a whole position gives the pixel itself and right angles no longer shift the image (centred disk at 0 degrees: shift -0.25, -0.25 px before, 0 now; 20 x 20 ramp at 0 degrees: up to 5.25 from the input before, 0 now). Every 'bilinear' rotation changes (runif 20 x 20 at 30 degrees, mode 'full': sum 197.73597 at version 1.3.0, 196.0985 now), and so do the rotations of *invariant_hash()* and of *Augmentation(rotate_method = 'bilinear')*. Redo rotated outputs and recompute stored *invariant_hash()* values. (F6, F45)
* *Augmentation()*: the rotation now uses *rotate_method*; before, it used *resiz_method* and ignored *rotate_method*. Results change when the two differ. Redo augmented data made with different methods. (F7)
* *Augmentation(image_thresh = )*: values above the threshold now become 1 and the others 0, as in *image_thresholding()*; before, the output was inverted. Redo thresholded augmented data. (F8)
* *RGB_to_Lab()*: an image with all values in [0, 1] (for instance the output of *readImage()*) is now multiplied by 255 before the conversion; before, its values were truncated to integers and L was 0 almost everywhere (1.png: 2 distinct L values before, 1013 now). 0-255 inputs with a value above 1 give the same results as before (they are still truncated to integers); a 0-255 image whose values are only 0 and 1 is now treated as a [0, 1] image (a pixel of value 1: L 0.274176 at version 1.3.0, 100 now). NA values no longer decide the range and stay NA (2 x 1 image with one NA: L 0 -125219318 at version 1.3.0, 32.80615 NA now). Redo Lab data of [0, 1] images. (F9)
* *edge_detection()*, *NormalizeObject()*, *norm_matrix_range()*: a flat (constant) input now normalises to 0 (*norm_matrix_range()*: to *min_value*) instead of NaN, so the edges of a flat image are 0 (no edges). Inputs with a range do not change, and an input with an NA still returns NA. Redo saved edge maps or normalised data of flat images, and drop any NaN handling written for them. (F11)
* *edge_detection(method = 'LoG')*: the Gaussian kernel now uses the given *range_gauss*; before, it always used 2. Results change only for a non-default *range_gauss*; redo LoG edge maps computed with one. (F12)
* *rotateImage(mode = 'full')*: for a non-square image both sides of the output are now floor(sqrt(rows^2 + cols^2)), so the rotated image is no longer clipped (5 x 8 at 90 degrees: 7 x 11 keeping 35 of 40 values before, 9 x 9 keeping 40 now). The output dimensions of non-square images change; square images keep their size. Redo code that relies on these dimensions. (F56)

### Bug fixes

* *superpixels()*: a side of the image shorter than, or between 1.5 and 2 times, the step size left pixels outside every seed window, which crashed the R session with 'slic' and 'slico'. These inputs now return labels. Results of inputs that worked do not change: in 2304 random and fixed cases, the 888 that gave a result at version 1.3.0 have the same labels and slic data, and the 1015 that crashed do not crash now. (F41, F42)
* *superpixels()*: a 'superpixel' value that is not a single number between 1 and the integer maximum now gives a clear R error. (F41)
* *superpixels(method = "slico")*: the errors about the 'K' parameter and 'clustersize[k]' are gone and these inputs now return labels; the number of superpixels can differ from the one asked (runif images, seeds 1 to 3: 523 x 645: 11 labels for 10, 202 for 200; 10 x 1200: 30 to 33 for 10; 1 x 1200 and 2 x 1200: 1 for 10; 30 x 40: 541 to 554 for 1000, 247 to 272 for 2000). Results of inputs that worked do not change (same 2304 cases). (F43)
* *superpixels(method = "slic")*: a 'compactness' of NA, NaN, Inf or 1e308 no longer crashes the R session; these values now return labels (30 x 40 image, superpixel = 20: 15 labels). In 450 cases with other values, labels and slic data did not change. (F41)
* *ZCAwhiten()*: *k* is now checked against the number of rows of the image (the covariance is rows x rows): a wide image with *k* above its rows gets a clear R error instead of an Armadillo index error, and a tall image accepts *k* up to its rows. Results of inputs that worked do not change. (F10)
* *superpixels()*: an invalid *method* (or any error after the pixel buffers were allocated) no longer leaks memory (40 calls on a 600 x 600 image grew the process by 107.2 MB before). Results do not change. (F13)
* *imageShow()* with a file path no longer creates or overwrites *file_path* in the global environment; the shiny app gets the path through a shiny option. (F15)
* *plot_gabor()* and *plot_multi_images()* of *GaborFeatureExtract* restore the graphical parameters (*par()*) they change. (F17)
* *hash_apply()*: an error inside its parallel loops (for instance a 'bilinear' resize of images with 1 column) is now an R error raised before the parallel loop starts; before, it ended the R session (5000 images of 3 x 1 with 8 threads, 40 calls per process: 3 of 3 processes ended abnormally before, 0 of 5 now; one machine). Results do not change. (F44)
* *rotateImage()* and *rotateFixed()*: their help pages now say that they turn in opposite directions (*rotateImage()* counter-clockwise, *rotateFixed()* clockwise, as displayed with the first row at the top). Neither function changed. (F55)
* *hash_apply()* with *threads* above 1 no longer ends the R session when a resize warns (for instance 49 x 49 images with *hash_size = 8*): the warning came from the parallel worker threads (3000 images, 2 threads: 3 of 3 processes crashed before, 0 of 3 now; one machine). Results do not change; with *threads* above 1 only the first image still warns. (F53)
* *Augmentation()* no longer writes its result into the input matrix (after *Augmentation(y, image_thresh = 0.5)* the matrix *y* held the thresholded values). The returned value does not change; arrays and lists were not affected. (F57)
* *HOG_apply()* of images with a NaN pixel now gives the R error "Mat::operator(): index out of bounds", as *HOG()* of one image does, instead of ending the R session (also with 1 thread). Results for images without NaN do not change. (F62)

### Performance

* *padding()*: 1024 x 1024 to 2048 x 2048 went from 11.6 - 24.1 s to 0.07 - 0.09 s (3 runs on one machine). Results do not change. (F4)
* *resizeImage(method = 'bilinear')*: gray 1024 to 2048 went from 6.0 s to 0.2 s, RGB from 16.4 s to 0.87 s, and the peak memory of the measuring script from 1087 MB to 416 MB (one machine). Results do not change: 1237 cases compared with version 1.3.0 gave the same 943 results and the same 294 errors. A single row reduced to half its columns or less keeps working (1 x 6 to 1 x 3); a single column, any other single row and a 0-row image stay errors, most with a clearer message. (F5)
* *hash_apply(mode = 'hash')* now runs in parallel (the whole loop was in a critical section): 600 images of 64 x 64, phash, 4 threads: 1.546 s before, 0.430 s now (1 thread: 1.528 s and 1.627 s; one machine). Hashes do not change. (F14)
* *gabor_feature_extraction()* no longer keeps the complex convolution results it never returns: peak memory of a 256 x 256 image with 5 scales and 8 orientations 83.6 MB before, 36.9 MB now (one machine). Results do not change. (F16)
* *ZCAwhiten()* uses a symmetric eigen-decomposition and two thin products instead of an svd and a rows x rows matrix: 512 x 512, k = 20: 0.811 s before, 0.427 s now (one machine). Results differ only by rounding (largest absolute difference 9.3e-14 on a 256 x 256 image, k = 50). (F18)
* *dilationErosion()* uses two separable passes without a matrix per pixel: 1024 x 1024 with a 3 x 3 filter 0.481 s before, 0.063 s now (one machine, debug build). Results do not change (identical on 4 cases; a NaN cell of the window is still skipped, as before: 12 x 10 image with 15 NaN, 8 filter and method cases identical to version 1.3.0). (F19)
* *phash()* / *hash_apply()*: the DCT computes its cosine vectors once per call and the matrix-to-image conversion copies the data directly: phash of 2000 images of 28 x 28 3.026 s before, 0.66 s now (one machine, debug build). Hashes do not change. (F20)


## OpenImageR 1.3.0

* I updated the README.md file related to "[[Rcpp::plugins(cpp11)]]", which was removed in the previous version
* I fixed a bug in the internal *'func_chech_range()'* function (see issue: https://github.com/mlampros/OpenImageR/issues/28)


## OpenImageR 1.2.9

* I added an exception in the test-file *"test-norm_matrix_range.R"* related to Mac OSx and Power-PC (see issue: https://github.com/mlampros/OpenImageR/issues/27)
* I removed the "CXX_STD = CXX11" from the "Makevars" files, and the "[[Rcpp::plugins(cpp11)]]" from the "extract_inst_folder_headers.cpp" file due to the following NOTE from CRAN, "NOTE Specified C++11: please drop specification unless essential" (see also: https://www.tidyverse.org/blog/2023/03/cran-checks-compiled-code/#note-regarding-systemrequirements-c11)
* I removed the '-mthreads' compilation flags from the Makevars.win file because I received a CRAN NOTE


## OpenImageR 1.2.8

* I added the *'verify_image_extension()'* function to verify that the image path file extensions are supported from the OpenImageR package (see issue: https://github.com/mlampros/OpenImageR/issues/25) 


## OpenImageR 1.2.7

* I updated the output error message in all cases of 3-dimensional images where the third dimension had to be equal to 3


## OpenImageR 1.2.6

* The "delationErosion" function will be replaced with the "dilationErosion" function in version 1.3.0, because the name of the algorithm was wrong ('delation' actually corresponds to 'dilation'). See issue https://github.com/mlampros/OpenImageR/issues/24


## OpenImageR 1.2.5

* I fixed the (debian, fedora) *clang14* warning: "use of bitwise '&' with boolean operands"


## OpenImageR 1.2.4

* I fixed a *clang-UBSAN* warning related to the *warpAffine()* and *warpAffine_2d()* functions of the *OpenImageRheader.h* file (this warning was visible in the checks of the *fastGLCM* R package)


## OpenImageR 1.2.3

* I've added the *getAffineTransform()* and *warpAffine()* functions
* I've added the *Warp_Affine* vignette explaining the functionality of the included functions
* I've added tests for the *getAffineTransform()* and *warpAffine()* functions
* I updated the *COPYRIGHTS* and *CITATION* files
* I modified the *imageShow()* function by adding the *clear_viewer* parameter. If this parameter is TRUE then the previous image will be removed in the viewer before displaying the next one


## OpenImageR 1.2.2

* I've modified the *'plot_multi_images()'* method of the *GaborFeatureExtract* R6 class by adding the *titles* and the *axes* parameters.


## OpenImageR 1.2.1

* I've fixed an mistake in the code of the *RGB_to_HSV()* function in the *inst/include/OpenImageRheader.h* file related to an [StackOverflow issue](https://stackoverflow.com/q/70183472)
* I've added the *'normalize_pixels'* parameter to the *'resizeImage()'* function and I've set it to FALSE so that output pixel values are not normalized by default


## OpenImageR 1.2.0

* I've modified the *resize_nearest_rcpp()* function in the *inst/include/OpenImageRheader.h* file by adding a warning in case that the input parameters *width* and *height* do not match with the output *width* and *height*. See the [issue 21](https://github.com/mlampros/OpenImageR/issues/21) in the OpenImageR Github repository. Moreover, I've added tests for the case that the *resizeImage(method = "nearest")* function gives a warning.


## OpenImageR 1.1.9

* I fixed the error in the CRAN results due to mistakes in creation of matrices and arrays in the test (testthat) files.


## OpenImageR 1.1.8

* I've added the *CITATION* file in the *inst* directory


## OpenImageR 1.1.7

* I fixed an error in the *'equal_spaced'* version of the *crop_image_secondary()* function
* I fixed invalid URL's (both in R scripts and vignettes)
* I added the *superpixel_bbox()* function which returns the bounding box boundaries for the superpixels
* I added the *superpixel_bbox_subset()* function which returns the bounding box boundaries of a subset of superpixels
* I added the *padding()* function, which allows a user to pad / extend a matrix or array using a specified value


## OpenImageR 1.1.6

* I renamed the *load_3d_binary* to *load_binary* and now a user can load either a 2- or a 3-dimensional object. I also fixed a bug in the function.
* I modified the *superpixels* function, so that it can take 2-dimensional objects too (matrices) besides 3-dimensional images (no need to convert to 3-dimensional outside of the function).
* I added the parameter *vectorize_magnitude* to the *gabor_feature_extraction* method of the *GaborFeatureExtract* R6 class. In case that the *vectorize_magnitude* parameter is set to FALSE the user will receive the magnitude in form of a matrix ( for instance if the *scales* parameter is set to 2 and the (rows, cols) of the image are (128, 128) then the resulted magnitude will be a list of length 2 where each sublist will be a matrix with dimensions (128, 128) (pixel-wise magnitude values) )
* I modified the *meshgrid_x()* and *meshgrid_y()* Rcpp functions and now they return faster as before
* I modified the check-input-data-cases for the *RGB_to_Lab* and *RGB_to_HSV*
* I fixed the *"failure: the condition has length > 1"* CRAN error which appeared mainly due to the misuse of the base *class()* function in multiple code snippets in the package (for more info on this matter see: https://developer.r-project.org/Blog/public/2019/11/09/when-you-think-class.-think-again/index.html)


## OpenImageR 1.1.5

* I renamed the internal function *norm_range_gauss* to *norm_matrix_range*. Now, the *norm_matrix_range* function is accessible when loading the OpenImageR package. Moreover, I excluded rounding [ round() ], which occured by default in the *norm_range_gauss* function.


## OpenImageR 1.1.4

* I fixed the duplicated vignette title warning


## OpenImageR 1.1.3

* I fixed a documentation error as suggested in [issue 11](https://github.com/mlampros/OpenImageR/issues/11) of the OpenImageR Github repository (converted radians to degrees in all scripts of the OpenImageR package).


## OpenImageR 1.1.2

* I converted the OpenImageR package to a header-only package, so that Rcpp functions can be called from another R package.
* I added the *SLIC* and *SLICO* superpixel algorithms as header files and as a function (*superpixels()*) which can be called from within R.
* I added the *load_3d_binary()* function in case that a user intends to load 3d-image data from a binary file (as is the case when a user writes data when using the *superpixels()* function).
* I added the RGB (Red-Green-Blue) to LAB (Lightness, A-colour-dimension, B-colour-dimension) colour conversion (*RGB_to_Lab()*).
* I added the RGB (Red-Green-Blue) to HSV (Hue, Saturation, Value) colour conversion (*RGB_to_HSV()*).
* I added the vignette "Image_segmentation_superpixels_clustering.Rmd"


## OpenImageR 1.1.1

* I modified the *rotate_nearest_bilinear* Rcpp function in the *utils.cpp* file to fix the ASAN-gcc error.


## OpenImageR 1.1.0

* I fixed the ASAN-gcc error.
* I commented 3 test cases for the Solaris OS, otherwise it gives an error (see the comments in the *test-gabor.R* tests file).
* I updated the Vignette *Gabor Feature extraction*.


## OpenImageR 1.0.9

I added the *GaborFeatureExtract* R6 class, which as the name suggests extracts Gabor features from images (I updated the documentation and Vignettes too)


## OpenImageR 1.0.8

I added the *DARMA_64BIT_WORD* flag in the Makevars file to allow the package processing big datasets


## OpenImageR 1.0.7

I removed the *threads* parameter from the *Augmentation* function as there is no significant improvement (in execution speed) between a threaded and a non-threaded version.
I modified the *OpenMP* clauses of the .cpp files to address the ASAN errors.


## OpenImageR 1.0.6

I fixed a bug in the *translation_mat* function of the *utils.cpp* source file (https://github.com/mlampros/OpenImageR/issues/6 and https://github.com/mlampros/OpenImageR/issues/7). Moreover, I added the *padded_value* parameter in the *translation* and *Augmentation* R functions to allow the user to adjust the shifted values ( in case that *shift_rows* or *shift_cols* is not 0 ). The *padded_value* parameter can take either a numeric value or a numeric vector depending on the dimensions of the image. Previously, the shifted values were replaced by default with 0.


## OpenImageR 1.0.5

I fixed a bug in the *dhash* function according to the [Changelog](https://github.com/JohannesBuchner/imagehash) of the imagehash python library. Moreover, I added a function to round float numbers in the *image_hashing.cpp* source file, otherwise by creating hash values using the *bilinear* interpolation method the returned values are incorrect.


## OpenImageR 1.0.4

I modified the R script files to accept extension types similar to .tiff such as .tif, .TIFF or .TIF


## OpenImageR 1.0.3

I modified the Augmentation function to correct an issue which occured due to the zca_comps parameter


## OpenImageR 1.0.2

I did OpenMP optional for all cpp files due to the fact that the OSX OS does not currently support openMP ( r-devel-osx-x86_64-clang )


I removed the floor() function from the lines 45, 46 of the hog_features.cpp, because otherwise Solaris OS throws the following error:

* error: call of overloaded ‘floor(int)’ is ambiguous


I can not fix the following error in the hog_features.cpp file for the r-oldrel-windows platform as it has either to do with the RcppArmadillo version or is GCC specific,

* error: could not convert '{{-0x00000000000000001, 0, 1}, {-0x00000000000000002, 0, 2}, {-0x00000000000000001, 0, 1}}' from '<brace-enclosed initializer list>' to 'arma::mat {aka arma::Mat<double>}'

Platforms, which support newer versions of GCC (GCC >= 4.9.0) and a recent version of RcppArmadillo (>= 0.7)  do not throw such an error.


## OpenImageR 1.0.1

I modified the Makevars files, so that OpenImageR can be installed error-free in the following OS's,

* r-devel-osx-x86_64-clang
* r-patched-solaris-sparc
* r-patched-solaris-x86
* r-oldrel-windows-ix86+x86_64


## OpenImageR 1.0.0

