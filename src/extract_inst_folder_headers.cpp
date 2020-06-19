# include <RcppArmadillo.h>
// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::plugins(openmp)]]
// [[Rcpp::plugins(cpp11)]]

# include "OpenImageRheader.h"


//----------------------------------------------------------------------------------------------------------------------------  Gabor-features [ parallelized & not-parallelized]

// return data for the 'gaborFilterBank'
//

// [[Rcpp::export]]
Rcpp::List Gabor_Filter_Bank(int u, int v, int m, int n, bool plot_data = false) {

  oimageR::Gabor_Features gab_fts;

  gab_fts.gaborFilterBank(u, v, m, n, plot_data);

  oimageR::gabor_filt_bnk gbf_dat = gab_fts.return_gaborFilterBank();

  Rcpp::List res_out = Rcpp::List::create(Rcpp::Named("gaborArray") = gbf_dat.gaborArray);

  if (plot_data) {

    res_out["gabor_imaginary"] = gbf_dat.gabor_imaginary;

    res_out["gabor_real"] = gbf_dat.gabor_real;
  }

  return res_out;
}



// return data for the 'gaborFeatures'
//

// [[Rcpp::export]]
Rcpp::List Gabor_export_Features(arma::Mat<double> img, int d1, int d2, int u, int v, int m, int n, bool downsample_vec = false,

                                 bool plot_data = false, bool normalize_features = false, int threads = 1, bool vectorize_magnitude = true) {

  oimageR::Gabor_Features_Rcpp gab_fts_Rcpp;

  arma::Mat<arma::cx_double> img_cx = gab_fts_Rcpp.conv2complex(img);

  gab_fts_Rcpp.gaborFeaturesRcpp(img_cx, u, v, m, n, d1, d2, downsample_vec, plot_data, normalize_features, threads);

  return gab_fts_Rcpp.return_gaborFeatures(plot_data, vectorize_magnitude);
}



// generate gabor features [ Local Energy, Mean Amplitude ]
//
// it takes a matrix of images (such as the mnist dataset 70000 x 784) converts each row to an image (28 x 28),
// utilizes the 'gaborFeatures' function for each image and computes the features based on the 'gabor_features' method.
//

// [[Rcpp::export]]
Rcpp::List Gabor_generate(arma::Mat<double>& img_data, int img_nrow, int img_ncol, int d1, int d2, int u, int v, int m, int n,

                          bool downsample_vec = false, bool normalize_features = false, int threads = 1) {

  #ifdef _OPENMP
  omp_set_num_threads(threads);
  #endif

  int num_features = 2;                                                      // here I use 2 features [ local-energy, mean-aptitude ] ; modify this in case that I add other features - in both classes - besides those two

  arma::Mat<double> res_out(img_data.n_rows, (u * v) * num_features, arma::fill::zeros);

  arma::Mat<double> res_out_magnitude(img_data.n_rows, u * img_data.n_cols, arma::fill::zeros);

  unsigned int i, k, f;

  #ifdef _OPENMP
  #pragma omp parallel for schedule(static) shared(img_data, img_ncol, img_nrow, n, m, v, u, normalize_features, downsample_vec, d2, d1, res_out, res_out_magnitude, threads) private(i,k,f)
  #endif
  for (i = 0; i < img_data.n_rows; i++) {

    oimageR::Gabor_Features gab_fts;

    oimageR::gabor_feats_obj concat = gab_fts.inner_func_gabor(i, img_data, img_nrow, img_ncol, d1, d2, u, v, m, n, downsample_vec, normalize_features);

    if (!(concat.gabor_features.empty() || concat.gabor_features_Magn.empty())) {

      for (k = 0; k < concat.gabor_features.n_cols; k++) {

        #ifdef _OPENMP
        #pragma omp atomic write
        #endif
        res_out(i,k) = concat.gabor_features(0,k);                           // 'concat.gabor_features' is a single-row matrix
      }

      for (f = 0; f < concat.gabor_features_Magn.n_cols; f++) {

        #ifdef _OPENMP
        #pragma omp atomic write
        #endif
        res_out_magnitude(i,f) = concat.gabor_features_Magn(0,f);
      }
    }
  }

  return Rcpp::List::create(Rcpp::Named("magnitude") = res_out_magnitude, Rcpp::Named("energy_aptitude") = res_out);
}


//----------------------------------------------------------------------------------------------------------------------------  Image-Hashing


// average-hash-string function
//

// [[Rcpp::export]]
arma::mat average_hash_string(arma::mat gray_image, int hash_size = 8, std::string resize_method = "nearest") {

  oimageR::Image_Hashing ImgHash;
  return ImgHash.average_hash_string(gray_image, hash_size, resize_method);
}


// average-hash-binary function
//

// [[Rcpp::export]]
arma::rowvec average_hash_binary(arma::mat gray_image, int hash_size = 8, std::string resize_method = "nearest") {

  oimageR::Image_Hashing ImgHash;
  return ImgHash.average_hash_binary(gray_image, hash_size, resize_method);
}


// phash-string function
//

// [[Rcpp::export]]
arma::mat phash_string(arma::mat gray_image, int hash_size = 8, int highfreq_factor = 4, std::string resize_method = "nearest") {

  oimageR::Image_Hashing ImgHash;
  return ImgHash.phash_string(gray_image, hash_size, highfreq_factor, resize_method);
}


// phash-binary function
//

// [[Rcpp::export]]
arma::rowvec phash_binary(arma::mat gray_image, int hash_size = 8, int highfreq_factor = 4, std::string resize_method = "nearest") {

  oimageR::Image_Hashing ImgHash;
  return ImgHash.phash_binary(gray_image, hash_size, highfreq_factor, resize_method);
}


// binary-to-hex function
//

// [[Rcpp::export]]
std::string binary_to_hex(arma::mat x) {

  oimageR::Image_Hashing ImgHash;
  return ImgHash.binary_to_hex(x);
}


// dhash-string function
//

// [[Rcpp::export]]
arma::mat dhash_string(arma::mat gray_image, int hash_size = 8, std::string resize_method = "nearest") {

  oimageR::Image_Hashing ImgHash;
  return ImgHash.dhash_string(gray_image, hash_size, resize_method);
}


// dhash-binary function
//

// [[Rcpp::export]]
arma::rowvec dhash_binary(arma::mat gray_image, int hash_size = 8, std::string resize_method = "nearest") {

  oimageR::Image_Hashing ImgHash;
  return ImgHash.dhash_binary(gray_image, hash_size, resize_method);
}


// hash-image function
//

// [[Rcpp::export]]
arma::mat hash_image(arma::mat& x, int new_width, int new_height, std::string& resize_method, int hash_size = 8, int highfreq_factor = 4, int method = 1, int threads = 1) {

  oimageR::Image_Hashing ImgHash;
  return ImgHash.hash_image(x, new_width, new_height, resize_method, hash_size, highfreq_factor, method, threads);
}


// hash-image-cube function
//

// [[Rcpp::export]]
arma::mat hash_image_cube(arma::cube& x, std::string& resize_method, int hash_size = 8, int highfreq_factor = 4, int method = 1, int threads = 1) {

  oimageR::Image_Hashing ImgHash;
  return ImgHash.hash_image_cube(x, resize_method, hash_size, highfreq_factor, method, threads);
}


// hash-image-hex function
//

// [[Rcpp::export]]
std::vector<std::string> hash_image_hex(arma::mat& x, int new_width, int new_height, std::string& resize_method, int hash_size = 8, int highfreq_factor = 4, int method = 1, int threads = 1) {

  oimageR::Image_Hashing ImgHash;
  return ImgHash.hash_image_hex(x, new_width, new_height, resize_method, hash_size, highfreq_factor, method, threads);
}


// hash-image-cube-hex function
//

// [[Rcpp::export]]
std::vector<std::string> hash_image_cube_hex(arma::cube& x, std::string& resize_method, int hash_size = 8, int highfreq_factor = 4, int method = 1, int threads = 1) {

  oimageR::Image_Hashing ImgHash;
  return ImgHash.hash_image_cube_hex(x, resize_method, hash_size, highfreq_factor, method, threads);
}


// https://en.wikipedia.org/wiki/Levenshtein_distance
//

// [[Rcpp::export]]
int levenshtein_dist(std::string s, std::string t) {

  oimageR::Image_Hashing ImgHash;
  return ImgHash.levenshtein_dist(s, t);
}


//----------------------------------------------------------------------------------------------------------------------------   HoG-features


// HOG function
//

// [[Rcpp::export]]
arma::rowvec hog_cpp(arma::mat image, int n_divs = 3, int n_bins = 6) {

  oimageR::HoG_features HGF;
  return HGF.hog_cpp(image, n_divs, n_bins);
}


// conversion from list to array
//

// [[Rcpp::export]]
arma::cube list_2array_convert(Rcpp::List x) {

  oimageR::Image_Hashing ImgHash;
  return ImgHash.list_2array_convert(x);
}


// use this HOG function if the input x is an array of images like the CIFAR-10 data
//

// [[Rcpp::export]]
arma::mat HOG_array(arma::cube& x, int n_divs = 3, int n_bins = 6, int threads = 1) {

  oimageR::HoG_features HGF;
  return HGF.HOG_array(x, n_divs, n_bins, threads);
}


// Use this HOG function if the input x is a matrix like the MNIST data where each row of matrix x is an image of 28x28 dimensions
//

// [[Rcpp::export]]
arma::mat HOG_matrix(arma::mat& x, int height, int width, int n_divs = 3, int n_bins = 6, int threads = 1) {

  oimageR::HoG_features HGF;
  return HGF.HOG_matrix(x, height, width, n_divs, n_bins, threads);
}


//----------------------------------------------------------------------------------------------------------------------------  Utility Functions


// convert an RGB image to Gray
//

// [[Rcpp::export]]
arma::mat Rgb_2gray(arma::cube RGB_image) {

  oimageR::Utility_functions UTLF;
  return UTLF.rgb_2gray_rcpp(RGB_image);
}


// 2-dimensional convolutions of mode "same", "full"
//

// [[Rcpp::export]]
arma::mat conv2d(arma::mat image, arma::mat kernel, std::string mode) {

  oimageR::Utility_functions UTLF;
  return UTLF.conv2d(image, kernel, mode);
}


// 3-dimensional convolutions of mode "same", "full"
//

// [[Rcpp::export]]
arma::cube conv3d(arma::cube image, arma::mat kernel, std::string mode) {

  oimageR::Utility_functions UTLF;
  return UTLF.conv3d(image, kernel, mode);
}


// normalizes 'array' in range [0,1]
//

// [[Rcpp::export]]
arma::cube Normalize_array(arma::cube x) {

  oimageR::Utility_functions UTLF;
  return UTLF.Normalize_array(x);
}


// normalizes 'matrix' in range [0,1]
//

// [[Rcpp::export]]
arma::mat Normalize_matrix(arma::mat x) {

  oimageR::Utility_functions UTLF;
  return UTLF.Normalize_matrix(x);
}


// rotate image using an angle and methods 'nearest', 'bilinear' [ matrix ]
//

// [[Rcpp::export]]
arma::mat rotate_nearest_bilinear(arma::mat& image, double angle, std::string& method, std::string mode, int threads) {

  oimageR::Utility_functions UTLF;
  return UTLF.rotate_nearest_bilinear(image, angle, method, mode, threads);
}


//----------------------------------------------------------------------------------------------------------------
// rotate image using an angle and methods 'nearest', 'bilinear' [ array ]
//

// [[Rcpp::export]]
arma::cube rotate_nearest_bilinear_array_same(arma::cube src, double angle, std::string method, int threads) {

  oimageR::Utility_functions UTLF;
  return UTLF.rotate_nearest_bilinear_array_same(src, angle, method, threads);
}


// [[Rcpp::export]]
arma::cube rotate_nearest_bilinear_array_full(arma::cube src, double angle, std::string method, int threads) {

  oimageR::Utility_functions UTLF;
  return UTLF.rotate_nearest_bilinear_array_full(src, angle, method, threads);
}

//----------------------------------------------------------------------------------------------------------------


// rotate image, specific angles [ matrix ]
//

// [[Rcpp::export]]
arma::mat rotate_rcpp(arma::mat image, int angle) {

  oimageR::Utility_functions UTLF;
  return UTLF.rotate_rcpp(image, angle);
}


// resize an image using nearest neighbor   [ array ]
//

// [[Rcpp::export]]
arma::cube resize_nearest_array(arma::cube image, double width, double height) {

  oimageR::Utility_functions UTLF;
  return UTLF.resize_nearest_array(image, width, height);
}


// function for bilinear interpolation   [ output : array ]
//

// [[Rcpp::export]]
arma::cube bilinear_array(arma::cube image, double width, double height) {

  oimageR::Utility_functions UTLF;
  return UTLF.bilinear_array(image, width, height);
}


// resize an image using nearest neighbor   [ matrix ]
//
// modification of http://stackoverflow.com/questions/1550878/nearest-neighbor-interpolation-algorithm-in-matlab
//

// [[Rcpp::export]]
arma::mat resize_nearest_rcpp(arma::mat image, double width, double height) {

  oimageR::Utility_functions UTLF;
  return UTLF.resize_nearest_rcpp(image, width, height);
}


// main function for bilinear interpolation   [ output : matrix ]
//
// modification of http://stackoverflow.com/questions/26142288/resize-an-image-with-bilinear-interpolation-without-imresize?lq=1
//

// [[Rcpp::export]]
arma::mat resize_bilinear_rcpp(arma::mat image, double width, double height) {

  oimageR::Utility_functions UTLF;
  return UTLF.resize_bilinear_rcpp(image, width, height);
}


// flip 'matrix' [ horizontal, vertical ]
//

// [[Rcpp::export]]
arma::mat im_flip(arma::mat x, int mode = 1) {

  oimageR::Utility_functions UTLF;
  return UTLF.im_flip(x, mode);
}


// flip 'array' [ horizontal, vertical ]
//

// [[Rcpp::export]]
arma::cube im_flip_cube(arma::cube src, int mode = 1) {

 oimageR::Utility_functions UTLF;
  return UTLF.im_flip_cube(src, mode);
}


// zca-whitening  [ single image ]
//
// http://ufldl.stanford.edu/wiki/index.php/Implementing_PCA/Whitening
// http://stackoverflow.com/questions/31528800/how-to-implement-zca-whitening-python
// http://www.cs.toronto.edu/~kriz/learning-features-2009-TR.pdf
// http://stats.stackexchange.com/questions/117427/what-is-the-difference-between-zca-whitening-and-pca-whitening
//

// [[Rcpp::export]]
arma::mat zca_whitening(arma::mat data, int k, double epsilon) {

  oimageR::Utility_functions UTLF;
  return UTLF.zca_whitening(data, k, epsilon);
}


// zca-whitening  [ array ]
//

// [[Rcpp::export]]
arma::cube zca_whiten_cube(arma::cube src, int k, double epsilon) {

  oimageR::Utility_functions UTLF;
  return UTLF.zca_whiten_cube(src, k, epsilon);
}


// dilation or erosion of an image   [ matrix ]
//
// http://www.mathworks.com/help/images/morphological-dilation-and-erosion.html
//

// [[Rcpp::export]]
arma::mat diate_erode(arma::mat& image, arma::rowvec& Filter, int method = 1, int threads = 1) {

  oimageR::Utility_functions UTLF;
  return UTLF.diate_erode(image, Filter, method, threads);
}


// dilation or erosion of an image   [ array ]
//

// [[Rcpp::export]]
arma::cube diate_erode_cube(arma::cube image, arma::rowvec Filter, int method = 1, int threads = 1) {

  oimageR::Utility_functions UTLF;
  return UTLF.diate_erode_cube(image, Filter, method, threads);
}


// translation function    [ matrix ]
//

// [[Rcpp::export]]
arma::mat translation_mat(arma::mat& image, int shift_rows = 0, int shift_cols = 0, double FILL_VALUE = 0.0) {

  oimageR::Utility_functions UTLF;
  return UTLF.translation_mat(image, shift_rows, shift_cols, FILL_VALUE);
}


// function which takes as input a 2-dimensional matrix and performs augmentations
// for arma::uvec crop_height, crop_width in case of empy vectors use in R : numeric(0)
//

// [[Rcpp::export]]
arma::mat augment_transf(arma::mat& image, std::string flip_mode, arma::uvec crop_height, arma::uvec crop_width, double resiz_width = 0.0,

                         double resiz_height = 0.0, std::string resiz_method = "nearest", double shift_rows = 0.0, double shift_cols = 0.0,

                         double rotate_angle = 0.0, std::string rotate_method = "nearest", int zca_comps = 0, double zca_epsilon = 0.0,

                         double image_thresh = 0.0, double pad_shift_value = 0.0) {

  oimageR::Utility_functions UTLF;
  return UTLF.augment_transf(image, flip_mode, crop_height, crop_width, resiz_width, resiz_height, resiz_method, shift_rows, shift_cols,
                             rotate_angle, rotate_method, zca_comps, zca_epsilon, image_thresh, pad_shift_value);
}



// function which takes as input either a 3-dimensional array OR an array of gray-matrices (2-dimensional matrices) and performs augmentations
// the size of BOTH the cube AND the cube.slice(i) is changed dynamically using the .set_size() operator
//

// [[Rcpp::export]]
arma::cube augment_transf_array(arma::cube& image, std::string flip_mode, arma::uvec crop_height, arma::uvec crop_width, arma::rowvec pad_shift_value, double resiz_width = 0.0,

                                double resiz_height = 0.0, std::string resiz_method = "nearest", double shift_rows = 0.0, double shift_cols = 0.0,

                                double rotate_angle = 0.0, std::string rotate_method = "nearest", int zca_comps = 0, double zca_epsilon = 0.0,

                                double image_thresh = 0.0) {

  oimageR::Utility_functions UTLF;
  return UTLF.augment_transf_array(image, flip_mode, crop_height, crop_width, pad_shift_value, resiz_width, resiz_height, resiz_method,
                                   shift_rows, shift_cols, rotate_angle, rotate_method, zca_comps, zca_epsilon, image_thresh);
}


// extention of the previous function (augment_transf_array) by using an Rcpp::List to preprocess and store 3-dimensional arrays
// this function takes a list of arrays as input and after preprocessing (using the augment_transf_array function) it returns an Rcpp::List
//

// [[Rcpp::export]]
Rcpp::List augment_array_list(Rcpp::List x, std::string flip_mode, arma::uvec crop_height, arma::uvec crop_width, arma::rowvec pad_shift_value, double resiz_width = 0.0,

                              double resiz_height = 0.0, std::string resiz_method = "nearest", double shift_rows = 0.0, double shift_cols = 0.0,

                              double rotate_angle = 0.0, std::string rotate_method = "nearest", int zca_comps = 0, double zca_epsilon = 0.0,

                              double image_thresh = 0.0) {

  oimageR::Utility_functions UTLF;
  return UTLF.augment_array_list(x, flip_mode, crop_height, crop_width, pad_shift_value, resiz_width, resiz_height, resiz_method,
                                 shift_rows, shift_cols, rotate_angle, rotate_method, zca_comps, zca_epsilon, image_thresh);
}


// returns minimum and maximum values of matrix
//

// [[Rcpp::export]]
Rcpp::List MinMaxMatrix(arma::mat x) {

  oimageR::Utility_functions UTLF;
  return UTLF.MinMaxMatrix(x);
}


// returns minimum and maximum values of array
//

// [[Rcpp::export]]
Rcpp::List MinMaxArray(arma::cube x) {

  oimageR::Utility_functions UTLF;
  return UTLF.MinMaxArray(x);
}


// returns 'min' or 'max' values of an array in form of a vector
//

// [[Rcpp::export]]
arma::rowvec Array_range(arma::cube x, int mode = 1) {

  oimageR::Utility_functions UTLF;
  return UTLF.Array_range(x, mode);
}


// similar to the 'y' matrix of the matlab 'meshgrid' function
//

// [[Rcpp::export]]
arma::mat meshgrid_y(int rows, int cols) {

  oimageR::Utility_functions UTLF;
  return UTLF.meshgrid_y(rows, cols);
}


// similar to the 'x' matrix of the matlab 'meshgrid' function
//

// [[Rcpp::export]]
arma::mat meshgrid_x(int rows, int cols) {

  oimageR::Utility_functions UTLF;
  return UTLF.meshgrid_x(rows, cols);
}



//---------------------------------------------------------------------------------------------------------------------------- SLIC, SLICO

// RGB to LAB colour type conversion [ see the 'snic' superpixel implementation -- slightly modified to return data ]
// https://www.epfl.ch/labs/ivrl/research/snic-superpixels/
//

/**
 * =================================================================================
 * snic_mex.cpp
 *
 * AUTORIGHTS
 * Copyright (C) 2016 Ecole Polytechnique Federale de Lausanne (EPFL), Switzerland.
 * Created by Radhakrishna Achanta on 05/November/16 (firstname.lastname@epfl.ch)
 *
 * Code released for research purposes only. For commercial purposes, please
 * contact the author.
 * =================================================================================
 *
 * *Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met

 * Redistributions of source code must retain the above copyright
 notice, this list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright
 notice, this list of conditions and the following disclaimer in the
 documentation and/or other materials provided with the distribution.
 * Neither the name of EPFL nor the names of its contributors may
 be used to endorse or promote products derived from this software
 without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND ANY
 EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 DISCLAIMED. IN NO EVENT SHALL THE REGENTS AND CONTRIBUTORS BE LIABLE FOR ANY
 DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

// [[Rcpp::export]]
arma::cube rgbtolab(arma::cube rgb) {

  arma::cube res(rgb.n_rows, rgb.n_cols, rgb.n_slices);

  int i; int sR, sG, sB;
  double R,G,B;
  double X,Y,Z;
  double r, g, b;
  const double epsilon = 0.008856;	//actual CIE standard
  const double kappa   = 903.3;		//actual CIE standard

  const double Xr = 0.950456;	//reference white
  const double Yr = 1.0;		//reference white
  const double Zr = 1.088754;	//reference white
  double xr,yr,zr;
  double fx, fy, fz;
  double lval,aval,bval;
  int ROWS = rgb.n_rows;
  int COLS = rgb.n_cols;

  for(i = 0; i < ROWS; i++) {
    for (int j = 0; j < COLS; j++) {

      sR = rgb.slice(0)(i,j); sG = rgb.slice(1)(i,j); sB = rgb.slice(2)(i,j);
      R = sR/255.0;
      G = sG/255.0;
      B = sB/255.0;

      if(R <= 0.04045)	r = R/12.92;
      else				r = pow((R+0.055)/1.055,2.4);
      if(G <= 0.04045)	g = G/12.92;
      else				g = pow((G+0.055)/1.055,2.4);
      if(B <= 0.04045)	b = B/12.92;
      else				b = pow((B+0.055)/1.055,2.4);

      X = r*0.4124564 + g*0.3575761 + b*0.1804375;
      Y = r*0.2126729 + g*0.7151522 + b*0.0721750;
      Z = r*0.0193339 + g*0.1191920 + b*0.9503041;

      //------------------------
      // XYZ to LAB conversion
      //------------------------
      xr = X/Xr;
      yr = Y/Yr;
      zr = Z/Zr;

      if(xr > epsilon)	fx = pow(xr, 1.0/3.0);
      else				fx = (kappa*xr + 16.0)/116.0;
      if(yr > epsilon)	fy = pow(yr, 1.0/3.0);
      else				fy = (kappa*yr + 16.0)/116.0;
      if(zr > epsilon)	fz = pow(zr, 1.0/3.0);
      else				fz = (kappa*zr + 16.0)/116.0;

      lval = 116.0*fy-16.0;
      aval = 500.0*(fx-fy);
      bval = 200.0*(fy-fz);

      res.slice(0)(i,j) = lval; res.slice(1)(i,j) = aval; res.slice(2)(i,j) = bval;
    }
  }

  return res;
}


// RGB to HSV colour type conversion
//  Reference : "Analytical Study of Colour Spaces for Plant Pixel Detection", Pankaj Kumar and Stanley J. Miklavcic, 2018, Journal of Imaging [ page 3 of 12 ]
//

// [[Rcpp::export]]
arma::cube RGB_to_hsv(arma::cube x) {

  oimageR::Utility_functions UTLF;
  return UTLF.RGB_to_HSV(x);
}


// interface-function for the 'slic' and 'slico' superpixels
// the 'output_image' parameter should be a binary file such as "/my_directory/image.bin"
//

// [[Rcpp::export]]
Rcpp::List interface_superpixels(arma::cube input_image, std::string method = "slic", int num_superpixel = 200,
                                 double compactness_factor = 20, bool return_slic_data = false, bool return_lab_data = false, 
                                 bool return_labels = false, std::string write_slic = "", bool verbose = false) {

  oimageR::Utility_functions UTLF;
  return UTLF.interface_superpixels(input_image, method, num_superpixel, compactness_factor, return_slic_data, return_lab_data,
                                    return_labels, write_slic, verbose);
}


// superpixels bounding-box
//

// [[Rcpp::export]]
Rcpp::List spix_bbox(arma::mat& spix_labels, bool non_overlapping_superpixels = false) {
  
  oimageR::Utility_functions UTLF;
  return UTLF.spix_bbox(spix_labels, non_overlapping_superpixels);
}


// bounding box for a subset of superpixels
//

// [[Rcpp::export]]
std::vector<int> spix_bbox_vector(arma::mat& spix_labels, arma::rowvec spix_labels_vec) {
  
  oimageR::Utility_functions UTLF;
  return UTLF.spix_bbox_vector(spix_labels, spix_labels_vec);
}



// padding of a matrix
//

// [[Rcpp::export]]
Rcpp::List pad_matrix(arma::mat &x, int new_rows, int new_cols, double fill_value = 0.0) {
  
  oimageR::Utility_functions UTLF;
  return UTLF.pad_matrix(x, new_rows, new_cols, fill_value);
}



// function to load 2- or 3-dimensional data [ in combination with the "interface_superpixels()"
// if the "write_slic" parameter is not set to "" ]
//

// [[Rcpp::export]]
Rcpp::List LOAD_data(std::string write_slic, std::string type = "2d") {    // type can be either '2d' or '3d'

  Rcpp::List out;
  arma::cube im3d;
  arma::mat im2d;
  if (type == "2d") {
    im2d.load(write_slic);
    out.push_back(im2d);
  }
  else if (type == "3d") {
    im3d.load(write_slic);
    out.push_back(im3d);
  }
  else {
    Rcpp::stop("The 'type' parameter can be either '2d' or '3d'!");
  }
  
  return out;
}


//----------------------------------------------------------------------------------------------------------------------------
