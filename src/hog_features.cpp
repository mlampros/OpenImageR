# include <RcppArmadillo.h>
// [[Rcpp::plugins(openmp)]]
// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::plugins(cpp11)]]


/**
 * Copyright (c) 2012, Sight Machine
 *
 * All rights reserved.
 *
 * https://github.com/sightmachine/SimpleCV
 *
 **/


#include <math.h>

#ifdef _OPENMP
#include <omp.h>
#endif

#include "utils.h"


/*
 * I used and modified the findHOGFeatures() function of the SimpleCV computer vision platform
 *
 * [ The HOG schema was taken from "https://github.com/lastlegion/SimpleCV/blob/develop/SimpleCV/ImageClass.py" ]
 *
 * please consult the COPYRIGHTS file
 *
 */


// HOG function

// [[Rcpp::export]]
arma::rowvec hog_cpp(arma::mat image, int n_divs = 3, int n_bins = 6) {

  int n_HOG = n_divs * n_divs * n_bins;                                  // Size of HOG vector

  arma::mat sobel_x = { {-1, 0, 1}, {-2, 0, 2}, {-1, 0, 1} };
  arma::mat sobel_y = { {-1, -2, -1}, {0, 0, 0}, {1, 2, 1} };

  arma::mat Ix = conv2(image, sobel_x, "same");
  arma::mat Iy = conv2(image, sobel_y, "same");

  int height = image.n_rows;
  int width = image.n_cols;

  Ix = Ix.t();
  Iy = Iy.t();

  int cellx = width / n_divs;                                           // height of each cell(division)
  int celly = height / n_divs;                                          // width of each cell(division)

  int img_area = height * width;                                        // Area of image

  double BIN_RANGE = (2 * arma::datum::pi) / n_bins;                    // Range of each bin

  arma::mat angles(Iy.n_rows, Iy.n_cols);                               // assume Ix, Iy have the same dimensions

  for (unsigned int i = 0; i < Iy.n_rows; i++) {

    for (unsigned int j = 0; j < Iy.n_cols; j++) {

      angles(i,j) = atan2(Iy(i,j), Ix(i,j));
    }
  }

  arma::mat magnit = arma::pow(arma::pow(Ix, 2.0) + arma::pow(Iy, 2.0), 0.5);

  arma::vec HOG = arma::zeros(n_HOG + 1);                                 // Initialize output HOG vector [ I added +1 otherwise it raises Error in some cases [ index = " (m * n_divs + n) * n_bins + nth_bin" == length(HOG) ]

  for (int m = 0; m < n_divs; m++) {

    for (int n = 0; n < n_divs; n++) {

      for (int i = 0; i < cellx; i++) {

        for (int j = 0; j < celly; j++) {

          double grad = magnit(m * cellx + i, n * celly + j);

          double norm_grad = grad / img_area;

          double angle = angles(m * cellx + i, n * celly + j);

          if (angle == arma::datum::nan) {                                  // exception to avoid error : in case of NA's overwrite angle with a small value close to zero

            angle = 0.000001;
          }

          if (angle < 0) {

            angle = angle + 2 * arma::datum::pi;
          }

          int nth_bin = floor(angle / BIN_RANGE);

          HOG((m * n_divs + n) * n_bins + nth_bin) += arma::as_scalar(norm_grad);
        }
      }
    }
  }

  return arma::conv_to< arma::rowvec >::from(HOG.subvec(0, HOG.n_elem - 2));                      // I added an extra zero-value in the HOG vector (to avoid potential error), which I exclude here
}



// secondary function for the 'HOG_matrix' function
//

// [[Rcpp::export]]
arma::rowvec inner_hog_mat(arma::mat& x, unsigned int i, int height, int width, int n_divs, int n_bins) {

  arma::mat tmp = vec2mat(x.row(i), height, width);
  
  arma::rowvec tmp_hog = hog_cpp(tmp, n_divs, n_bins);
  
  return tmp_hog;
}



// Use this function if the input x is a matrix like the MNIST data where each row of matrix x is an image of 28x28 dimensions

// [[Rcpp::export]]
arma::mat HOG_matrix(arma::mat& x, int height, int width, int n_divs = 3, int n_bins = 6, int threads = 1) {

  #ifdef _OPENMP
  omp_set_num_threads(threads);
  #endif

  arma::mat out(x.n_rows, n_divs * n_divs * n_bins);

  unsigned int i,j;

  #ifdef _OPENMP
  #pragma omp parallel for schedule(static) shared(out, width, height, x, n_bins, n_divs) private(i,j)
  #endif
  for (i = 0; i < out.n_rows; i++) {

    arma::rowvec tmp_hog = inner_hog_mat(x, i, height, width, n_divs, n_bins);
    
    for (j = 0; j < tmp_hog.n_elem; j++) {
      
      #ifdef _OPENMP
      #pragma omp atomic write
      #endif
      out(i,j) = tmp_hog(j);
    }
  }

  return(out);
}



// secondary function for the 'HOG_array' function
//

// [[Rcpp::export]]
arma::rowvec inner_hog_array(arma::cube& x, int n_divs, int n_bins, unsigned int i) {

  arma::rowvec tmp_hog = hog_cpp(x.slice(i), n_divs, n_bins);
  
  return tmp_hog;
}



// use this function if the input x is an array of images like the CIFAR-10 data

// [[Rcpp::export]]
arma::mat HOG_array(arma::cube& x, int n_divs = 3, int n_bins = 6, int threads = 1) {

  #ifdef _OPENMP
  omp_set_num_threads(threads);
  #endif

  arma::mat out(x.n_slices, n_divs * n_divs * n_bins);

  unsigned int i,j;

  #ifdef _OPENMP
  #pragma omp parallel for schedule(static) shared(out, n_bins, n_divs, x) private(i,j)
  #endif
  for (i = 0; i < out.n_rows; i++) {
    
    arma::rowvec tmp_hog = inner_hog_array(x, n_divs, n_bins, i);

    for (j = 0; j < tmp_hog.n_elem; j++) {
      
      #ifdef _OPENMP
      #pragma omp atomic write
      #endif
      out(i,j) = tmp_hog(j);
    }
  }

  return(out);
}

