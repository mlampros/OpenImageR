# include <RcppArmadillo.h>
// [[Rcpp::plugins(openmp)]]
// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::plugins(cpp11)]]


/**
 * Copyright (c) 2013-2016, Johannes Buchner
 *
 * All rights reserved.
 *
 * https://github.com/JohannesBuchner/imagehash
 *
 **/


#ifdef _OPENMP
#include <omp.h>
#endif

#include "utils.h"

#include <stdio.h>
#include <stdlib.h>


// [[Rcpp::export]]
float round_rcpp(float f, int decimal_places = 5) {

  return std::round(f * std::pow(10, decimal_places)) / std::pow(10, decimal_places);
}


// convert binary to hexadecimal
//

// [[Rcpp::export]]
std::string binary_to_hex(arma::mat x) {

  arma::rowvec VEC = arma::vectorise(x, 1);

  int h = 0;
  std::string s;

  for (unsigned int i = 0; i < VEC.n_elem; i++) {

    if (VEC(i) == 1) {

      h += std::pow(2.0, static_cast<double>(mod(i,8)));     // static_cast, so that pow(..) works
    }

    if (mod(i,8) == 7) {

      std::stringstream sstream;

      sstream << std::hex << h;

      std::string result = sstream.str();

      if (result.length() == 0) {

        result = "00";}

      if (result.length() == 1) {

        result = "0" + result;
      }

      s += result;

      h = 0;
    }
  }

  return s;
}



// https://en.wikipedia.org/wiki/Levenshtein_distance

// [[Rcpp::export]]
int levenshtein_dist(std::string s, std::string t) {

  if (s == t) return 0;
  if (s.length() == 0) return t.length();
  if (t.length() == 0) return s.length();

  arma::rowvec v0(t.length() + 1);
  arma::rowvec v1(t.length() + 1);

  for (unsigned int i = 0; i < v0.n_elem ; i++) {

    v0[i] = i;
  }

  for (unsigned int i = 0; i < s.length(); i++) {

    v1[0] = i + 1;

    for (unsigned int j = 0; j < t.length(); j++) {

      int cost = (s[i] == t[j]) ? 0 : 1;           // condition ? result_if_true : result_if_false
      
      arma::rowvec tmp_vec(3, arma::fill::zeros);
      
      tmp_vec(0) = v1[j] + 1;
      tmp_vec(1) = v0[j + 1] + 1;
      tmp_vec(2) = v0[j] + cost;

      //arma::rowvec tmp_vec = {v1[j] + 1, v0[j + 1] + 1, v0[j] + cost};              // I commented this line due to the following warning : "warning: attempt to free a non-heap object ‘tmp’ [-Wfree-nonheap-object]"

      v1[j + 1] = min(tmp_vec);
    }

    for (unsigned int j = 0; j < v0.size(); j++) {

      v0[j] = v1[j];
    }
  }

  return(v1[t.length()]);
}



// This function is a secondary function of the 'dct_2d'

// [[Rcpp::export]]
arma::vec func_dct(arma::vec x) {

  arma::vec res(x.n_elem);

  for (unsigned int k = 0; k < x.n_elem; k++) {

    res(k) = sum( x.t() * cos(arma::datum::pi / x.n_elem * (seq_rcpp(x.n_elem) + 0.5) * k));
  }

  return(res);
}


// discrete cosine transform  [ The DCT-II is probably the most commonly used form , https://en.wikipedia.org/wiki/Discrete_cosine_transform ]

// [[Rcpp::export]]
arma::mat dct_2d(arma::mat x) {

  arma::mat out(size(x), arma::fill::zeros);

  for (unsigned int i = 0; i < out.n_rows; i++) {

    out.row(i) = arma::conv_to< arma::rowvec >::from(func_dct(arma::conv_to< arma::vec >::from(x.row(i))));
  }

  return(out);
}



// phash function binary           [ please consult the COPYRIGHT file ]

// [[Rcpp::export]]
arma::rowvec phash_binary(arma::mat gray_image, int hash_size = 8, int highfreq_factor = 4, std::string resize_method = "nearest") {

  int img_size = hash_size * highfreq_factor;

  arma::mat resiz;

  if (resize_method == "nearest") {

    resiz = resize_nearest_rcpp(gray_image, img_size, img_size);}

  if (resize_method == "bilinear") {

    resiz = resize_bilinear_rcpp(gray_image, img_size, img_size);
  }

  arma::mat dcost_cols = dct_2d(resiz.t());               // dct column-wise

  arma::mat dcost_rows = dct_2d(dcost_cols.t());            // dct row-wise

  arma::mat dctlowfreq = dcost_rows(arma::span(0, hash_size-1), arma::span(0, hash_size-1));

  float med = arma::as_scalar(median(arma::vectorise(dctlowfreq)));

  float TMP2 = round_rcpp(med);

  arma::mat diff(size(dctlowfreq), arma::fill::zeros);

  for (unsigned int i = 0; i < diff.n_rows; i++) {

    for (unsigned int j = 0; j < diff.n_cols; j++) {

      float TMP1 = round_rcpp(dctlowfreq(i,j));

      diff(i,j) = TMP1 > TMP2;
    }
  }

  return(arma::vectorise(diff, 1));                      // vectorize matrix row-wise
}



// phash function hash               [ please consult the COPYRIGHT file ]

// [[Rcpp::export]]
arma::mat phash_string(arma::mat gray_image, int hash_size = 8, int highfreq_factor = 4, std::string resize_method = "nearest") {

  int img_size = hash_size * highfreq_factor;

  arma::mat resiz;

  if (resize_method == "nearest") {

    resiz = resize_nearest_rcpp(gray_image, img_size, img_size);}

  if (resize_method == "bilinear") {

    resiz = resize_bilinear_rcpp(gray_image, img_size, img_size);
  }

  arma::mat dcost_cols = dct_2d(resiz.t());               // dct column-wise

  arma::mat dcost_rows = dct_2d(dcost_cols.t());            // dct row-wise

  arma::mat dctlowfreq = dcost_rows(arma::span(0, hash_size-1), arma::span(0, hash_size-1));

  float med = arma::as_scalar(median(arma::vectorise(dctlowfreq)));

  float TMP2 = round_rcpp(med);

  arma::mat diff(size(dctlowfreq), arma::fill::zeros);

  for (unsigned int i = 0; i < diff.n_rows; i++) {

    for (unsigned int j = 0; j < diff.n_cols; j++) {

      float TMP1 = round_rcpp(dctlowfreq(i,j));

      diff(i,j) = TMP1 > TMP2;
    }
  }

  return(diff);
}



// average hash function binary                [ please consult the COPYRIGHT file ]

// [[Rcpp::export]]
arma::rowvec average_hash_binary(arma::mat gray_image, int hash_size = 8, std::string resize_method = "nearest") {

  arma::mat resiz;

  if (resize_method == "nearest") {

    resiz = resize_nearest_rcpp(gray_image, hash_size, hash_size);}

  if (resize_method == "bilinear") {

    resiz = resize_bilinear_rcpp(gray_image, hash_size, hash_size);
  }

  float MEAN = arma::as_scalar(mean(vectorise(resiz)));

  float TMP2 = round_rcpp(MEAN);

  arma::mat diff(arma::size(resiz), arma::fill::zeros);

  for (unsigned int i = 0; i < diff.n_rows; i++) {

    for (unsigned int j = 0; j < diff.n_cols; j++) {

      float TMP1 = round_rcpp(resiz(i,j));

      diff(i,j) = TMP1 > TMP2;
    }
  }

  return(arma::vectorise(diff, 1));                      // vectorize matrix row-wise
}


// average hash function hash                  [ please consult the COPYRIGHT file ]

// [[Rcpp::export]]
arma::mat average_hash_string(arma::mat gray_image, int hash_size = 8, std::string resize_method = "nearest") {

  arma::mat resiz;

  if (resize_method == "nearest") {

    resiz = resize_nearest_rcpp(gray_image, hash_size, hash_size);}

  if (resize_method == "bilinear") {

    resiz = resize_bilinear_rcpp(gray_image, hash_size, hash_size);
  }

  float MEAN = arma::as_scalar(mean(vectorise(resiz)));

  float TMP2 = round_rcpp(MEAN);

  arma::mat diff(arma::size(resiz), arma::fill::zeros);

  for (unsigned int i = 0; i < diff.n_rows; i++) {

    for (unsigned int j = 0; j < diff.n_cols; j++) {

      float TMP1 = round_rcpp(resiz(i,j));

      diff(i,j) = TMP1 > TMP2;
    }
  }

  return(diff);
}



// dhash function binary                 [ please consult the COPYRIGHT file ]

// [[Rcpp::export]]
arma::rowvec dhash_binary(arma::mat gray_image, int hash_size = 8, std::string resize_method = "nearest") {

  arma::mat resiz;

  if (resize_method == "nearest") {

    resiz = resize_nearest_rcpp(gray_image, hash_size, hash_size + 1);}

  if (resize_method == "bilinear") {

    resiz = resize_bilinear_rcpp(gray_image, hash_size, hash_size + 1);
  }

  arma::mat tmp1 = resiz(arma::span(0, resiz.n_rows-1), arma::span(1,resiz.n_cols-1));               // fix bug for dhash [ horizontally vs. vertically ]

  arma::mat tmp2 = resiz(arma::span(0, resiz.n_rows-1), arma::span(0,resiz.n_cols-2));               // fix bug for dhash

  arma::mat out(tmp1.n_rows, tmp1.n_cols, arma::fill::zeros);

  for (unsigned int i = 0; i < tmp1.n_cols; i++) {

    for (unsigned int j = 0; j < tmp1.n_rows; j++) {

      float TMP1 = round_rcpp(tmp1(i,j));                                                             // fix bug for 'bilinear' interpolation [ in case that both i and j are 1 it should return FALSE ( I used rounding to 5 digits otherwise the equality does not work )]

      float TMP2 = round_rcpp(tmp2(i,j));                                                             // ROUND ALL HASH FUNCTIONS TO 5 DIGITS [ USE FLOATS RATHER THAN DOUBLE NUMBERS ]

      out(i,j) = TMP1 > TMP2;
    }
  }

  return(arma::vectorise(out, 1));                      // vectorize matrix row-wise
}


// dhash function hash                [ please consult the COPYRIGHT file ]

// [[Rcpp::export]]
arma::mat dhash_string(arma::mat gray_image, int hash_size = 8, std::string resize_method = "nearest") {

  arma::mat resiz;

  if (resize_method == "nearest") {

    resiz = resize_nearest_rcpp(gray_image, hash_size, hash_size + 1);}

  if (resize_method == "bilinear") {

    resiz = resize_bilinear_rcpp(gray_image, hash_size, hash_size + 1);
  }

  arma::mat tmp1 = resiz(arma::span(0, resiz.n_rows-1), arma::span(1,resiz.n_cols-1));               // fix bug for dhash [ horizontally vs. vertically ]

  arma::mat tmp2 = resiz(arma::span(0, resiz.n_rows-1), arma::span(0,resiz.n_cols-2));               // fix bug for dhash

  arma::mat out(tmp1.n_rows, tmp1.n_cols, arma::fill::zeros);

  for (unsigned int i = 0; i < tmp1.n_cols; i++) {

    for (unsigned int j = 0; j < tmp1.n_rows; j++) {

      float TMP1 = round_rcpp(tmp1(i,j));                                                             // fix bug for 'bilinear' interpolation [ in case that both i and j are 1 it should return FALSE ( I used rounding to 5 digits otherwise the equality does not work )]

      float TMP2 = round_rcpp(tmp2(i,j));                                                             // ROUND ALL HASH FUNCTIONS TO 5 DIGITS [ USE FLOATS RATHER THAN DOUBLE NUMBERS ]

      out(i,j) = TMP1 > TMP2;
    }
  }

  return(out);
}



// secondary function for 'hash_image'
//

// [[Rcpp::export]]
arma::rowvec inner_hash_im(arma::mat& x, unsigned int i, int new_width, int new_height, int method, int hash_size, int highfreq_factor, std::string& resize_method) {
  
  arma::mat tmp_mat = vec2mat(arma::conv_to< arma::rowvec >::from(x.row(i)), new_width, new_height);
  
  arma::rowvec tmp_vec;
  
  if (method == 1) {
    
    tmp_vec = phash_binary(tmp_mat, hash_size, highfreq_factor, resize_method);}
  
  if (method == 2) {
    
    tmp_vec = average_hash_binary(tmp_mat, hash_size, resize_method);}
  
  if (method == 3) {
    
    tmp_vec = dhash_binary(tmp_mat, hash_size, resize_method);
  }
  
  return tmp_vec;
}


// this function takes a matrix and it returns a binary matrix using either phash, average_hash or dhash

// [[Rcpp::export]]
arma::mat hash_image(arma::mat& x, int new_width, int new_height, std::string& resize_method, int hash_size = 8, int highfreq_factor = 4, int method = 1, int threads = 1) {

  #ifdef _OPENMP
  omp_set_num_threads(threads);
  #endif

  if (method > 3 || method < 1) Rcpp::stop("method should be 1,2 or 3");

  unsigned int wid_heig = new_width * new_height;

  if (wid_heig > x.row(0).n_elem) Rcpp::stop("new_width times new_height should be equal to the columns of the matrix x");

  unsigned int uns_hash_siz = hash_size;

  if (method == 1 && (new_width < hash_size * highfreq_factor || new_height < hash_size * highfreq_factor)) {

      Rcpp::stop("the value of hash_size leads to dimensions greater than the dimensions of the initial image. Hashing an image is meant for down-sampling");}

  if (method == 2 && (uns_hash_siz >= x.n_rows || uns_hash_siz >= x.n_cols)) { Rcpp::stop("the hash size should be less than the original dimensions of the image");}

  if (method == 3 && (uns_hash_siz >= x.n_rows - 1 || uns_hash_siz >= x.n_cols - 1)) { Rcpp::stop("the hash size should be less than the (original dimensions - 1) of the image"); }

  int tmp_cols_h = std::pow(static_cast<double>(hash_size), 2.0);    // static_cast to make pow(..) work AND int conversion, so that n_cols is an integer

  arma::mat out(x.n_rows, tmp_cols_h, arma::fill::zeros);
  
  unsigned int i,k;

  #ifdef _OPENMP
  #pragma omp parallel for schedule(static) shared(x, new_height, new_width, method, resize_method, highfreq_factor, hash_size, out) private(i,k)
  #endif
  for (i = 0; i < x.n_rows; i++) {

    arma::rowvec tmp_vec = inner_hash_im(x, i, new_width, new_height, method, hash_size, highfreq_factor, resize_method);
    
    for (k = 0; k < tmp_vec.n_elem; k++) {
      
      #ifdef _OPENMP
      #pragma omp atomic write
      #endif
      out(i,k) = tmp_vec(k);
    }
  }

  return(out);
}


// secondary function for 'hash_image_cube'
//

// [[Rcpp::export]]
arma::rowvec inner_hash_im_cube(arma::cube& x, unsigned int i, int method, int hash_size, int highfreq_factor, std::string& resize_method) {
  
  arma::rowvec tmp_vec;
  
  if (method == 1) {
    
    tmp_vec = phash_binary(x.slice(i), hash_size, highfreq_factor, resize_method);}
  
  if (method == 2) {
    
    tmp_vec = average_hash_binary(x.slice(i), hash_size, resize_method);}
  
  if (method == 3) {
    
    tmp_vec = dhash_binary(x.slice(i), hash_size, resize_method);
  }
  
  return tmp_vec;
}


// this function takes an array and it returns a binary matrix using either phash, average_hash or dhash

// [[Rcpp::export]]
arma::mat hash_image_cube(arma::cube& x, std::string& resize_method, int hash_size = 8, int highfreq_factor = 4, int method = 1, int threads = 1) {

  #ifdef _OPENMP
  omp_set_num_threads(threads);
  #endif

  if (method > 3 || method < 1) Rcpp::stop("method should be 1,2 or 3");

  unsigned int uns_hash_siz = hash_size;

  unsigned int uns_hash_freq = hash_size * highfreq_factor;

  if (method == 1 && (x.n_rows < uns_hash_freq || x.n_cols < uns_hash_freq)) {

      Rcpp::stop("the value of hash_size leads to dimensions greater than the dimensions of the initial image. Hashing an image is meant for down-sampling");}

  if (method == 2 && (uns_hash_siz >= x.n_rows || uns_hash_siz >= x.n_cols)) { Rcpp::stop("the hash size should be less than the original dimensions of the image");}

  if (method == 3 && (uns_hash_siz >= x.n_rows - 1 || uns_hash_siz >= x.n_cols - 1)) { Rcpp::stop("the hash size should be less than the (original dimensions - 1) of the image");}

  int tmp_cols_h = std::pow(static_cast<double>(hash_size), 2.0);    // static_cast to make pow(..) work AND int conversion, so that n_cols is an integer

  arma::mat out(x.n_slices, tmp_cols_h, arma::fill::zeros);
  
  unsigned int i,k;

  #ifdef _OPENMP
  #pragma omp parallel for schedule(static) shared(x, method, resize_method, highfreq_factor, hash_size, out) private(i,k)
  #endif
  for (i = 0; i < x.n_slices; i++) {
    
    arma::rowvec tmp_vec = inner_hash_im_cube(x, i, method, hash_size, highfreq_factor, resize_method);
    
    for (k = 0; k < tmp_vec.n_elem; k++) {
      
      #ifdef _OPENMP
      #pragma omp atomic write
      #endif
      out(i,k) = tmp_vec(k);
    }
  }

  return(out);
}



// convert a list of matrices to an array of matrices

// [[Rcpp::export]]
arma::cube list_2array_convert(Rcpp::List x) {

  arma::mat tmp_x = x[0];
  
  unsigned int ITERS = x.size();

  arma::cube out(tmp_x.n_rows, tmp_x.n_cols, ITERS);

  for (unsigned int i = 0; i < ITERS; i++) {

    arma::mat tmp_mat = x[i];

    out.slice(i) = tmp_mat;
  }

  return(out);
}


// secondary function for 'hash_image_hex'
//

// [[Rcpp::export]]
std::string inner_hash_im_hex(arma::mat& x, unsigned int i, int new_width, int new_height, std::string& resize_method, int hash_size, int highfreq_factor, int method) {
  
  arma::mat tmp_out;
  
  arma::mat tmp_mat = vec2mat(arma::conv_to< arma::rowvec >::from(x.row(i)), new_width, new_height);
  
  if (method == 1) {
    
    tmp_out = phash_string(tmp_mat, hash_size, highfreq_factor, resize_method);}
  
  if (method == 2) {
    
    tmp_out = average_hash_string(tmp_mat, hash_size, resize_method);}
  
  if (method == 3) {
    
    tmp_out = dhash_string(tmp_mat, hash_size, resize_method);
  }
  
  std::string str_val = binary_to_hex(tmp_out);
  
  return str_val;
}



// this function takes a matrix and it returns a character vector of hashes using either phash, average_hash or dhash

// [[Rcpp::export]]
std::vector<std::string> hash_image_hex(arma::mat& x, int new_width, int new_height, std::string& resize_method, int hash_size = 8, int highfreq_factor = 4, int method = 1, int threads = 1) {

  #ifdef _OPENMP
  omp_set_num_threads(threads);
  #endif

  if (method > 3 || method < 1) Rcpp::stop("method should be 1,2 or 3");

  unsigned int new_hei_weig = new_width * new_height;

  unsigned int uns_hash_siz = hash_size;

  if (new_hei_weig > x.row(0).n_elem) Rcpp::stop("new_width times new_height should be equal to the columns of the matrix x");

  if (method == 1 && (new_width < hash_size * highfreq_factor || new_height < hash_size * highfreq_factor)) {

      Rcpp::stop("the value of hash_size leads to dimensions greater than the dimensions of the initial image. Hashing an image is meant for down-sampling");}

  if (method == 2 && (uns_hash_siz >= x.n_rows || uns_hash_siz >= x.n_cols)) Rcpp::stop("the hash size should be less than the original dimensions of the image");

  if (method == 3 && (uns_hash_siz >= x.n_rows - 1 || uns_hash_siz >= x.n_cols - 1)) Rcpp::stop("the hash size should be less than the (original dimensions - 1) of the image");

  std::vector<std::string> out(x.n_rows);

  unsigned int i;
  
  #ifdef _OPENMP
  #pragma omp parallel for schedule(static) shared(x, new_height, new_width, method, resize_method, highfreq_factor, hash_size, out) private(i)
  #endif
  for (i = 0; i < x.n_rows; i++) {

    #ifdef _OPENMP
    #pragma omp critical
    #endif
    {
      std::string tmp_str = inner_hash_im_hex(x, i, new_width, new_height, resize_method, hash_size, highfreq_factor, method);
      
      out[i] = tmp_str;
    }
  }

  return(out);
}


// secondary function for the 'hash_image_cube_hex' function
//

// [[Rcpp::export]]
std::string inner_hash_im_cube_hex(arma::cube& x, unsigned int i, std::string& resize_method, int hash_size, int highfreq_factor, int method) {
  
  arma::mat tmp_out;
  
  if (method == 1) {
    
    tmp_out = phash_string(x.slice(i), hash_size, highfreq_factor, resize_method);}
  
  if (method == 2) {
    
    tmp_out = average_hash_string(x.slice(i), hash_size, resize_method);}
  
  if (method == 3) {
    
    tmp_out = dhash_string(x.slice(i), hash_size, resize_method);
  }
  
  std::string str_val = binary_to_hex(tmp_out);
  
  return str_val;
}



// this function takes an array and it returns a character vector of hashes using either phash, average_hash or dhash

// [[Rcpp::export]]
std::vector<std::string> hash_image_cube_hex(arma::cube& x, std::string& resize_method, int hash_size = 8, int highfreq_factor = 4, int method = 1, int threads = 1) {

  #ifdef _OPENMP
  omp_set_num_threads(threads);
  #endif

  if (method > 3 || method < 1) Rcpp::stop("method should be 1,2 or 3");

  unsigned int uns_hash_siz = hash_size;

  unsigned int uns_hash_freq = hash_size * highfreq_factor;

  if (method == 1 && (x.n_rows < uns_hash_freq || x.n_cols < uns_hash_freq)) {

      Rcpp::stop("the value of hash_size leads to dimensions greater than the dimensions of the initial image. Hashing an image is meant for down-sampling");}

  if (method == 2 && (uns_hash_siz >= x.n_rows || uns_hash_siz >= x.n_cols)) Rcpp::stop("the hash size should be less than the original dimensions of the image");

  if (method == 3 && (uns_hash_siz >= x.n_rows - 1 || uns_hash_siz >= x.n_cols - 1)) Rcpp::stop("the hash size should be less than the (original dimensions - 1) of the image");

  std::vector<std::string> out(x.n_slices);

  unsigned int i;
  
  #ifdef _OPENMP
  #pragma omp parallel for schedule(static) shared(x, method, resize_method, highfreq_factor, hash_size, out) private(i)
  #endif
  for (i = 0; i < x.n_slices; i++) {

    #ifdef _OPENMP
    #pragma omp critical
    #endif
    {
      std::string tmp_str = inner_hash_im_cube_hex(x, i, resize_method, hash_size, highfreq_factor, method);
      
      out[i] = tmp_str;
    }
  }

  return(out);
}

