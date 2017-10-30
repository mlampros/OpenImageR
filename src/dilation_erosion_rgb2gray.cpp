# include <RcppArmadillo.h>
// [[Rcpp::plugins(openmp)]]
// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::plugins(cpp11)]]

#ifdef _OPENMP
#include <omp.h>
#endif


// secondary function for 'dilate_erode'
//

// [[Rcpp::export]]
double inner_dilate_erode(arma::rowvec& Filter, double replace_Val, unsigned int i, unsigned int j, int method, arma::mat& image) {
  
  arma::mat tmp_kern(Filter(0), Filter(1));
  
  tmp_kern.fill(replace_Val);
  
  double out_val = 0.0;
  
  for (unsigned int k = 0; k < tmp_kern.n_rows; k++) {
    
    for (unsigned int f = 0; f < tmp_kern.n_cols; f++) {
      
      unsigned int use_rows = i + k; 
      
      unsigned int use_cols = j + f; 
      
      if (use_rows > (image.n_rows - 1) || use_cols > (image.n_cols - 1)) {
        
        if (method == 1) {
          
          tmp_kern(k, f) = replace_Val;}
        
        if (method == 2) {
          
          tmp_kern(k, f) = replace_Val;
        }
      }
      
      else {
        
        tmp_kern(k, f) = image(i + k, j + f);
      }
    }
  }
  
  if (method == 1) {
    
    out_val = arma::max(arma::vectorise(tmp_kern));
  }
  
  if (method == 2) {
    
    out_val = arma::min(arma::vectorise(tmp_kern));
  }
  
  return out_val;
}



// dilation or erosion of an image   [ matrix ]
//
// http://www.mathworks.com/help/images/morphological-dilation-and-erosion.html
//

// [[Rcpp::export]]
arma::mat diate_erode(arma::mat& image, arma::rowvec& Filter, int method = 1, int threads = 1) {
  
  #ifdef _OPENMP
  omp_set_num_threads(threads);
  #endif
  
  double replace_Val;
  
  if (method == 1) {
    
    replace_Val = 0.0;
  }
  
  if (method == 2) {
    
    if (max(arma::vectorise(image)) > 1.0) {
      
      replace_Val = 255.0;}
    
    if (max(arma::vectorise(image)) <= 1.0) {
      
      replace_Val = 1.0;
    }
  }
  
  arma::mat tmp_filt(image.n_rows, image.n_cols, arma::fill::zeros);
  
  unsigned int i,j;
  
  #ifdef _OPENMP  
  #pragma omp parallel for schedule(static) shared(image, Filter, replace_Val, method, tmp_filt) private(i,j)
  #endif
  for (i = 0; i < image.n_rows; i++) {
    
    for (j = 0; j < image.n_cols; j++) {
      
      #ifdef _OPENMP
      #pragma omp atomic write
      #endif
      tmp_filt(i,j) = inner_dilate_erode(Filter, replace_Val, i, j, method, image);
    }
  }
  
  return(tmp_filt);
}



// dilation or erosion of an image   [ array ]
//

// [[Rcpp::export]]
arma::cube diate_erode_cube(arma::cube image, arma::rowvec Filter, int method = 1, int threads = 1) {
  
  arma::cube cube_out(image.n_rows, image.n_cols, 3);
  
  for (int i = 0; i < 3; i++) {
    
    cube_out.slice(i) = diate_erode(image.slice(i), Filter, method, threads);
  }                    
  
  return(cube_out);
}



//' convert an RGB image to Gray
//' 
//' @param RGB_image a 3-dimensional array
//' @return a matrix
//' @author Lampros Mouselimis
//' @details
//' This function converts an RGB image to gray
//' @export
//' @examples
//' 
//' path = system.file("tmp_images", "1.png", package = "OpenImageR")
//' 
//' image = readImage(path)
//' 
//' gray = rgb_2gray(image)
// [[Rcpp::export]]
arma::mat rgb_2gray(arma::cube RGB_image) {
  
  arma::mat image_out = RGB_image.slice(0) * 299 / 1000 + RGB_image.slice(1) * 587 / 1000 + RGB_image.slice(2) * 114 / 1000;
  
  return(image_out);
}
