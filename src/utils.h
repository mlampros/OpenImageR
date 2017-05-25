
#ifndef __utils__
#define __utils__

arma::mat indices(int rows, int cols);
arma::mat vec2mat(arma::rowvec V, int mat_rows, int mat_cols);
arma::vec seq_rcpp(int x);
arma::mat resize_nearest_rcpp(arma::mat image, double width, double height);
arma::mat resize_bilinear_rcpp(arma::mat image, double width, double height);
int mod (int a, int b);

#endif
