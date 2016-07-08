# include <RcppArmadillo.h>
// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::plugins(openmp)]]
// [[Rcpp::plugins(cpp11)]]

#ifdef _OPENMP
#include <omp.h>
#endif

#include "utils.h"



// remainder

// [[Rcpp::export]]
int mod (int a, int b) {
  
  return(a % b);
}


// returns the indices in form of a matrix

// [[Rcpp::export]]
arma::mat indices(int rows, int cols) {
  
  arma::mat out(rows, cols, arma::fill::zeros);
  
  int count = 0;
  
  for (int i = 0; i < rows; i++) {
    
    for (int j = 0; j < cols; j++) {
      
      out(i,j) = count;
      
      count += 1;
    }
  }
  
  return(out);
}



// convert vector to matrix (using row-wise indexing)

// [[Rcpp::export]]
arma::mat vec2mat(arma::rowvec V, int mat_rows, int mat_cols) {    // mat_rows == columns, mat_cols == rows 
  
  arma::mat out(mat_rows, mat_cols, arma::fill::zeros);
  
  arma::mat index = indices(mat_rows, mat_cols);
  
  for (int i = 0; i < mat_rows; i++) {
    
    out.row(i) = arma::conv_to< arma::rowvec >::from(V(arma::conv_to< arma::uvec >::from(index.row(i))));
  }
  
  return(out.t());
}


// secondary function to create a sequence

// [[Rcpp::export]]
arma::vec seq_rcpp(int x) {
  
  arma::vec out(x, arma::fill::zeros);
  
  for (int i = 0; i < x; i++) {
    
    out(i) = i;
  }
  
  return(out);
}


// resize an image using nearest neighbor   [ matrix ]
//
// modification of http://stackoverflow.com/questions/1550878/nearest-neighbor-interpolation-algorithm-in-matlab

// [[Rcpp::export]]
arma::mat resize_nearest_rcpp(arma::mat image, double width, double height) { 
  
  double scale_rows = width/image.n_rows;
  
  double scale_cols = height/image.n_cols;
  
  int new_row_size = floor(scale_rows * image.n_rows);
  
  int new_col_size = floor(scale_cols * image.n_cols);
  
  arma::uvec row_index = arma::conv_to< arma::uvec >::from(floor(((seq_rcpp(new_row_size) - 0.5) / scale_rows ) + 0.5));
  
  arma::uvec col_index = arma::conv_to< arma::uvec >::from(floor(((seq_rcpp(new_col_size) - 0.5) / scale_cols ) + 0.5));
  
  double row_index_max = row_index.max();
  
  double col_index_max = col_index.max();
  
  row_index.transform( [row_index_max](double val) { return (val == row_index_max) ? (row_index_max - 1) : val;} );     // if row_index greater than the maximum row-index subtract 1 from row-index
  
  col_index.transform( [col_index_max](double val) { return (val == col_index_max) ? (col_index_max - 1) : val;} );      // if col_index greater than the maximum col-index subtract 1 from col-index
  
  arma::mat tmp_im = image(row_index, col_index);
  
  return(tmp_im);
}


// resize an image using nearest neighbor   [ array ]

// [[Rcpp::export]]
arma::cube resize_nearest_array(arma::cube image, double width, double height) {
  
  arma::cube cube_out(width, height, 3);                       
  
  for (int i = 0; i < 3; i++) {
    
    cube_out.slice(i) = resize_nearest_rcpp(image.slice(i), width, height);
  } 
  
  return(cube_out);
}


// indices of matrix column-wise

// [[Rcpp::export]]
arma::mat indices_bilinear(int rows, int cols) {               
  
  arma::mat out(rows, cols, arma::fill::zeros);
  
  int count = 0;
  
  int len = out.col(0).n_elem;
  
  for (int j = 0; j < cols; j++) {
    
    arma::vec btw(len, arma::fill::zeros);
    
    for (int i = 0; i < len; i++) {
      
      btw(i) = count;
      
      count += 1;
    }
    
    out.col(j) = btw;
  }
  
  return(out);
}


// similar to the 'x' matrix of the matlab 'meshgrid' function

// [[Rcpp::export]]
arma::mat meshgrid_x(int rows, int cols) {
  
  arma::mat out(rows, cols, arma::fill::zeros);
  
  for (int i = 0; i < rows; i++) {
    
    arma::vec tmp_vec(cols, arma::fill::zeros);
    
    for (int j = 0; j < cols; j++) {
      
      tmp_vec(j) = j;
    }
    
    out.row(i) = arma::conv_to< arma::rowvec >::from(tmp_vec);
  }
  
  return(out);
}


// similar to the 'y' matrix of the matlab 'meshgrid' function

// [[Rcpp::export]]
arma::mat meshgrid_y(int rows, int cols) {
  
  arma::mat out(rows, cols, arma::fill::zeros);
  
  int len = out.row(0).n_elem;
  
  for (int i = 0; i < rows; i++) {
    
    arma::rowvec tmp_vec(len);
    
    out.row(i) = tmp_vec.fill(i);
  }
  
  return(out);
}


// replace value above or below a thresh
// if mode = 1 : thresh >   (if greater)
// if mode = 2 : thresh <   (if less)

// [[Rcpp::export]]
arma::mat replaceVal(arma::mat x, double thresh, double value, int mode = 1) {
  
  arma::mat out = x;
  
  for (int i = 0; i < x.n_rows; i++) {
    
    for (int j = 0; j < x.n_cols; j++) {
      
      if (mode == 1) {
        
        if (x(i,j) > thresh) {
          
          out(i,j) = value;
        }
      }
      
      if (mode == 2) {
        
        if (x(i,j) < thresh) {
          
          out(i,j) = value;
        }
      }
    }
  }
  
  return(out);
}


// convert rowvector to matrix column-wise

// [[Rcpp::export]]
arma::mat vec2mat_colwise(arma::rowvec VEC, int mat_rows, int mat_cols) {
  
  arma::mat out(mat_rows, mat_cols, arma::fill::zeros);
  
  arma::mat index = indices_bilinear(mat_rows, mat_cols);
  
  for (int i = 0; i < mat_rows; i++) {
    
    out.row(i) = arma::conv_to< arma::rowvec >::from(VEC(arma::conv_to< arma::uvec >::from(index.row(i))));
  }
  
  return(out);
}


// vectorize a matrix  [ as rowvec ]

// [[Rcpp::export]]
arma::rowvec Vectz(arma::mat x) {
  
  return(arma::conv_to< arma::rowvec >::from(vectorise(x)));
}


// main function for bilinear interpolation   [ output : matrix ]
//
// modification of http://stackoverflow.com/questions/26142288/resize-an-image-with-bilinear-interpolation-without-imresize?lq=1

// [[Rcpp::export]]
arma::mat resize_bilinear_rcpp(arma::mat image, double width, double height) {
  
  double in_rows = image.n_rows;
  double in_cols = image.n_cols;
  
  double S_R = in_rows / width;
  double S_C = in_cols / height;
  
  arma::mat cf = meshgrid_x(width, height) + 1;
  arma::mat rf = meshgrid_y(width, height) + 1;
  
  rf = rf * S_R;
  cf = cf * S_C;
  
  arma::mat r = floor(rf);
  arma::mat c = floor(cf);
  
  r = replaceVal(r, 1, 1, 2);
  c = replaceVal(c, 1, 1, 2);
  
  r = replaceVal(r, in_rows - 1, in_rows - 1, 1);
  c = replaceVal(c, in_cols - 1, in_cols - 1, 1);
  
  arma::Mat<double> delta_R = rf - r;
  arma::Mat<double> delta_C = cf - c;
  
  arma::rowvec in1_ind = Vectz( (c - 1) * in_rows + r );
  arma::rowvec in2_ind = Vectz( (c - 1) * in_rows + (r + 1) );
  arma::rowvec in3_ind = Vectz( c * in_rows + r );
  arma::rowvec in4_ind = Vectz( c * in_rows + (r + 1) );
  
  arma::rowvec vect_image = Vectz(image);
  
  arma::rowvec in1_IND(in1_ind.n_elem, arma::fill::zeros);
  arma::rowvec in2_IND(in2_ind.n_elem, arma::fill::zeros);
  arma::rowvec in3_IND(in3_ind.n_elem, arma::fill::zeros);
  arma::rowvec in4_IND(in4_ind.n_elem, arma::fill::zeros);
  
  for (int i = 0; i < in1_ind.n_elem; i++) {
    
    in1_IND(i) = vect_image(in1_ind(i) - 1);
    in2_IND(i) = vect_image(in2_ind(i) - 1);
    in3_IND(i) = vect_image(in3_ind(i) - 1);
    in4_IND(i) = vect_image(in4_ind(i) - 1);
  }
  
  arma::mat tmp_out = ( vec2mat_colwise(in1_IND, width, height) % (1.0 - delta_R) % (1.0 - delta_C) ) + ( vec2mat_colwise(in2_IND, width, height) % (delta_R) % (1.0 - delta_C) ) + 
    
    ( vec2mat_colwise(in3_IND, width, height) % (1.0 - delta_R) % (delta_C) ) + ( vec2mat_colwise(in4_IND, width, height) % (delta_R) % (delta_C) );
  
  return(tmp_out);
}


// function for bilinear interpolation   [ output : array ]

// [[Rcpp::export]]
arma::cube bilinear_array(arma::cube image, double width, double height) {
  
  arma::cube cube_out(width, height, 3);
  
  for (int i = 0; i < 3; i++) {
    
    cube_out.slice(i) = resize_bilinear_rcpp(image.slice(i), width, height);
  } 
  
  return(cube_out);
}


// returns 'min' or 'max' values of an array in form of a vector

// [[Rcpp::export]]
arma::rowvec Array_range(arma::cube x, int mode = 1) {
  
  arma::rowvec tmp_val(3);
  
  for (int i = 0; i < 3; i++) {
    
    if (mode == 1) {
      
      tmp_val(i) = x.slice(i).max();}
    
    if (mode == 2) {
      
      tmp_val(i) = x.slice(i).min();
    }
  }
  
  return(tmp_val);
}



// normalizes 'array' in range [0,1]

// [[Rcpp::export]]
arma::cube Normalize_array(arma::cube x) {
  
  arma::cube x_out(x.slice(0).n_rows, x.slice(0).n_cols, 3);
  
  for (int i = 0; i < 3; i++) {
    
    double max_val = x.slice(i).max();
    
    double min_val = x.slice(i).min();
    
    x_out.slice(i) = (x.slice(i) - min_val)/(max_val - min_val);
  }
  
  return(x_out);
}


// normalizes 'matrix' in range [0,1]

// [[Rcpp::export]]
arma::mat Normalize_matrix(arma::mat x) {
  
  arma::mat x_out(x.n_rows, x.n_cols);
  
  double max_val = x.max();
    
  double min_val = x.min();
    
  x_out = (x - min_val)/(max_val - min_val);
  
  return(x_out);
}



// rotate image, specific angles [ matrix ]

// [[Rcpp::export]]
arma::mat rotate_rcpp(arma::mat image, int angle) {
  
  if (angle == 90) {
    angle = 1;}
  else if (angle == 180) {
    angle = 2;}
  else if (angle == 270) {
    angle = 3;}
  else {
    Rcpp::stop("valid rotation angles are 90, 180, 270 degrees");
  }
  
  arma::mat new_img = image;
  
  int x = 0;
  
  while(x < angle) {
    
    new_img = arma::flipud(new_img).t();
    
    x += 1;
  }
  
  return(new_img);
}


// secondary function for 'rotate_nearest_bilinear'        [  create sequence of values for rows and columns ( prespecified range ) ]

// [[Rcpp::export]]
arma::uvec seq_rcpp_range(int start, int end) {
  
  int rang = end - start;
  
  arma::uvec out(rang + 1, arma::fill::zeros);
  
  for (int i = 0; i < rang + 1; i++) {
    
    out(i) = start + i;
  }
  
  return(out);
}



// rotate image using an angle and methods 'nearest', 'bilinear' [ matrix ]
//

// [[Rcpp::export]]
arma::mat rotate_nearest_bilinear(arma::mat image, double angle, std::string method, std::string mode, int threads) {
  
  omp_set_num_threads(threads);
  
  int n = image.n_rows;
  int m = image.n_cols;
  
  int mm = n * sqrt(2.0);
  int nn = m * sqrt(2.0);
  
  arma::mat im2(mm, nn, arma::fill::zeros);
  
  double thet = angle * arma::datum::pi / 180.0;
  
  #pragma omp parallel for collapse(2)
  for (int t = 0; t < mm; t++) {
    
    for (int s = 0; s < nn; s++) {
      
      if (method == "nearest") {                          // http://stackoverflow.com/questions/1811372/how-to-rotate-image-by-nearest-neighbor-interpolation-using-matlab
        
        int i = ((t - mm / 2.0) * cos(thet) + (s - nn / 2.0) * sin(thet) + n / 2.0);
        int j = (-(t - mm / 2.0) * sin(thet) + (s - nn / 2.0) * cos(thet) + m / 2.0);
        
        if ( i > 0.0 && j > 0.0 && i < n && j < m) {
          
          im2(t,s) = image(i,j);
        }
      }
      
      if (method == "bilinear") {                        // http://www.itu.dk/people/vedrana/reports/IA_Report.pdf [ page 18, using distances rather than coefficients ] 
        
        double n0 = ((t - mm / 2.0) * cos(thet) + (s - nn / 2.0) * sin(thet) + n / 2.0);
        double m0 = (-(t - mm / 2.0) * sin(thet) + (s - nn / 2.0) * cos(thet) + m / 2.0);
        
        int n1 = floor(n0);
        int n2 = ceil(n0); 
        
        if (n1 == n2) {
          
          n2 = n2 + 1;
        }
        
        int m1 = floor(m0);
        int m2 = ceil(m0); 
        
        if (m1 == m2) {
          
          m2 = m2 + 1;
        }
        
        if (n1 >= 0.0 && n2 < n && m1 >= 0.0 && m2 < m) {
          
          im2(t,s) = ((n2 - n0 + m2 - m0) * image(n1,m1) + (n2 - n0 + m0 - m1) * image(n1,m2) + (n0 - n1 + m2 - m0) * image(n2,m1) + (n0 - n1 + m0 - m1) * image(n2,m1)) / 4.0;
        }
      }
    }
  }
  
  if (mode == "same") {
    
    // get index -- rows
    
    int dif_rows = im2.n_rows - image.n_rows;
    
    int rem_rows;
    arma::uvec remov_rows;
    
    if (dif_rows == 0) {
      
      remov_rows = seq_rcpp_range(0, im2.n_rows - 1);}
    
    else {
      
      rem_rows = mod(dif_rows, 2);
      remov_rows = seq_rcpp_range((floor(dif_rows/2.0) + 1), (im2.n_rows - floor(dif_rows/2.0) - rem_rows));
    }
    
    // get index -- columns
    
    int dif_cols = im2.n_cols - image.n_cols;
    
    int rem_cols;
    arma::uvec remov_cols;
    
    if (dif_cols == 0) {
      
      remov_cols = seq_rcpp_range(0, im2.n_cols - 1);}
    
    else {
      
      rem_cols = mod(dif_cols, 2);
      remov_cols = seq_rcpp_range((floor(dif_cols/2.0) + 1), (im2.n_cols - floor(dif_cols/2.0) - rem_cols));
    }
    
    im2 = im2(remov_rows, remov_cols);
  }
  
  return(im2);
}


//-----------------------------------------------------------------------------
// rotate image using an angle and methods 'nearest', 'bilinear' [ array ]
//

// [[Rcpp::export]]
arma::cube rotate_nearest_bilinear_array_same(arma::cube src, double angle, std::string method, int threads) {
  
  arma::cube cube_out(src.n_rows, src.n_cols, 3, arma::fill::zeros);
  
  for (int i = 0; i < 3; i++) {
    
    cube_out.slice(i) = rotate_nearest_bilinear(src.slice(i), angle, method, "same", threads);
  }
  
  return(cube_out);
}


// [[Rcpp::export]]
arma::cube rotate_nearest_bilinear_array_full(arma::cube src, double angle, std::string method, int threads) {
  
  int mm = src.n_rows * sqrt(2.0);
  int nn = src.n_cols * sqrt(2.0);
  
  arma::cube cube_out(mm, nn, 3, arma::fill::zeros);
  
  for (int i = 0; i < 3; i++) {
    
    cube_out.slice(i) = rotate_nearest_bilinear(src.slice(i), angle, method, "full", threads);
  }
  
  return(cube_out);
}


//-----------------------------------------------------------------------------

// 2-dimensional convolutions of mode "same", "full"

// [[Rcpp::export]]
arma::mat conv2d(arma::mat image, arma::mat kernel, std::string mode) {
  
  arma::mat CONV;
  
  if (mode == "full") {
    
    CONV = arma::conv2(image, kernel, "full");}           // returns a convolution matrix of : size(image) + size(kernel) - 1
  
  if (mode == "same") {
    
    CONV = arma::conv2(image, kernel, "same");             // returns a convolution matrix of equal size with the image
  }
  
  return(CONV);
}


// 3-dimensional convolutions of mode "same", "full"

// [[Rcpp::export]]
arma::cube conv3d(arma::cube image, arma::mat kernel, std::string mode) {
  
  arma::cube cube_out(image.n_rows, image.n_cols, 3);
  
  if (mode == "full") {
    
    cube_out.set_size(image.n_rows + kernel.n_rows - 1, image.n_cols + kernel.n_cols - 1, 3);
  }
  
  for (int i = 0; i < 3; i++) {
    
    arma::mat tmp = conv2d(image.slice(i), kernel, mode);
    
    cube_out.slice(i) = tmp;
  }
  
  return(cube_out);
}

//-----------------------------------------------------------------------------

// svd adjusted for the zca_whitening function
//

// [[Rcpp::export]]
arma::mat svd_arma_econ(arma::mat m) {
  
  arma::mat u;
  arma::vec s;
  arma::mat v;
  
  arma::svd_econ(u,s,v,m, "left");         
  
  arma::mat tmp = arma::join_vert(u,s.t());     // join 'u' and 's' THEN split, so that calculation of svd occurs only once
  
  return(tmp);
}


// subtract mean column-wise
//

// [[Rcpp::export]]
arma::mat removeMean(arma::mat data) {
  
  arma::mat subtr_avg(data.n_rows, data.n_cols);
  
  for (int i = 0; i < data.n_cols; i++) {
    
    double tmp_mean = mean(data.col(i));
    
    subtr_avg.col(i) = data.col(i) - tmp_mean;
  }
  
  return(subtr_avg);
}



// zca-whitening  [ single image ]
//
// http://ufldl.stanford.edu/wiki/index.php/Implementing_PCA/Whitening
// http://stackoverflow.com/questions/31528800/how-to-implement-zca-whitening-python
// http://www.cs.toronto.edu/~kriz/learning-features-2009-TR.pdf
// http://stats.stackexchange.com/questions/117427/what-is-the-difference-between-zca-whitening-and-pca-whitening


// [[Rcpp::export]]
arma::mat zca_whitening(arma::mat data, int k, double epsilon) { 
  
  arma::mat subtr_avg = removeMean(data);
  
  arma::mat sigma = (subtr_avg * subtr_avg.t())/subtr_avg.n_cols;
  
  arma::mat tmp_usv = svd_arma_econ(sigma);
  
  arma::mat tmp_u = tmp_usv.submat(0, 0, tmp_usv.n_rows - 2, k - 1);
  
  arma::vec tmp_d = arma::conv_to< arma::vec >::from(tmp_usv.submat(tmp_usv.n_rows - 1, 0, tmp_usv.n_rows - 1, tmp_usv.n_cols - 1));
  
  arma::mat ZCAMatrix = (tmp_u * arma::diagmat(1.0/sqrt(tmp_d.subvec(0, k - 1) + epsilon))) * tmp_u.t();
  
  arma::mat data_whitening = ZCAMatrix * subtr_avg;
  
  return(data_whitening);
}


// zca-whitening  [ array ]
//

// [[Rcpp::export]]
arma::cube zca_whiten_cube(arma::cube src, int k, double epsilon) {
  
  arma::cube cube_out(src.n_rows, src.n_cols, 3, arma::fill::zeros);
  
  for (int i = 0; i < 3; i++) {
    
    cube_out.slice(i) = zca_whitening(src.slice(i), k, epsilon);
  }
  
  return(cube_out);
}



// flip 'matrix' [ horizontal, vertical ]
//

// [[Rcpp::export]]
arma::mat im_flip(arma::mat x, int mode = 1) {
  
  arma::mat new_img;
  
  if (mode == 1) {
    
    new_img = arma::flipud(x);}           // reverse order of rows
  
  if (mode == 2) {
    
    new_img = arma::fliplr(x);           // reverse order of columns
  }
  
  return(new_img);
}


// flip 'array' [ horizontal, vertical ]
//

// [[Rcpp::export]]
arma::cube im_flip_cube(arma::cube src, int mode = 1) {
  
  arma::cube cube_out(src.n_rows, src.n_cols, 3, arma::fill::zeros);                       
  
  for (int i = 0; i < 3; i++) {
    
    arma::mat tmp = im_flip(src.slice(i), mode);
    
    cube_out.slice(i) = tmp;
  }
  
  return(cube_out);
}



// translation function    [ matrix ]
//

// [[Rcpp::export]]
arma::mat translation_mat(arma::mat& image, int shift_rows = 0, int shift_cols = 0) {
  
  if (shift_rows > 0) {
    
    image = image.rows(shift_rows, image.n_rows - 1);
    
    image.insert_rows(image.n_rows, shift_rows);
  }
  
  if (shift_rows < 0) {
    
    image = image.rows(0, image.n_rows - abs(shift_rows));
    
    image.insert_rows(0, abs(shift_rows) - 1);
  }
  
  if (shift_cols > 0) {
    
    image = image.cols(shift_cols, image.n_cols - 1);
    
    image.insert_cols(image.n_cols, shift_cols);}
  
  if (shift_cols < 0) {
    
    image = image.cols(0, image.n_cols - abs(shift_cols));
    
    image.insert_cols(0, abs(shift_cols) - 1);
  }
  
  return(image);
}




// function which takes as input a 2-dimensional matrix and performs augmentations
// for arma::uvec crop_height, crop_width in case of empy vectors use in R : numeric(0)

// [[Rcpp::export]]
arma::mat augment_transf(arma::mat& image, std::string flip_mode, arma::uvec crop_height, arma::uvec crop_width, double resiz_width = 0.0, 
                         
                         double resiz_height = 0.0, std::string resiz_method = "nearest", double shift_rows = 0.0, double shift_cols = 0.0, 
                         
                         double rotate_angle = 0.0, std::string rotate_method = "nearest", int zca_comps = 0, double zca_epsilon = 0.0, double image_thresh = 0.0) {
  
  int set_rows = 0;
  int set_cols = 0;
  
  if (flip_mode == "horizontal") { image = im_flip(image, 2);}
  if (flip_mode == "vertical") { image = im_flip(image, 1);}
  if (crop_height.is_empty()) { set_cols = image.n_cols; } else { set_cols = crop_height.n_elem; image = image.cols(crop_height);}
  if (crop_width.is_empty()) { set_rows = image.n_rows; } else { set_rows = crop_width.n_elem; image = image.rows(crop_width);}
  if (resiz_height > 0.0) { set_cols = resiz_height; }
  if (resiz_width > 0.0) { set_rows = resiz_width; }
  
  if (resiz_method == "nearest") {
    
    image = resize_nearest_rcpp(image, set_rows, set_cols);
  }
  
  if (resiz_method == "bilinear") {
    
    image = resize_bilinear_rcpp(image, set_rows, set_cols);
  }
  
  if (shift_rows > 0.0 || shift_cols > 0.0) {
    
    image = translation_mat(image, shift_rows, shift_cols);
  }
  
  if (rotate_angle > 0.0) {
    
    image = rotate_nearest_bilinear(image, rotate_angle, resiz_method, "same", 1);
  }
  
  if (zca_comps > 0 && zca_epsilon > 0.0) {
    
    image = zca_whitening(image, zca_comps, zca_epsilon);
  }
  
  if (image_thresh > 0.0) {
    
    image.transform( [image_thresh](double val) { return (val > image_thresh) ? double(0) : double(1); } );
  }
  
  return(image);
}



// function which takes as input either a 3-dimensional array OR an array of gray-matrices (2-dimensional matrices) and performs augmentations
// the size of BOTH the cube AND the cube.slice(i) is changed dynamically using the .set_size() operator
//

// [[Rcpp::export]]
arma::cube augment_transf_array(arma::cube& image, std::string flip_mode, arma::uvec crop_height, arma::uvec crop_width, double resiz_width = 0.0, 
                                
                                double resiz_height = 0.0, std::string resiz_method = "nearest", double shift_rows = 0.0, double shift_cols = 0.0, 
                                
                                double rotate_angle = 0.0, std::string rotate_method = "nearest", int zca_comps = 0, double zca_epsilon = 0.0,
                                
                                double image_thresh = 0.0, int threads = 1) {
  
  omp_set_num_threads(threads);
    
  arma::cube cube_out;
  
  int tmp_rows = 0;
  
  int tmp_cols = 0;
  
  if (crop_height.is_empty()) {tmp_cols = image.n_cols;} else {tmp_cols = crop_height.n_elem;}
  if (crop_width.is_empty()) {tmp_rows = image.n_rows;} else {tmp_rows = crop_width.n_elem;}
  if (resiz_height > 0.0) {tmp_cols = resiz_height;}
  if (resiz_width > 0.0) {tmp_rows = resiz_width;}
  
  cube_out.set_size(tmp_rows, tmp_cols, image.n_slices);
  
  #pragma omp parallel for schedule(static)
  for (int i = 0; i < image.n_slices; i++) {
    
    arma::mat tmp_mat;
    
    tmp_mat.set_size(tmp_rows, tmp_cols);
    
    tmp_mat = image.slice(i);
    
    cube_out.slice(i) = augment_transf(tmp_mat, flip_mode, crop_height, crop_width, resiz_width, resiz_height, resiz_method,
                   
                   shift_rows, shift_cols, rotate_angle, rotate_method, zca_comps, zca_epsilon, image_thresh);
  }
  
  return(cube_out);
}



// extention of the previous function (augment_transf_array) by using an Rcpp::List to preprocess and store 3-dimensional arrays
// this function takes a list of arrays as input and after preprocessing (using the augment_transf_array function) it returns an Rcpp::List
//


// [[Rcpp::export]]
Rcpp::List augment_array_list(Rcpp::List x, std::string flip_mode, arma::uvec crop_height, arma::uvec crop_width, double resiz_width = 0.0, 
                
                double resiz_height = 0.0, std::string resiz_method = "nearest", double shift_rows = 0.0, double shift_cols = 0.0, 
                
                double rotate_angle = 0.0, std::string rotate_method = "nearest", int zca_comps = 0, double zca_epsilon = 0.0, double image_thresh = 0.0) {
  
  Rcpp::List tmp_list(x.size());
  
  for (int i = 0; i < tmp_list.size(); i++) {
    
    arma::cube tmp_cube = x[i];

    tmp_list[i] = augment_transf_array(tmp_cube, flip_mode, crop_height, crop_width, resiz_width, resiz_height, resiz_method, shift_rows, 
                                       
                                       shift_cols, rotate_angle, rotate_method, zca_comps, zca_epsilon, image_thresh, 1);
  }
  
  return(tmp_list);
}



// returns minimum and maximum values of array
//

// [[Rcpp::export]]
Rcpp::List MinMaxArray(arma::cube x) {
  
  arma::rowvec tmp_min(3);
  arma::rowvec tmp_max(3);
  
  for (int i = 0; i < 3; i++) {
    
    tmp_min(i) = x.slice(i).min();
    tmp_max(i) = x.slice(i).max();
  }
  
  return(Rcpp::List::create( Rcpp::Named("min")=tmp_min, Rcpp::Named("max")=tmp_max));
}


// returns minimum and maximum values of matrix
//

// [[Rcpp::export]]
Rcpp::List MinMaxMatrix(arma::mat x) {
  
  return(Rcpp::List::create( Rcpp::Named("min") = x.min(), Rcpp::Named("max") = x.max()));
}

