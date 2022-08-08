
#ifndef __OpenImageRheader__
#define __OpenImageRheader__

#ifdef _OPENMP
#include <omp.h>
#endif


# include <math.h>
# include <stdio.h>
# include <stdlib.h>
# include <string>

# include "SLIC.h"
# include "SLICO.h"


/**
 * Copyright (c) 2015, Mohammad Haghighat
 *
 * All rights reserved.
 *
 * https://github.com/mhaghighat/gabor
 *
 *
 * [ for the reimplementation of the "Gabor Features" code in Rcpp see the lines 1684 - 2223 of this file ]
 *
 **/



/**
 * Copyright (c) 2012, Sight Machine
 *
 * All rights reserved.
 *
 * https://github.com/sightmachine/SimpleCV
 *
 *
 * [ for the reimplementation of the "HoG Features" code in Rcpp see the lines 2224 - 2418 of this file ]
 *
 **/



/**
 * Copyright (c) 2013-2016, Johannes Buchner
 *
 * All rights reserved.
 *
 * https://github.com/JohannesBuchner/imagehash
 *
 *
 * [ for the reimplementation of the "Image Hashing" code in Rcpp see the lines 2419 - 3132 of this file ]
 *
 **/



/**
 * Copyright (c) 2019, Oleh Onyshchak
 *
 * All rights reserved.
 *
 * https://github.com/OlehOnyshchak/ImageTransformations
 *
 *
 * [ for the reimplementation of the "WarpAffine" code in Rcpp see the lines 3133 - 3324 of this file ]
 *
 **/


namespace oimageR {


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Utility-Functions


  class Utility_functions {

    public:

      Utility_functions() { }


      // secondary function for 'dilate_erode'
      //

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
      // https://www.mathworks.com/help/images/morphological-dilation-and-erosion.html
      //

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

      arma::cube diate_erode_cube(arma::cube image, arma::rowvec Filter, int method = 1, int threads = 1) {

        arma::cube cube_out(image.n_rows, image.n_cols, 3);

        for (int i = 0; i < 3; i++) {

          cube_out.slice(i) = diate_erode(image.slice(i), Filter, method, threads);
        }

        return(cube_out);
      }



      // convert an RGB image to Gray
      //

      arma::mat rgb_2gray_rcpp(arma::cube RGB_image) {

        arma::mat image_out = RGB_image.slice(0) * 299 / 1000 + RGB_image.slice(1) * 587 / 1000 + RGB_image.slice(2) * 114 / 1000;

        return(image_out);
      }



      // remainder
      //

      int mod (int a, int b) {

        return(a % b);
      }


      // returns the indices in form of a matrix
      //

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
      //

      arma::mat vec2mat(arma::rowvec V, int mat_rows, int mat_cols) {    // mat_rows == columns, mat_cols == rows

        arma::mat out(mat_rows, mat_cols, arma::fill::zeros);

        arma::mat index = indices(mat_rows, mat_cols);

        for (int i = 0; i < mat_rows; i++) {

          out.row(i) = arma::conv_to< arma::rowvec >::from(V(arma::conv_to< arma::uvec >::from(index.row(i))));
        }

        return(out.t());
      }


      // secondary function to create a sequence
      //

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
      //

      arma::mat resize_nearest_rcpp(arma::mat image, double width, double height) {

        double scale_rows = width/image.n_rows;
        double scale_cols = height/image.n_cols;

        int new_row_size = floor(scale_rows * image.n_rows);
        int new_col_size = floor(scale_cols * image.n_cols);

        if (new_row_size != width) {
          std::string warn_message = "When resizing the 'width' becomes " + std::to_string(new_row_size) + " therefore we have to adjust it to the input width parameter of " + std::to_string(width) + " !";
          new_row_size = ((new_row_size != width) ? (width) : new_row_size);
          Rcpp::warning(warn_message);
        }

        if (new_col_size != height) {
          std::string warn_message = "When resizing the 'height' becomes " + std::to_string(new_col_size) + " therefore we have to adjust it to the input height parameter of " + std::to_string(height) + " !";
          new_col_size = ((new_col_size != height) ? (height) : new_col_size);
          Rcpp::warning(warn_message);
        }

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
      //

      arma::cube resize_nearest_array(arma::cube image, double width, double height) {

        arma::cube cube_out(width, height, 3);

        for (int i = 0; i < 3; i++) {

          cube_out.slice(i) = resize_nearest_rcpp(image.slice(i), width, height);
        }

        return(cube_out);
      }


      // indices of matrix column-wise
      //

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


      // similar to the 'x' matrix of the matlab 'meshgrid' function  [ populating the matrices column-wise, is faster ]
      // Armadillo uses the "column-major ordering" which means read and write by column is faster than by row, see:
      // https://stackoverflow.com/a/53498100
      //

      arma::mat meshgrid_x(int rows, int cols) {

        arma::mat out(rows, cols, arma::fill::zeros);

        arma::colvec tmp_vec(out.col(0).n_elem);

        for (int i = 0; i < cols; i++) {

          out.col(i) = tmp_vec.fill(i);
        }

        return(out);
      }

      // arma::mat meshgrid_x(int rows, int cols) {             // previous version of function
      //   arma::mat out(rows, cols, arma::fill::zeros);
      //   for (int i = 0; i < rows; i++) {
      //     arma::vec tmp_vec(cols, arma::fill::zeros);
      //     for (int j = 0; j < cols; j++) {
      //       tmp_vec(j) = j;
      //     }
      //     out.row(i) = arma::conv_to< arma::rowvec >::from(tmp_vec);
      //   }
      //   return(out);
      // }


      // similar to the 'y' matrix of the matlab 'meshgrid' function  [ populating the matrices column-wise, is faster ]
      // Armadillo uses the "column-major ordering" which means read and write by column is faster than by row, see:
      // https://stackoverflow.com/a/53498100
      //

      arma::mat meshgrid_y(int rows, int cols) {

        arma::mat out(rows, cols, arma::fill::zeros);

        for (int i = 0; i < cols; i++) {

          out.col(i) = arma::regspace<arma::colvec>(0, rows - 1);
        }

        return(out);
      }

      // arma::mat meshgrid_y(int rows, int cols) {             // previous version of function
      //   arma::mat out(rows, cols, arma::fill::zeros);
      //   int len = out.row(0).n_elem;
      //   for (int i = 0; i < rows; i++) {
      //     arma::rowvec tmp_vec(len);
      //     out.row(i) = tmp_vec.fill(i);
      //   }
      //   return(out);
      // }



      // bounding box for the superpixels
      // similar to 'regionprops' in python : https://stackoverflow.com/a/40228346/8302386  [ if 'non_overlapping_superpixels' is set to false ]
      //
      // non-overlapping : it should return the same min-max values ( x-y-coordinates ) as the overlapping case, however
      //                   I will also return the pixel-indices of each rectangle ( bounding box ) that contain missing values (NA's).
      //                   so when I create the patches ( rectangles ) from an image I should fill the overlapping pixels with
      //                   a fill value ( ideally 0.0 ). That way I can use the rectangle-patches to train algorithms.
      //                   Creating non-overlapping extended superpixels ( in form of rectangles ) is not possible due to the
      //                   irregular shape of the superpixels and moreover because it is highly probable that the boundaries of a
      //                   single superpixel might belong to more than 1 superpixel-rows or -columns of an image making the calculation
      //                   of a max. value for each image superpixel-row or -column impossible.
      //

      Rcpp::List spix_bbox(arma::mat& spix_labels, bool non_overlapping_superpixels = false) {

        if (spix_labels.empty()) {
          Rcpp::stop("The input matrix is empty!");
        }

        arma::mat copy_labels;

        if (non_overlapping_superpixels) {
          copy_labels = spix_labels;
        }

        arma::mat unq_labels = arma::unique(spix_labels);
        arma::mat res_mt(unq_labels.n_rows, 6, arma::fill::zeros);

        Rcpp::List overlap_pixs;

        if (non_overlapping_superpixels) {
          overlap_pixs = Rcpp::List(unq_labels.n_rows);                  // declare and then initialize to a specific size an Rcpp-object, see : https://stackoverflow.com/a/30135123/8302386
        }

        arma::mat idx_rows = meshgrid_y(spix_labels.n_rows, spix_labels.n_cols);
        arma::mat idx_cols = meshgrid_x(spix_labels.n_rows, spix_labels.n_cols);

        for (unsigned int i = 0; i < unq_labels.n_rows; i++) {

          unsigned int tmp_unq_label = unq_labels[i];                   // the min, max values will be saved in the same order as the unique labels

          arma::uvec idx = arma::find(spix_labels == tmp_unq_label);

          arma::mat subset_r = idx_rows(idx);
          arma::mat subset_c = idx_cols(idx);

          int min_r = subset_r.min();
          int max_r = subset_r.max();
          int min_c = subset_c.min();
          int max_c = subset_c.max();
          int dif_r = max_r - min_r + 1;          // add 1 because when extracting the bbox'es I want the boundaries included too [ 6 - 2 == 4 but to get 2 included I have to add 1 ]
          int dif_c = max_c - min_c + 1;          // add 1 because when extracting the bbox'es I want the boundaries included too

          // std::cout << min_r << " - " << min_c << " - " << max_r << " - " << max_c  << std::endl;

          arma::uvec cp_idx;

          if (non_overlapping_superpixels) {

            arma::mat cp_mt = copy_labels.submat( min_r, min_c, max_r, max_c );

            if (cp_mt.has_nan()) {

              cp_idx = arma::find_nonfinite(cp_mt);
            }

            copy_labels.submat( min_r, min_c, max_r, max_c ).fill(arma::datum::nan);         // by default fill overlapping pixels with NA's

            overlap_pixs[unq_labels[i]] = cp_idx;
          }

          res_mt(tmp_unq_label,0) = min_r;
          res_mt(tmp_unq_label,1) = max_r;
          res_mt(tmp_unq_label,2) = min_c;
          res_mt(tmp_unq_label,3) = max_c;
          res_mt(tmp_unq_label,4) = dif_r;
          res_mt(tmp_unq_label,5) = dif_c;
        }

        res_mt = arma::join_horiz(res_mt, unq_labels);

        if (non_overlapping_superpixels) {

          return Rcpp::List::create( Rcpp::Named("bbox_matrix") = res_mt,
                                     Rcpp::Named("overlapping_pixs") = overlap_pixs);
        }
        else {
          return Rcpp::List::create( Rcpp::Named("bbox_matrix") = res_mt );
        }
      }


      // compute the bounding box for a subset of superpixels
      //

      std::vector<int> spix_bbox_vector(arma::mat& spix_labels, arma::rowvec spix_labels_vec) {

        if (spix_labels.empty()) {
          Rcpp::stop("The input matrix is empty!");
        }

        arma::mat idx_rows = meshgrid_y(spix_labels.n_rows, spix_labels.n_cols);
        arma::mat idx_cols = meshgrid_x(spix_labels.n_rows, spix_labels.n_cols);

        std::vector<int> all_idxs;

        for (unsigned int i = 0; i < spix_labels_vec.n_elem; i++) {

          std::vector<int> idx = arma::conv_to<std::vector<int> >::from(arma::find(spix_labels == spix_labels_vec(i)));

          all_idxs.insert(std::end(all_idxs), std::begin(idx), std::end(idx));
        }

        arma::uvec all_idxs_ar = arma::conv_to<arma::uvec >::from(all_idxs);

        arma::mat subset_r = idx_rows(all_idxs_ar);
        arma::mat subset_c = idx_cols(all_idxs_ar);

        int min_r = subset_r.min();
        int max_r = subset_r.max();
        int min_c = subset_c.min();
        int max_c = subset_c.max();

        std::vector<int> res_out = { min_r, max_r, min_c, max_c, max_r - min_r + 1, max_c - min_c + 1 };

        return res_out;
      }


      // secondary function for 'pad_matrix'
      //

      bool EVEN( int value ) {

        if ( value % 2 == 0 ) {
          return true;
        }
        else {
          return false;
        }
      }



      // padding of new rows / cols based on the 'new_rows' and 'new_cols' parameters
      //

      Rcpp::List pad_matrix(arma::mat &x, arma::uword new_rows, arma::uword new_cols, double fill_value = 0.0) {

        if (new_rows < x.n_rows) {
          Rcpp::stop("The 'new_rows' should be greater than the rows of the input data");
        }
        if (new_cols < x.n_cols) {
          Rcpp::stop("The 'new_cols' should be greater than columns of the input data");
        }

        unsigned int dif_rows = new_rows - x.n_rows;
        arma::mat concat_row(1, x.n_cols);

        int rows_start = 0;
        int rows_end = 0;

        if (dif_rows > 0) {

          concat_row.fill(fill_value);

          for (unsigned int i = 0; i < dif_rows; i++) {

            if (EVEN(i)) {
              x = arma::join_cols(x, concat_row);
              rows_end++;
            }
            else {
              x = arma::join_cols(concat_row, x);
              rows_start++;
            }
          }
        }

        arma::mat concat_col(x.n_rows, 1);
        unsigned int dif_cols = new_cols - x.n_cols;

        int cols_start = 0;
        int cols_end = 0;

        if (dif_cols > 0) {

          concat_col.fill(fill_value);

          for (unsigned int i = 0; i < dif_cols; i++) {

            if (EVEN(i)) {
              x = arma::join_rows(x, concat_col);
              cols_end++;
            }
            else {
              x = arma::join_rows(concat_col, x);
              cols_start++;
            }
          }
        }

        return Rcpp::List::create( Rcpp::Named("data") = x,
                                   Rcpp::Named("padded_start") = rows_start,
                                   Rcpp::Named("padded_end") = rows_end,
                                   Rcpp::Named("padded_left") = cols_start,
                                   Rcpp::Named("padded_right") = cols_end );
      }



      // replace value above or below a thresh
      // if mode = 1 : thresh >   (if greater)
      // if mode = 2 : thresh <   (if less)
      //

      arma::mat replaceVal(arma::mat x, double thresh, double value, int mode = 1) {

        arma::mat out = x;

        for (unsigned int i = 0; i < x.n_rows; i++) {

          for (unsigned int j = 0; j < x.n_cols; j++) {

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
      //

      arma::mat vec2mat_colwise(arma::rowvec VEC, int mat_rows, int mat_cols) {

        arma::mat out(mat_rows, mat_cols, arma::fill::zeros);

        arma::mat index = indices_bilinear(mat_rows, mat_cols);

        for (int i = 0; i < mat_rows; i++) {

          out.row(i) = arma::conv_to< arma::rowvec >::from(VEC(arma::conv_to< arma::uvec >::from(index.row(i))));
        }

        return(out);
      }


      // vectorize a matrix  [ as rowvec ]
      //

      arma::rowvec Vectz(arma::mat x) {

        return(arma::conv_to< arma::rowvec >::from(vectorise(x)));
      }


      // main function for bilinear interpolation   [ output : matrix ]
      //
      // modification of http://stackoverflow.com/questions/26142288/resize-an-image-with-bilinear-interpolation-without-imresize?lq=1
      //

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

        for (unsigned int i = 0; i < in1_ind.n_elem; i++) {

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
      //

      arma::cube bilinear_array(arma::cube image, double width, double height) {

        arma::cube cube_out(width, height, 3);

        for (int i = 0; i < 3; i++) {

          cube_out.slice(i) = resize_bilinear_rcpp(image.slice(i), width, height);
        }

        return(cube_out);
      }


      // returns 'min' or 'max' values of an array in form of a vector
      //

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
      //

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
      //

      arma::mat Normalize_matrix(arma::mat x) {

        arma::mat x_out(x.n_rows, x.n_cols);

        double max_val = x.max();

        double min_val = x.min();

        x_out = (x - min_val)/(max_val - min_val);

        return(x_out);
      }



      // rotate image, specific angles [ matrix ]
      //

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
      //

      arma::uvec seq_rcpp_range(int start, int end) {

        int rang = end - start;

        arma::uvec out(rang + 1, arma::fill::zeros);

        for (int i = 0; i < rang + 1; i++) {

          out(i) = start + i;
        }

        return(out);
      }



      // inner function for 'rotate_nearest_bilinear'
      //

      double inner_bilinear(int t, int s, arma::mat& image, std::string& method, int n, int m, int mm, int nn, double thet) {

        double im_value = 0.0;

        if (method == "nearest") {                          // http://stackoverflow.com/questions/1811372/how-to-rotate-image-by-nearest-neighbor-interpolation-using-matlab

          int i = ((t - mm / 2.0) * cos(thet) + (s - nn / 2.0) * sin(thet) + n / 2.0);
          int j = (-(t - mm / 2.0) * sin(thet) + (s - nn / 2.0) * cos(thet) + m / 2.0);

          if ( i > 0.0 && j > 0.0 && i < n && j < m) {

            im_value = image(i,j);
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

            im_value = ((n2 - n0 + m2 - m0) * image(n1,m1) + (n2 - n0 + m0 - m1) * image(n1,m2) + (n0 - n1 + m2 - m0) * image(n2,m1) + (n0 - n1 + m0 - m1) * image(n2,m1)) / 4.0;
          }
        }

        return im_value;
      }



      // rotate image using an angle and methods 'nearest', 'bilinear' [ matrix ]
      //

      arma::mat rotate_nearest_bilinear(arma::mat& image, double angle, std::string& method, std::string mode, int threads) {

        #ifdef _OPENMP
        omp_set_num_threads(threads);
        #endif

        int n = image.n_rows;
        int m = image.n_cols;

        int mm = n * sqrt(2.0);
        int nn = m * sqrt(2.0);

        arma::mat im2(mm, nn, arma::fill::zeros);

        double thet = angle * arma::datum::pi / 180.0;

        int t,s;

        #ifdef _OPENMP
        #pragma omp parallel for schedule(static) shared(mm, nn, method, thet, n, m, im2, image) private(t,s)
        #endif
        for (t = 0; t < mm; t++) {

          for (s = 0; s < nn; s++) {

            double tmp_val = inner_bilinear(t, s, image, method, n, m, mm, nn, thet);

            #ifdef _OPENMP
            #pragma omp atomic write
            #endif
            im2(t,s) = tmp_val;
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

      arma::cube rotate_nearest_bilinear_array_same(arma::cube src, double angle, std::string method, int threads) {

        arma::cube cube_out(src.n_rows, src.n_cols, 3, arma::fill::zeros);

        for (int i = 0; i < 3; i++) {

          cube_out.slice(i) = rotate_nearest_bilinear(src.slice(i), angle, method, "same", threads);
        }

        return(cube_out);
      }


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
      //

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
      //

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

      arma::mat removeMean(arma::mat data) {

        arma::mat subtr_avg(data.n_rows, data.n_cols);

        for (unsigned int i = 0; i < data.n_cols; i++) {

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
      //

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

      arma::cube zca_whiten_cube(arma::cube src, int k, double epsilon) {

        arma::cube cube_out(src.n_rows, src.n_cols, 3, arma::fill::zeros);

        for (int i = 0; i < 3; i++) {

          cube_out.slice(i) = zca_whitening(src.slice(i), k, epsilon);
        }

        return(cube_out);
      }



      // flip 'matrix' [ horizontal, vertical ]
      //

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

      arma::mat translation_mat(arma::mat& image, int shift_rows = 0, int shift_cols = 0, double FILL_VALUE = 0.0) {

        if (shift_rows > 0) {

          image = image.rows(shift_rows, image.n_rows - 1);

          unsigned int previous_rows;

          if (FILL_VALUE != 0.0) { previous_rows = image.n_rows; }

          image.insert_rows(image.n_rows, shift_rows);

          if (FILL_VALUE != 0.0) {

            image.rows(previous_rows, image.n_rows -1).fill(FILL_VALUE);
          }
        }

        if (shift_rows < 0) {

          image = image.rows(0, image.n_rows - std::abs(shift_rows) - 1);

          image.insert_rows(0, std::abs(shift_rows));

          if (FILL_VALUE != 0.0) {

            image.rows(0, std::abs(shift_rows) - 1).fill(FILL_VALUE);
          }
        }

        if (shift_cols > 0) {

          image = image.cols(shift_cols, image.n_cols - 1);

          unsigned int previous_cols;

          if (FILL_VALUE != 0.0) { previous_cols = image.n_cols; }

          image.insert_cols(image.n_cols, shift_cols);

          if (FILL_VALUE != 0.0) {

            image.cols(previous_cols, image.n_cols -1).fill(FILL_VALUE);
          }
        }

        if (shift_cols < 0) {

          image = image.cols(0, image.n_cols - std::abs(shift_cols) - 1);

          image.insert_cols(0, std::abs(shift_cols));

          if (FILL_VALUE != 0.0) {

            image.cols(0, std::abs(shift_cols) - 1).fill(FILL_VALUE);
          }
        }

        return(image);
      }



      // function which takes as input a 2-dimensional matrix and performs augmentations
      // for arma::uvec crop_height, crop_width in case of empy vectors use in R : numeric(0)
      //

      arma::mat augment_transf(arma::mat& image, std::string flip_mode, arma::uvec crop_height, arma::uvec crop_width, double resiz_width = 0.0,

                               double resiz_height = 0.0, std::string resiz_method = "nearest", double shift_rows = 0.0, double shift_cols = 0.0,

                               double rotate_angle = 0.0, std::string rotate_method = "nearest", int zca_comps = 0, double zca_epsilon = 0.0,

                               double image_thresh = 0.0, double pad_shift_value = 0.0) {

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

        if (shift_rows != 0.0 || shift_cols != 0.0) {

          image = translation_mat(image, shift_rows, shift_cols, pad_shift_value);
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

      arma::cube augment_transf_array(arma::cube& image, std::string flip_mode, arma::uvec crop_height, arma::uvec crop_width, arma::rowvec pad_shift_value, double resiz_width = 0.0,

                                      double resiz_height = 0.0, std::string resiz_method = "nearest", double shift_rows = 0.0, double shift_cols = 0.0,

                                      double rotate_angle = 0.0, std::string rotate_method = "nearest", int zca_comps = 0, double zca_epsilon = 0.0,

                                      double image_thresh = 0.0) {

        arma::cube cube_out;

        int tmp_rows = 0;

        int tmp_cols = 0;

        if (crop_height.is_empty()) {tmp_cols = image.n_cols;} else {tmp_cols = crop_height.n_elem;}
        if (crop_width.is_empty()) {tmp_rows = image.n_rows;} else {tmp_rows = crop_width.n_elem;}
        if (resiz_height > 0.0) {tmp_cols = resiz_height;}
        if (resiz_width > 0.0) {tmp_rows = resiz_width;}

        cube_out.set_size(tmp_rows, tmp_cols, image.n_slices);

        arma::mat tmp_mat;

        tmp_mat.set_size(tmp_rows, tmp_cols);

        for (unsigned int i = 0; i < image.n_slices; i++) {

          tmp_mat = image.slice(i);

          cube_out.slice(i) = augment_transf(tmp_mat, flip_mode, crop_height, crop_width, resiz_width, resiz_height, resiz_method,

                         shift_rows, shift_cols, rotate_angle, rotate_method, zca_comps, zca_epsilon, image_thresh, arma::as_scalar(pad_shift_value(i)));
        }

        return(cube_out);
      }



      // extention of the previous function (augment_transf_array) by using an Rcpp::List to preprocess and store 3-dimensional arrays
      // this function takes a list of arrays as input and after preprocessing (using the augment_transf_array function) it returns an Rcpp::List
      //

      Rcpp::List augment_array_list(Rcpp::List x, std::string flip_mode, arma::uvec crop_height, arma::uvec crop_width, arma::rowvec pad_shift_value, double resiz_width = 0.0,

                                    double resiz_height = 0.0, std::string resiz_method = "nearest", double shift_rows = 0.0, double shift_cols = 0.0,

                                    double rotate_angle = 0.0, std::string rotate_method = "nearest", int zca_comps = 0, double zca_epsilon = 0.0,

                                    double image_thresh = 0.0) {

        Rcpp::List tmp_list(x.size());

        for (int i = 0; i < tmp_list.size(); i++) {

          arma::cube tmp_cube = x[i];

          tmp_list[i] = augment_transf_array(tmp_cube, flip_mode, crop_height, crop_width, pad_shift_value, resiz_width, resiz_height, resiz_method, shift_rows,

                                             shift_cols, rotate_angle, rotate_method, zca_comps, zca_epsilon, image_thresh);
        }

        return(tmp_list);
      }



      // returns minimum and maximum values of array
      //

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

      Rcpp::List MinMaxMatrix(arma::mat x) {

        return(Rcpp::List::create( Rcpp::Named("min") = x.min(), Rcpp::Named("max") = x.max()));
      }


      // theta value calculation   [ secondary function for the next "RGB_to_HSV" function ]
      //

      double theta(double R, double G, double B, double eps = 2.2204e-16) {
        double tmp_sqrt = std::pow(R-G, 2.0) + (R-B)*(G-B);
        if (tmp_sqrt < 0) {
          tmp_sqrt = std::abs(tmp_sqrt);                                      // account for the fact that 'tmp_sqrt' is negative (sqrt(-1) gives NA). Return the absolute value of the negative value.
        }
        tmp_sqrt = std::sqrt(tmp_sqrt);                                       // account for the fact that 'tmp_sqrt' is 0 (division by 0 gives NA). Add eps to the 0 value.
        if (tmp_sqrt == 0) {
          tmp_sqrt += eps;
        }
        double tmp_inner = ((1.0/2.0)*((R-G) + (R-B))) / tmp_sqrt;
        return std::acos(tmp_inner);
      }


      // RGB to HSV colour type conversion
      //

      /**
       *
       * Reference : "Analytical Study of Colour Spaces for Plant Pixel Detection", Pankaj Kumar and Stanley J. Miklavcic, 2018, Journal of Imaging [ page 3 of 12 ]
       *
       */

      arma::cube RGB_to_HSV(arma::cube x) {             // don't use x as reference because it will be altered

        arma::mat R = x.slice(0);
        arma::mat G = x.slice(1);
        arma::mat B = x.slice(2);

        arma::mat H(R.n_rows, R.n_cols);
        arma::mat S(R.n_rows, R.n_cols);
        arma::mat V(R.n_rows, R.n_cols);

        int items = R.n_rows * R.n_cols;

        for (int i = 0; i < items; i++) {
          if (G(i) >= B(i)) {
            H(i) = theta(R(i), G(i), B(i));
          }
          else {
            H(i) = (2.0 * arma::datum::pi) - theta(R(i), G(i), B(i));
          }

          double s_val = R(i);
          if (G(i) < s_val) {
            s_val = G(i);
          }
          if (B(i) < s_val) {
            s_val = B(i);
          }

          S(i) = 1.0 - (3.0 * s_val) / (R(i) + G(i) + B(i));
          V(i) = (1.0 / 3.0) * (R(i) + G(i) + B(i));
        }

        arma::cube res(x.n_rows, x.n_cols, x.n_slices);

        res.slice(0) = H;
        res.slice(1) = S;
        res.slice(2) = V;

        return res;
      }



      // interface-function for the 'slic' and 'slico' superpixels
      // the INPUT data should be of type arma::cube() (2-dimensional or 3-dimensional)
      // In case of .TIFF files if the image has less than 3 image-bands convert from 2-dimensional to 3-dimensional (can be done with the 'List_2_Array' function or
      // directly using Rcpp by appending 3 times the same matrix to an arma::cube - if the .tif file is a matrix)
      // the 'output_image' parameter should be a binary file such as "/my_directory/image.bin"
      //

      Rcpp::List interface_superpixels(arma::cube image, std::string method = "slic", int num_superpixel = 200, double compactness_factor = 20,
                                       bool return_slic_data = false, bool return_lab_data = false, bool return_labels = false,
                                       std::string write_slic = "", bool verbose = false) {

        if (image.empty()) {
          Rcpp::stop("The input image is empty!");
        }
        int CHANNELS = image.n_slices;
        if (CHANNELS > 3) {                                                                 // in case of more than 3 slices keep the first 3 and print a warning
          image = image.slices(0,2);
          std::string str_slices = std::to_string(CHANNELS);
          Rcpp::warning("The input data has more than 3 dimensions. The dimensions were reduced from " + str_slices + " to 3!");
          CHANNELS = image.n_slices;                                                        // update the image-channels after subsetting
        }
        if (CHANNELS != 3 && CHANNELS != 1) {                                               // raise an error if the image is not 3-dimensional
          Rcpp::stop("The input image should be either a 2-dimensional (matrix) or a 3-dimensional object (RGB)!");
        }
        double im_MAX = image.max();
        double im_MIN = image.min();
        int ROWS = image.n_rows;
        int COLS = image.n_cols;
        int sz = COLS * ROWS;
        unsigned int* pbuff = new unsigned int[sz];                                         // initialize the pointer array
        int* klabels = new int[sz];                                                         // Initialize parameters
        unsigned int blue, green, red;
        int numlabels = 0;
        arma::cube dat_cube_lab;

        if (im_MAX <= 1.0 && im_MIN >= -1.0) {                                              // multiply with 255 if the input image takes values betw. -1 and 1
          image *= 255;
          Rcpp::warning("The input data has values between " + std::to_string(im_MIN) + " and " + std::to_string(im_MAX) +  ". The image-data will be multiplied by the value: 255!");
        }
        if (verbose) {
          Rcpp::Rcout << "The input image has the following dimensions: " << image.n_rows << " " << image.n_cols << " " << image.n_slices << std::endl;
        }
        for (int i = 0; i < ROWS; i++) {
          for (int j = 0; j < COLS; j++) {
            if (CHANNELS == 1) {
              red = image.slice(0)(i,j);
              pbuff[j + COLS * i] = (red << (16 & 0xff)) + (red << (8 & 0xff)) + (red & 0xff);   // replicate the input-channel 3 times
            }
            else {
              blue = image.slice(2)(i,j);                                                        // revert the order of the 'color' variable so that it can be correctly displayed in R
              green = image.slice(1)(i,j);
              red = image.slice(0)(i,j);
              pbuff[j + COLS * i] = (red << (16 & 0xff)) + (green << (8 & 0xff)) + (blue & 0xff);
            }
          }
        }
        if (verbose) {
          Rcpp::Rcout << "The '" << method << "' method will be utilized!" << std::endl;
        }
        if (method == "slic") {
          SLIC segment_slic;                                                                // Perform SLIC on the image buffer
          segment_slic.DoSuperpixelSegmentation_ForGivenNumberOfSuperpixels(pbuff, COLS, ROWS, klabels, numlabels, num_superpixel, compactness_factor);           // Alternately one can also use the function DoSuperpixelSegmentation_ForGivenStepSize() for a desired superpixel size
          segment_slic.DrawContoursAroundSegments(pbuff, klabels, COLS, ROWS, 0xff0000);    // Draw boundaries around segments
          if (return_lab_data) {
            dat_cube_lab = segment_slic.return_lab();                                       // return the initial data in Lab-colour format
          }
        }
        else if (method == "slico") {
          SLICO segment_slico;
          segment_slico.PerformSLICO_ForGivenK(pbuff, COLS, ROWS, klabels, numlabels, num_superpixel);            // the compactness parameter is not used in the "slico" method
          segment_slico.DrawContoursAroundSegments(pbuff, klabels, COLS, ROWS, 0xff0000);
          if (return_lab_data) {
            dat_cube_lab = segment_slico.return_lab();                                      // return the initial data in Lab-colour format
          }
        }
        else {
          Rcpp::stop("Invalid 'method' parameter!");
        }

        arma::mat ROW_3mat;
        std::vector<int> res_labels;                                                        // return the labels if the 'return_labels' parameter is set to true
        arma::mat mt_labels;
        arma::cube arma_3d;

        if ( (write_slic != "") || return_slic_data) {
          ROW_3mat.resize(3, COLS * ROWS);                                                  // save the data in 3 * (ROWS * COLS) format
          arma_3d.set_size(ROWS, COLS, 3);

          for (int i = 0; i < ROWS; i++) {
            for (int j = 0; j < COLS; j++) {
              uint8_t *color = (uint8_t*)(pbuff+ i * COLS + j);

              arma_3d.slice(0)(i,j) = color[2];                                                // revert the order of the 'color' variable so that it can be correctly displayed in R
              arma_3d.slice(1)(i,j) = color[1];
              arma_3d.slice(2)(i,j) = color[0];
            }
          }
        }

        int store_n_rows = arma_3d.n_rows;                                                     // store information of the output matrix before reseting the object
        int store_n_cols = arma_3d.n_cols;

        arma::mat tmp_slice, tmp_lab;
        if ( (write_slic != "") || return_slic_data || return_lab_data) {
          if (CHANNELS == 1) {                                                                 // subset the data for the 2-dimensional case
            tmp_slice = arma_3d.slice(2);                                                      // save slice 2 because I reverted the 'color' order
            arma_3d.reset();                                                                   // after reseting I have to set-the-size, otherwise it gives an error

            if (return_lab_data) {
              tmp_lab = dat_cube_lab.slice(2);
              dat_cube_lab.reset();
            }
          }
        }

        if (verbose) {
          Rcpp::Rcout << "The output image has the following dimensions: " << store_n_rows << " " << store_n_cols << " " << CHANNELS << std::endl;
        }
        if (write_slic != "") {                                                             // write the data to a binary file using armadillo
          if (CHANNELS == 1) {
            tmp_slice.save(write_slic);
            if (!return_slic_data) {
              tmp_slice.reset();            // if 'return_slic_data' is set tp false, reset the size of the cube to zero after saving the data
            }
          }
          else {
            arma_3d.save(write_slic);
            if (!return_slic_data) {
              arma_3d.reset();              // if 'return_slic_data' is set tp false, reset the size of the cube to zero after saving the data
            }
          }
        }
        if (return_labels) {
          res_labels.assign(klabels, klabels + sz);
          mt_labels.set_size(ROWS,COLS);
          for (int i = 0; i < ROWS; i++) {
            for (int j = 0; j < COLS; j++) {
              mt_labels(i,j) = res_labels[i * COLS + j];
            }
          }
        }

        if (pbuff) delete [] pbuff;                                                         // Clean up
        if (klabels) delete [] klabels;
        Rcpp::List all_obj;

        if (CHANNELS == 1) {
          all_obj = Rcpp::List::create(Rcpp::Named("slic_data") = tmp_slice, Rcpp::Named("labels") = mt_labels);
        }
        else {
          all_obj = Rcpp::List::create(Rcpp::Named("slic_data") = arma_3d, Rcpp::Named("labels") = mt_labels);
        }

        if (return_lab_data) {
          if (CHANNELS == 1) {
            all_obj["lab_data"] = tmp_lab;
          }
          else {
            all_obj["lab_data"] = dat_cube_lab;
          }
        }

        return all_obj;
      }


      ~Utility_functions() { }

  };



  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Gabor-features [ parallelized & not-parallelized]


  // struct to return the 'gaborFilterBank' matrices
  //

  struct gabor_filt_bnk {

    arma::field<arma::Mat<arma::cx_double> > gaborArray;

    arma::field<arma::Mat<double> > gabor_imaginary;

    arma::field<arma::Mat<double> > gabor_real;
  };



  // struct to return objects in the 'gaborFeaturesRcpp' method
  //

  struct gabor_obj {

    arma::Mat<arma::cx_double> tmp_cx_inner;

    arma::vec gaborAbs_vec;

    arma::vec gaborMagn_vec;
  };



  // struct to return the Gabor features ['Local Energy', 'Mean Amplitude', 'Magnitude']
  //

  struct gabor_feats_obj {

    arma::Mat<double> gabor_features;

    arma::Mat<double> gabor_features_Magn;
  };


  // Gabor Features  [ armadillo class -- parallelized ]
  //

  class Gabor_Features {

    private:

      friend class Gabor_Features_Rcpp;                                    // define 'Gabor_Features_Rcpp' as a friend class so that the private variables of 'Gabor_Features' class are accessible ( once the 'gaborFilterBank' is initialized ) [ https://www.quora.com/How-do-I-access-private-data-members-of-one-class-into-another-class-is-their-any-trick ]

      arma::field<arma::Mat<arma::cx_double> > gaborArray;

      arma::field<arma::Mat<double>> gabor_imaginary;

      arma::field<arma::Mat<double>> gabor_real;

      // std::complex<double> z = 1i;

      arma::cx_double z = arma::cx_double(0,1);

      int U;

      int V;

      arma::Mat<double> gabor_features;

      arma::Mat<double> gabor_features_Magn;


    public:


      Gabor_Features() { }


      /*

      Gabor feature extraction  [ for a matrix of images ]

      https://github.com/mhaghighat/gabor

      Output : struct  [ a number of matrices of type complex -- conversion of complex to double is not possible, however
      I can extract the real and the imaginary part of the complex matrix to do the plotting ]

      */

      void gaborFilterBank(int u, int v, int m, int n, bool plot_data = false) {

        U = u;

        V = v;

        gaborArray.set_size(u,v);

        if (plot_data) {

          gabor_imaginary.set_size(u,v);

          gabor_real.set_size(u,v);
        }

        double fmax = 0.25;

        double gama = std::sqrt(2.0);

        double eta = std::sqrt(2.0);

        for (int i = 1; i < u + 1; i++) {

          double fu = fmax / std::pow(std::sqrt(2.0), (i - 1.0));

          double alpha = fu / gama;

          double beta = fu / eta;

          for (int j = 1; j < v + 1; j++) {

            double tetav = ((j - 1.0) / v) * arma::datum::pi;

            arma::Mat<arma::cx_double> gFilter(m, n, arma::fill::zeros);

            for (int x = 1; x < m + 1; x++) {

              for (int y = 1; y < n + 1; y++) {

                double xprime = (x - ((m + 1.0) / 2.0)) * std::cos(tetav) + (y - ((n + 1.0) / 2.0)) * sin(tetav);

                double yprime = -(x - ((m + 1.0) / 2.0)) * std::sin(tetav) + (y - ((n + 1.0) / 2.0)) * std::cos(tetav);

                arma::cx_double tmp_exp_in = (2.0 * arma::datum::pi * fu * xprime) * z;

                arma::cx_double tmp_exp = exp(tmp_exp_in);

                arma::cx_double tmp_inner = ((std::pow(fu, 2.0) / (arma::datum::pi * gama * eta)) * std::exp(-((std::pow(alpha, 2.0)) * (std::pow(xprime, 2.0)) + (std::pow(beta, 2.0)) * (std::pow(yprime, 2.0))))) * z;

                gFilter(x-1, y-1) = tmp_inner * tmp_exp;                         // in 'gFilter' subtract 1 to adjust indexing-difference between matlab and c++
              }
            }

            gaborArray(i-1, j-1) = gFilter;                                      // in 'gFilter' subtract 1 to adjust indexing-difference between matlab and c++

            if (plot_data) {

              gabor_imaginary(i-1, j-1) = arma::imag(gFilter);

              gabor_real(i-1, j-1) = arma::real(gFilter);
            }
          }
        }
      }



      // matlab's 'Imfilter' using a 2-dimensional convolution of mode either "same" or "full"  [ in the 'gaborFeatures' method it defaults to 'same' because I use the input image dimensions before the double-for-loop to estimate the output-gabor-features dimensions ]
      //

      arma::Mat<arma::cx_double> IMFILTER(arma::Mat<arma::cx_double> image, arma::Mat<arma::cx_double> kernel, const char* mode = "same") {

        return arma::conv2(image, kernel, mode);
      }



      // downsample a matrix by rows [ similar to matlab's 'downsample' function ]
      //

      arma::Mat<double> downsample(arma::Mat<double> data, int keep_every_n) {

        arma::uvec SEQ = arma::regspace<arma::uvec>(0, keep_every_n, data.n_rows - 1);

        return data.rows(SEQ);
      }



      // calculate the number of elements of the feature-vector
      //

      int feat_vec_elem(int num_rows, int num_cols, int d1, int d2) {

        arma::uvec downsample_rows = arma::regspace<arma::uvec>(0, d1, num_rows - 1);

        arma::uvec downsample_cols = arma::regspace<arma::uvec>(0, d2, num_cols - 1);

        return downsample_rows.n_elem * downsample_cols.n_elem;
      }



      // Create Gabor-features  [ to utilize in parallelized matrix-like-image loop as is the case in the mnist data ]
      //

      void gaborFeatures_matrix(arma::Mat<double>& img, int d1 = 1, int d2 = 1, bool downsample_vec = false, bool normalize_features = false) {

        arma::Mat<arma::cx_double> img_complex = img * z;                                                        // here convert the input image to type complex so that convolution of two complex matrices (image, kernel) is feasible

        int n_elem_outer = img.n_rows * img.n_cols;

        if (downsample_vec) {

          n_elem_outer = feat_vec_elem(img.n_rows, img.n_cols, d1, d2);                                          // calculate the number of elements of each column in the 'OUTER_mt' matrix
        }

        int iter_feat_mat = 0;

        gabor_features.set_size(n_elem_outer, U * V);                                                            // output matrix for 'Local Energy', 'Mean Amplitude'

        gabor_features.fill(0.0);

        gabor_features_Magn.set_size(n_elem_outer, U);                                                           // output matrix for 'Magnitude'

        gabor_features_Magn.fill(0.0);

        for (int i = 0; i < U; i++) {

          arma::Mat<double> OUTER_mt(n_elem_outer, V, arma::fill::zeros), OUTER_mt_Magn(n_elem_outer, V, arma::fill::zeros);

          for (int j = 0; j < V; j++) {                                                                            // don't parallelize the loop (small or no benefit at all)

            arma::Mat<arma::cx_double> tmp_cx_inner = IMFILTER(img_complex, gaborArray(i,j), "same");              // convolution mode defaults to 'same' [ needed so that I can estimate the 'n_elems' of the 'gaborAbs_vec' vector using the 'n_elem_outer' variable before I enter the double for-loop ]

            arma::Mat<double> gabor_real = arma::real(tmp_cx_inner);                                               // I modified the initial matlab code and rather than keeping the arma::abs() I kept the arma::real()

            //---------------------------------------------------------------------- magnitude = sqrt(a^2 + b^2)

            arma::Mat<double> gabor_imag = arma::imag(tmp_cx_inner);

            arma::mat magnitude = arma::sqrt(gabor_real % gabor_real + gabor_imag % gabor_imag);

            arma::vec gaborMagn_vec = arma::vectorise(magnitude);

            //----------------------------------------------------------------------

            if (downsample_vec) {

              gabor_real = downsample(gabor_real, d1);

              gabor_real = downsample(gabor_real.t(), d2);
            }

            arma::vec gaborAbs_vec = arma::vectorise(gabor_real);

            if (normalize_features) {

              gaborAbs_vec = (gaborAbs_vec - arma::as_scalar(arma::mean(gaborAbs_vec))) / arma::as_scalar(arma::stddev(gaborAbs_vec, 1));                 //  Normalized to zero mean and unit variance
            }

            for (unsigned int k = 0; k < gaborAbs_vec.n_elem; k++) {                                                                                      // don't parallelize the inner loop (small or no benefit at all)

              OUTER_mt(k, j) = gaborAbs_vec(k);

              OUTER_mt_Magn(k, j) = gaborMagn_vec(k);
            }

            iter_feat_mat++;
          }

          int count = 0;

          for (int b = (iter_feat_mat - V); b < iter_feat_mat; b++) {

            gabor_features.col(b) = OUTER_mt.col(count);                                                                                                  // populate the 'gabor_features' iterative [ for every V group ]

            count++;
          }

          gabor_features_Magn.col(i) = arma::mean(OUTER_mt_Magn, 1);                                                                                     // Here someone can experiment with arma::mean(), arma::max() or arma::min() of Magnitude [ arma::mean() gives slightly better results ]
        }
      }



      // inner function used in 'Gabor_generate'
      //

      gabor_feats_obj inner_func_gabor(unsigned int i, arma::Mat<double>& img_data, int img_nrow, int img_ncol, int d1, int d2,

                                       int u, int v, int m, int n, bool downsample_vec = false, bool normalize_features = false) {

        arma::Mat<double> tmp_mt = arma::reshape(img_data.row(i), img_nrow, img_ncol);

        gaborFilterBank(u, v, m, n, false);

        gaborFeatures_matrix(tmp_mt, d1, d2, downsample_vec, normalize_features);

        arma::Mat<double> local_energy  = arma::sum(arma::pow(gabor_features, 2.0), 0);

        arma::Mat<double> mean_aplitude = arma::sum(arma::abs(gabor_features), 0);

        arma::Mat<double> concat = arma::join_rows(local_energy, mean_aplitude);

        gabor_features_Magn = arma::vectorise(gabor_features_Magn).t();

        return {mean_aplitude, gabor_features_Magn};
      }



      // return 'gaborFilterBank' output [ necessary so that Rcpp code is not present in the parallelized loop ]
      //

      gabor_filt_bnk return_gaborFilterBank() {

        return {gaborArray, gabor_imaginary, gabor_real};
      }


      ~Gabor_Features() { }
  };



  // Gabor Features  [ Rcpp Class non-parallelized ]
  // inherits the 'Gabor_Features' class
  //

  class Gabor_Features_Rcpp {

    /*

    Gabor feature extraction  [ convertion of 'matlab' to 'Rcpp' code ]

    https://github.com/mhaghighat/gabor

    Output : Rcpp::List [ a number of matrices of type complex -- conversion of complex to double is not possible, however
    I can extract the real and the imaginary part of the complex matrix to do the plotting ]

    */

    private:

      Gabor_Features gbf;

      Rcpp::List gaborResult;

      Rcpp::List gabor_result_imaginary;

      Rcpp::List gabor_result_real;

      arma::Mat<double> gabor_features;

      arma::Mat<double> gabor_features_Magn;

    public:

      Gabor_Features_Rcpp() { }


      // convert regular armadillo matrix of type double to complex matrix of type double
      //

      arma::Mat<arma::cx_double> conv2complex(arma::Mat<double> img) {

        arma::Mat<arma::cx_double> img_complex = img * gbf.z;

        return img_complex;
      }


      // inner function for 'gaborFeaturesRcpp'   [ to avoid ASAN-UBSAN errors ]
      //

      gabor_obj inner_struct_gabor(int i, int j, arma::Mat<arma::cx_double>& img_complex, int d1 = 1, int d2 = 1, bool downsample_vec = false, bool normalize_features = false) {

        arma::Mat<arma::cx_double> tmp_cx_inner = gbf.IMFILTER(img_complex, gbf.gaborArray(i,j), "same");          // convolution mode defaults to 'same' [ needed so that I can estimate the n_elems of the 'gaborAbs_vec' vector using the 'n_elem_outer' variable before I enter the double for-loop ]

        arma::Mat<double> gabor_real = arma::real(tmp_cx_inner);                                                   // I modified the initial matlab code and rather than keeping the arma::abs() I kept the arma::real()

        //---------------------------------------------------------------------- magnitude = sqrt(a^2 + b^2)

        arma::Mat<double> gabor_imag = arma::imag(tmp_cx_inner);

        arma::mat magnitude = arma::sqrt(gabor_real % gabor_real + gabor_imag % gabor_imag);

        arma::vec gaborMagn_vec = arma::vectorise(magnitude);

        //----------------------------------------------------------------------

        if (downsample_vec) {

          gabor_real = gbf.downsample(gabor_real, d1);

          gabor_real = gbf.downsample(gabor_real.t(), d2);
        }

        arma::vec gaborAbs_vec = arma::vectorise(gabor_real);

        if (normalize_features) {

          gaborAbs_vec = (gaborAbs_vec - arma::as_scalar(arma::mean(gaborAbs_vec))) / arma::as_scalar(arma::stddev(gaborAbs_vec, 1));                 //  Normalized to zero mean and unit variance
        }

        gabor_obj out_obj;

        out_obj.tmp_cx_inner = tmp_cx_inner;

        out_obj.gaborAbs_vec = gaborAbs_vec;

        out_obj.gaborMagn_vec = gaborMagn_vec;

        return out_obj;
      }



      // Create Gabor-features
      //

      void gaborFeaturesRcpp(arma::Mat<arma::cx_double>& img_complex, int u, int v, int m, int n, int d1 = 1, int d2 = 1, bool downsample_vec = false,

                             bool plot_data = false, bool normalize_features = false, int threads = 1) {

        gbf.gaborFilterBank(u, v, m, n, plot_data);                                                     // first run the 'gaborFilterBank' to populate the 'gaborArray', 'gabor_imaginary' and 'gabor_real' AND update the private variables in the 'Gabor_Features' class

        #ifdef _OPENMP
        omp_set_num_threads(threads);
        #endif

        int n_elem_outer = img_complex.n_rows * img_complex.n_cols;

        if (downsample_vec) {

          n_elem_outer = gbf.feat_vec_elem(img_complex.n_rows, img_complex.n_cols, d1, d2);             // calculate the number of elements of each column in the 'OUTER_mt' matrix
        }

        int iter_feat_mat = 0;

        gabor_features.set_size(n_elem_outer, gbf.U * gbf.V);

        gabor_features.fill(0.0);

        gabor_features_Magn.set_size(n_elem_outer, gbf.U);                                              // output matrix for 'Magnitude'

        gabor_features_Magn.fill(0.0);

        for (int i = 0; i < gbf.U; i++) {

          arma::Mat<double> OUTER_mt(n_elem_outer, gbf.V, arma::fill::zeros), OUTER_mt_Magn(n_elem_outer, gbf.V, arma::fill::zeros);

          arma::cx_cube OUTER_field(img_complex.n_rows, img_complex.n_cols, gbf.V);                     // use better arma::cube rather than arma::field to save the matrices

          int j;
          unsigned int f, t;

          #ifdef _OPENMP
          #pragma omp parallel for schedule(static) shared(i, img_complex, d1, d2, normalize_features, OUTER_mt, OUTER_mt_Magn, plot_data, OUTER_field, downsample_vec) private(j,f,t) reduction(+:iter_feat_mat)
          #endif
          for (j = 0; j < gbf.V; j++) {

            gabor_obj func_in = inner_struct_gabor(i, j, img_complex, d1, d2, downsample_vec, normalize_features);

            if (plot_data) {

              for (f = 0; f < func_in.tmp_cx_inner.n_rows; f++) {

                for (t = 0; t < func_in.tmp_cx_inner.n_cols; t++) {

                  OUTER_field.slice(j)(f,t) = func_in.tmp_cx_inner(f,t);
                }
              }
            }

            for (unsigned int k = 0; k < func_in.gaborAbs_vec.n_elem; k++) {

              #ifdef _OPENMP
              #pragma omp atomic write
              #endif
              OUTER_mt(k, j) = func_in.gaborAbs_vec(k);
              OUTER_mt_Magn(k, j) = func_in.gaborMagn_vec(k);
            }

            iter_feat_mat++;
          }

          int count = 0;

          for (int b = (iter_feat_mat - gbf.V); b < iter_feat_mat; b++) {

            gabor_features.col(b) = OUTER_mt.col(count);                                  // populate the 'gabor_features' iterative [ for every V group ]

            count++;
          }

          gabor_features_Magn.col(i) = arma::mean(OUTER_mt_Magn, 1);                      // Here someone can experiment with arma::mean(), arma::max() or arma::min() of Magnitude [ arma::mean() gives slightly better results ]

          gaborResult.push_back(OUTER_field);                                             // save the complex matrices

          if (plot_data) {

            arma::cube OUTER_field_IMAG(img_complex.n_rows, img_complex.n_cols, gbf.V), OUTER_field_REAL(img_complex.n_rows, img_complex.n_cols, gbf.V);

            for (unsigned int s = 0; s < OUTER_field.n_slices; s++) {

              OUTER_field_IMAG.slice(s) = arma::imag(OUTER_field.slice(s));

              OUTER_field_REAL.slice(s) = arma::real(OUTER_field.slice(s));
            }

            gabor_result_imaginary.push_back(OUTER_field_IMAG);                           // save the imaginary matrices

            gabor_result_real.push_back(OUTER_field_REAL);                                // save the real matrices
          }
        }
      }


      // return 'gaborFeatures' results
      //

      Rcpp::List return_gaborFeatures(bool plot_data, bool vectorize_magnitude = true) {

        arma::Mat<double> local_energy  = arma::sum(arma::pow(gabor_features, 2.0), 0);

        arma::Mat<double> mean_aplitude = arma::sum(arma::abs(gabor_features), 0);

        arma::Mat<double> concat = arma::join_rows(local_energy, mean_aplitude);

        if (vectorize_magnitude) {

          gabor_features_Magn = arma::vectorise(gabor_features_Magn).t();
        }

        Rcpp::List res_out_fts;

        res_out_fts["gaborFeatures"] = Rcpp::List::create(Rcpp::Named("magnitude") = gabor_features_Magn, Rcpp::Named("energy_aptitude") = concat);

        if (plot_data) {

          res_out_fts["gabor_features_imaginary"] = gabor_result_imaginary;

          res_out_fts["gabor_features_real"] = gabor_result_real;
        }

        return res_out_fts;
      }

      ~Gabor_Features_Rcpp() { }
  };

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ HoG-features


  class HoG_features {

    /*
     * I used and modified the findHOGFeatures() function of the SimpleCV computer vision platform
     *
     * [ The HOG schema was taken from "https://github.com/lastlegion/SimpleCV/blob/develop/SimpleCV/ImageClass.py" ]
     *
     * please consult the COPYRIGHTS file
     *
     */

    private:

      Utility_functions UtilF;


    public:

      HoG_features() { }


      // HOG function

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

      arma::rowvec inner_hog_mat(arma::mat& x, unsigned int i, int height, int width, int n_divs, int n_bins) {

        arma::mat tmp = UtilF.vec2mat(x.row(i), height, width);

        arma::rowvec tmp_hog = hog_cpp(tmp, n_divs, n_bins);

        return tmp_hog;
      }



      // Use this function if the input x is a matrix like the MNIST data where each row of matrix x is an image of 28x28 dimensions

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

      arma::rowvec inner_hog_array(arma::cube& x, int n_divs, int n_bins, unsigned int i) {

        arma::rowvec tmp_hog = hog_cpp(x.slice(i), n_divs, n_bins);

        return tmp_hog;
      }



      // use this function if the input x is an array of images like the CIFAR-10 data

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

      ~HoG_features() { }

  };


  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Image-Hashing


  class Image_Hashing {

    private:

      Utility_functions UtilF;

    public:

      Image_Hashing() { }


      // round float numbers
      //

      float round_rcpp(float f, int decimal_places = 5) {

        return std::round(f * std::pow(10, decimal_places)) / std::pow(10, decimal_places);
      }


      // convert binary to hexadecimal
      //

      std::string binary_to_hex(arma::mat x) {

        arma::rowvec VEC = arma::vectorise(x, 1);

        int h = 0;
        std::string s;

        for (unsigned int i = 0; i < VEC.n_elem; i++) {

          if (VEC(i) == 1) {

            h += std::pow(2.0, static_cast<double>(UtilF.mod(i,8)));     // static_cast, so that pow(..) works
          }

          if (UtilF.mod(i,8) == 7) {

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
      //

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

            int cost = (s[i] == t[j]) ? 0 : 1;                                               // condition ? result_if_true : result_if_false

            arma::rowvec tmp_vec(3, arma::fill::zeros);

            tmp_vec(0) = v1[j] + 1;
            tmp_vec(1) = v0[j + 1] + 1;
            tmp_vec(2) = v0[j] + cost;

            //arma::rowvec tmp_vec = {v1[j] + 1, v0[j + 1] + 1, v0[j] + cost};              // I commented this line due to the following warning : "warning: attempt to free a non-heap object 'tmp' [-Wfree-nonheap-object]"

            v1[j + 1] = min(tmp_vec);
          }

          for (unsigned int j = 0; j < v0.size(); j++) {

            v0[j] = v1[j];
          }
        }

        return(v1[t.length()]);
      }



      // This function is a secondary function of the 'dct_2d'
      //

      arma::vec func_dct(arma::vec x) {

        arma::vec res(x.n_elem);

        for (unsigned int k = 0; k < x.n_elem; k++) {

          res(k) = arma::accu( x.t() * cos(arma::datum::pi / x.n_elem * (UtilF.seq_rcpp(x.n_elem) + 0.5) * k));
        }

        return(res);
      }


      // discrete cosine transform  [ The DCT-II is probably the most commonly used form , https://en.wikipedia.org/wiki/Discrete_cosine_transform ]
      //

      arma::mat dct_2d(arma::mat x) {

        arma::mat out(size(x), arma::fill::zeros);

        for (unsigned int i = 0; i < out.n_rows; i++) {

          out.row(i) = arma::conv_to< arma::rowvec >::from(func_dct(arma::conv_to< arma::vec >::from(x.row(i))));
        }

        return(out);
      }



      // phash function binary           [ please consult the COPYRIGHT file ]
      //

      arma::rowvec phash_binary(arma::mat gray_image, int hash_size = 8, int highfreq_factor = 4, std::string resize_method = "nearest") {

        int img_size = hash_size * highfreq_factor;

        arma::mat resiz;

        if (resize_method == "nearest") {

          resiz = UtilF.resize_nearest_rcpp(gray_image, img_size, img_size);}

        if (resize_method == "bilinear") {

          resiz = UtilF.resize_bilinear_rcpp(gray_image, img_size, img_size);
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
      //

      arma::mat phash_string(arma::mat gray_image, int hash_size = 8, int highfreq_factor = 4, std::string resize_method = "nearest") {

        int img_size = hash_size * highfreq_factor;

        arma::mat resiz;

        if (resize_method == "nearest") {

          resiz = UtilF.resize_nearest_rcpp(gray_image, img_size, img_size);}

        if (resize_method == "bilinear") {

          resiz = UtilF.resize_bilinear_rcpp(gray_image, img_size, img_size);
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
      //

      arma::rowvec average_hash_binary(arma::mat gray_image, int hash_size = 8, std::string resize_method = "nearest") {

        arma::mat resiz;

        if (resize_method == "nearest") {

          resiz = UtilF.resize_nearest_rcpp(gray_image, hash_size, hash_size);}

        if (resize_method == "bilinear") {

          resiz = UtilF.resize_bilinear_rcpp(gray_image, hash_size, hash_size);
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
      //

      arma::mat average_hash_string(arma::mat gray_image, int hash_size = 8, std::string resize_method = "nearest") {

        arma::mat resiz;

        if (resize_method == "nearest") {

          resiz = UtilF.resize_nearest_rcpp(gray_image, hash_size, hash_size);}

        if (resize_method == "bilinear") {

          resiz = UtilF.resize_bilinear_rcpp(gray_image, hash_size, hash_size);
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
      //

      arma::rowvec dhash_binary(arma::mat gray_image, int hash_size = 8, std::string resize_method = "nearest") {

        arma::mat resiz;

        if (resize_method == "nearest") {

          resiz = UtilF.resize_nearest_rcpp(gray_image, hash_size, hash_size + 1);}

        if (resize_method == "bilinear") {

          resiz = UtilF.resize_bilinear_rcpp(gray_image, hash_size, hash_size + 1);
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
      //

      arma::mat dhash_string(arma::mat gray_image, int hash_size = 8, std::string resize_method = "nearest") {

        arma::mat resiz;

        if (resize_method == "nearest") {

          resiz = UtilF.resize_nearest_rcpp(gray_image, hash_size, hash_size + 1);}

        if (resize_method == "bilinear") {

          resiz = UtilF.resize_bilinear_rcpp(gray_image, hash_size, hash_size + 1);
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

      arma::rowvec inner_hash_im(arma::mat& x, unsigned int i, int new_width, int new_height, int method, int hash_size, int highfreq_factor, std::string& resize_method) {

        arma::mat tmp_mat = UtilF.vec2mat(arma::conv_to< arma::rowvec >::from(x.row(i)), new_width, new_height);

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
      //

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
      //

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
      //

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

      std::string inner_hash_im_hex(arma::mat& x, unsigned int i, int new_width, int new_height, std::string& resize_method, int hash_size, int highfreq_factor, int method) {

        arma::mat tmp_out;

        arma::mat tmp_mat = UtilF.vec2mat(arma::conv_to< arma::rowvec >::from(x.row(i)), new_width, new_height);

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
      //

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
      //

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


      ~Image_Hashing() { }

  };


  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Warp Affine


  class Warp_Affine {

    public:

      Warp_Affine() { }


      // Affine transformation function
      //

      arma::mat getAffineTransform(arma::mat& original_points,
                                   arma::mat& transformed_points) {

        arma::uword NROWS = original_points.n_rows;
        arma::uword NCOLS = original_points.n_cols;
        if ((NROWS != transformed_points.n_rows) & (NCOLS != transformed_points.n_cols)) Rcpp::stop("The rows and columns of the input 'original_points' and 'transformed_points' must match!");
        arma::mat p(NROWS, NCOLS + 1);

        for (arma::uword k = 0; k < NROWS; k++) {
          arma::rowvec orig_row = original_points.row(k);
          arma::rowvec iter_vec = {orig_row(0), orig_row(1), 1.0};
          p.row(k) = iter_vec;
        }

        return arma::solve(p, transformed_points).t();
      }


      // change the image representation from "horizontal" to "vertical"  [ 3-dimensional ]
      //

      arma::cube to_mtx(arma::cube& img) {

        arma::uword H = img.n_rows;
        arma::uword V = img.n_cols;
        arma::uword C = img.n_slices;

        arma::cube mtr = arma::zeros(V,H,C);
        for (arma::uword i = 0; i < H; i++) {
          mtr.col(i) = arma::reshape(img.row(i), V, 1, C);
        }

        return mtr;
      }


      // change the image representation from "horizontal" to "vertical"  [ 2-dimensional ]
      //

      arma::mat to_mtx_2d(arma::mat& img) {

        arma::uword H = img.n_rows;
        arma::uword V = img.n_cols;

        arma::mat mtr = arma::zeros(V,H);
        for (arma::uword i = 0; i < H; i++) {
          mtr.col(i) = arma::reshape(img.row(i), V, 1);
        }

        return mtr;
      }


      // change the image representation from "vertical" to "horizontal"  [ 3-dimensional ]
      //

      arma::cube to_img(arma::cube& mtr) {

        arma::uword V = mtr.n_rows;
        arma::uword H = mtr.n_cols;
        arma::uword C = mtr.n_slices;

        arma::cube img = arma::zeros(H,V,C);
        for (arma::uword i = 0; i < V; i++) {
          img.col(i) = arma::reshape(mtr.row(i), H, 1, C);
        }

        return img;
      }


      // change the image representation from "vertical" to "horizontal"  [ 2-dimensional ]
      //

      arma::mat to_img_2d(arma::mat& mtr) {

        arma::uword V = mtr.n_rows;
        arma::uword H = mtr.n_cols;

        arma::mat img = arma::zeros(H,V);
        for (arma::uword i = 0; i < V; i++) {
          img.col(i) = arma::reshape(mtr.row(i), H, 1);
        }

        return img;
      }


      // warpAffine transformation  [ 3-dimensional ]
      //

      arma::cube warpAffine(arma::cube& img,
                            arma::mat& M,
                            arma::uword R,
                            arma::uword C) {

        arma::cube mtr = to_mtx(img);

        arma::uword H = mtr.n_rows;
        arma::uword V = mtr.n_cols;
        arma::uword SLICES = mtr.n_slices;

        arma::cube dst = arma::zeros(R, C, SLICES);

        for (arma::uword i = 0; i < H; i++) {
          for (arma::uword j = 0; j < V; j++) {

            double i_inp = i;
            double j_inp = j;

            arma::vec iter_vec({i_inp, j_inp});
            arma::vec dot_vec = arma::affmul(M, iter_vec);

            arma::uword i_dst = dot_vec(0);                   // "i_dist" and "j_dist" are unsigned integers thus no need to specify "i_dst >= 0" and "j_dst >= 0"
            arma::uword j_dst = dot_vec(1);

            if ((i_dst < R) & (j_dst < C)) {
              dst.tube(i_dst, j_dst) = mtr.tube(i,j);
            }
          }
        }

        return to_img(dst);
      }


      // warpAffine transformation  [ 2-dimensional ]
      //

      arma::mat warpAffine_2d(arma::mat& img,
                              arma::mat& M,
                              arma::uword R,
                              arma::uword C,
                              int threads = 1) {
        #ifdef _OPENMP
        omp_set_num_threads(threads);
        #endif

        arma::mat mtr = to_mtx_2d(img);

        arma::uword H = mtr.n_rows;
        arma::uword V = mtr.n_cols;

        arma::mat dst = arma::zeros(R, C);
        arma::uword i,j;

        #ifdef _OPENMP
        #pragma omp parallel for schedule(auto) shared(M, R, C, H, V, dst, mtr) private(i,j) collapse(2)
        #endif
        for (i = 0; i < H; i++) {
          for (j = 0; j < V; j++) {

            double i_inp = i;
            double j_inp = j;

            arma::vec iter_vec({i_inp, j_inp});
            arma::vec dot_vec = arma::affmul(M, iter_vec);

            arma::uword i_dst = dot_vec(0);                   // "i_dist" and "j_dist" are unsigned integers thus no need to specify "i_dst >= 0" and "j_dst >= 0"
            arma::uword j_dst = dot_vec(1);

            if ((i_dst < R) & (j_dst < C)) {
              #ifdef _OPENMP
              #pragma omp atomic write
              #endif
              dst(i_dst, j_dst) = mtr(i,j);
            }
          }
        }

        return to_img_2d(dst);
      }


      ~Warp_Affine() { }
  };

}

#endif
