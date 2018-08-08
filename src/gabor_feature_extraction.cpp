# include <RcppArmadillo.h>
// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::plugins(openmp)]]
// [[Rcpp::plugins(cpp11)]]


/**
 * Copyright (c) 2015, Mohammad Haghighat
 *
 * All rights reserved.
 *
 * https://github.com/mhaghighat/gabor
 *
 **/



#ifdef _OPENMP
#include <omp.h>
#endif



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

    Rcpp::List return_gaborFeatures(bool plot_data) {
      
      arma::Mat<double> local_energy  = arma::sum(arma::pow(gabor_features, 2.0), 0);
      
      arma::Mat<double> mean_aplitude = arma::sum(arma::abs(gabor_features), 0);
      
      arma::Mat<double> concat = arma::join_rows(local_energy, mean_aplitude);
      
      gabor_features_Magn = arma::vectorise(gabor_features_Magn).t();
      
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



//------------------------------------------------------------------------------------------------------------------------------------------------------------------- exported functions


// return data for the 'gaborFilterBank'
//

// [[Rcpp::export]]
Rcpp::List Gabor_Filter_Bank(int u, int v, int m, int n, bool plot_data = false) {

  Gabor_Features gab_fts;

  Gabor_Features_Rcpp gab_fts_Rcpp;

  gab_fts.gaborFilterBank(u, v, m, n, plot_data);

  gabor_filt_bnk gbf_dat = gab_fts.return_gaborFilterBank();

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
                                 
                                 bool plot_data = false, bool normalize_features = false, int threads = 1) {

  Gabor_Features_Rcpp gab_fts_Rcpp;

  arma::Mat<arma::cx_double> img_cx = gab_fts_Rcpp.conv2complex(img);

  gab_fts_Rcpp.gaborFeaturesRcpp(img_cx, u, v, m, n, d1, d2, downsample_vec, plot_data, normalize_features, threads);

  return gab_fts_Rcpp.return_gaborFeatures(plot_data);
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
    
    Gabor_Features gab_fts;
    
    gabor_feats_obj concat = gab_fts.inner_func_gabor(i, img_data, img_nrow, img_ncol, d1, d2, u, v, m, n, downsample_vec, normalize_features);

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

//-------------------------------------------------------------------------------------------------------------------------------------------------------------------
