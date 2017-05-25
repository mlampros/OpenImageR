#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP OpenImageR_Array_range(SEXP, SEXP);
extern SEXP OpenImageR_augment_array_list(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP OpenImageR_augment_transf(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP OpenImageR_augment_transf_array(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP OpenImageR_average_hash_binary(SEXP, SEXP, SEXP);
extern SEXP OpenImageR_average_hash_string(SEXP, SEXP, SEXP);
extern SEXP OpenImageR_bilinear_array(SEXP, SEXP, SEXP);
extern SEXP OpenImageR_binary_to_hex(SEXP);
extern SEXP OpenImageR_conv2d(SEXP, SEXP, SEXP);
extern SEXP OpenImageR_conv3d(SEXP, SEXP, SEXP);
extern SEXP OpenImageR_dct_2d(SEXP);
extern SEXP OpenImageR_dhash_binary(SEXP, SEXP, SEXP);
extern SEXP OpenImageR_dhash_string(SEXP, SEXP, SEXP);
extern SEXP OpenImageR_diate_erode(SEXP, SEXP, SEXP, SEXP);
extern SEXP OpenImageR_diate_erode_cube(SEXP, SEXP, SEXP, SEXP);
extern SEXP OpenImageR_func_dct(SEXP);
extern SEXP OpenImageR_hash_image(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP OpenImageR_hash_image_cube(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP OpenImageR_hash_image_cube_hex(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP OpenImageR_hash_image_hex(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP OpenImageR_HOG_array(SEXP, SEXP, SEXP, SEXP);
extern SEXP OpenImageR_hog_cpp(SEXP, SEXP, SEXP);
extern SEXP OpenImageR_HOG_matrix(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP OpenImageR_im_flip(SEXP, SEXP);
extern SEXP OpenImageR_im_flip_cube(SEXP, SEXP);
extern SEXP OpenImageR_indices(SEXP, SEXP);
extern SEXP OpenImageR_indices_bilinear(SEXP, SEXP);
extern SEXP OpenImageR_levenshtein_dist(SEXP, SEXP);
extern SEXP OpenImageR_list_2array_convert(SEXP);
extern SEXP OpenImageR_meshgrid_x(SEXP, SEXP);
extern SEXP OpenImageR_meshgrid_y(SEXP, SEXP);
extern SEXP OpenImageR_MinMaxArray(SEXP);
extern SEXP OpenImageR_MinMaxMatrix(SEXP);
extern SEXP OpenImageR_mod(SEXP, SEXP);
extern SEXP OpenImageR_Normalize_array(SEXP);
extern SEXP OpenImageR_Normalize_matrix(SEXP);
extern SEXP OpenImageR_phash_binary(SEXP, SEXP, SEXP, SEXP);
extern SEXP OpenImageR_phash_string(SEXP, SEXP, SEXP, SEXP);
extern SEXP OpenImageR_removeMean(SEXP);
extern SEXP OpenImageR_replaceVal(SEXP, SEXP, SEXP, SEXP);
extern SEXP OpenImageR_resize_bilinear_rcpp(SEXP, SEXP, SEXP);
extern SEXP OpenImageR_resize_nearest_array(SEXP, SEXP, SEXP);
extern SEXP OpenImageR_resize_nearest_rcpp(SEXP, SEXP, SEXP);
extern SEXP OpenImageR_rgb_2gray(SEXP);
extern SEXP OpenImageR_rotate_nearest_bilinear(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP OpenImageR_rotate_nearest_bilinear_array_full(SEXP, SEXP, SEXP, SEXP);
extern SEXP OpenImageR_rotate_nearest_bilinear_array_same(SEXP, SEXP, SEXP, SEXP);
extern SEXP OpenImageR_rotate_rcpp(SEXP, SEXP);
extern SEXP OpenImageR_round_rcpp(SEXP, SEXP);
extern SEXP OpenImageR_seq_rcpp(SEXP);
extern SEXP OpenImageR_seq_rcpp_range(SEXP, SEXP);
extern SEXP OpenImageR_svd_arma_econ(SEXP);
extern SEXP OpenImageR_translation_mat(SEXP, SEXP, SEXP, SEXP);
extern SEXP OpenImageR_vec2mat(SEXP, SEXP, SEXP);
extern SEXP OpenImageR_vec2mat_colwise(SEXP, SEXP, SEXP);
extern SEXP OpenImageR_Vectz(SEXP);
extern SEXP OpenImageR_zca_whiten_cube(SEXP, SEXP, SEXP);
extern SEXP OpenImageR_zca_whitening(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"OpenImageR_Array_range",                        (DL_FUNC) &OpenImageR_Array_range,                         2},
    {"OpenImageR_augment_array_list",                 (DL_FUNC) &OpenImageR_augment_array_list,                 15},
    {"OpenImageR_augment_transf",                     (DL_FUNC) &OpenImageR_augment_transf,                     15},
    {"OpenImageR_augment_transf_array",               (DL_FUNC) &OpenImageR_augment_transf_array,               16},
    {"OpenImageR_average_hash_binary",                (DL_FUNC) &OpenImageR_average_hash_binary,                 3},
    {"OpenImageR_average_hash_string",                (DL_FUNC) &OpenImageR_average_hash_string,                 3},
    {"OpenImageR_bilinear_array",                     (DL_FUNC) &OpenImageR_bilinear_array,                      3},
    {"OpenImageR_binary_to_hex",                      (DL_FUNC) &OpenImageR_binary_to_hex,                       1},
    {"OpenImageR_conv2d",                             (DL_FUNC) &OpenImageR_conv2d,                              3},
    {"OpenImageR_conv3d",                             (DL_FUNC) &OpenImageR_conv3d,                              3},
    {"OpenImageR_dct_2d",                             (DL_FUNC) &OpenImageR_dct_2d,                              1},
    {"OpenImageR_dhash_binary",                       (DL_FUNC) &OpenImageR_dhash_binary,                        3},
    {"OpenImageR_dhash_string",                       (DL_FUNC) &OpenImageR_dhash_string,                        3},
    {"OpenImageR_diate_erode",                        (DL_FUNC) &OpenImageR_diate_erode,                         4},
    {"OpenImageR_diate_erode_cube",                   (DL_FUNC) &OpenImageR_diate_erode_cube,                    4},
    {"OpenImageR_func_dct",                           (DL_FUNC) &OpenImageR_func_dct,                            1},
    {"OpenImageR_hash_image",                         (DL_FUNC) &OpenImageR_hash_image,                          8},
    {"OpenImageR_hash_image_cube",                    (DL_FUNC) &OpenImageR_hash_image_cube,                     6},
    {"OpenImageR_hash_image_cube_hex",                (DL_FUNC) &OpenImageR_hash_image_cube_hex,                 6},
    {"OpenImageR_hash_image_hex",                     (DL_FUNC) &OpenImageR_hash_image_hex,                      8},
    {"OpenImageR_HOG_array",                          (DL_FUNC) &OpenImageR_HOG_array,                           4},
    {"OpenImageR_hog_cpp",                            (DL_FUNC) &OpenImageR_hog_cpp,                             3},
    {"OpenImageR_HOG_matrix",                         (DL_FUNC) &OpenImageR_HOG_matrix,                          6},
    {"OpenImageR_im_flip",                            (DL_FUNC) &OpenImageR_im_flip,                             2},
    {"OpenImageR_im_flip_cube",                       (DL_FUNC) &OpenImageR_im_flip_cube,                        2},
    {"OpenImageR_indices",                            (DL_FUNC) &OpenImageR_indices,                             2},
    {"OpenImageR_indices_bilinear",                   (DL_FUNC) &OpenImageR_indices_bilinear,                    2},
    {"OpenImageR_levenshtein_dist",                   (DL_FUNC) &OpenImageR_levenshtein_dist,                    2},
    {"OpenImageR_list_2array_convert",                (DL_FUNC) &OpenImageR_list_2array_convert,                 1},
    {"OpenImageR_meshgrid_x",                         (DL_FUNC) &OpenImageR_meshgrid_x,                          2},
    {"OpenImageR_meshgrid_y",                         (DL_FUNC) &OpenImageR_meshgrid_y,                          2},
    {"OpenImageR_MinMaxArray",                        (DL_FUNC) &OpenImageR_MinMaxArray,                         1},
    {"OpenImageR_MinMaxMatrix",                       (DL_FUNC) &OpenImageR_MinMaxMatrix,                        1},
    {"OpenImageR_mod",                                (DL_FUNC) &OpenImageR_mod,                                 2},
    {"OpenImageR_Normalize_array",                    (DL_FUNC) &OpenImageR_Normalize_array,                     1},
    {"OpenImageR_Normalize_matrix",                   (DL_FUNC) &OpenImageR_Normalize_matrix,                    1},
    {"OpenImageR_phash_binary",                       (DL_FUNC) &OpenImageR_phash_binary,                        4},
    {"OpenImageR_phash_string",                       (DL_FUNC) &OpenImageR_phash_string,                        4},
    {"OpenImageR_removeMean",                         (DL_FUNC) &OpenImageR_removeMean,                          1},
    {"OpenImageR_replaceVal",                         (DL_FUNC) &OpenImageR_replaceVal,                          4},
    {"OpenImageR_resize_bilinear_rcpp",               (DL_FUNC) &OpenImageR_resize_bilinear_rcpp,                3},
    {"OpenImageR_resize_nearest_array",               (DL_FUNC) &OpenImageR_resize_nearest_array,                3},
    {"OpenImageR_resize_nearest_rcpp",                (DL_FUNC) &OpenImageR_resize_nearest_rcpp,                 3},
    {"OpenImageR_rgb_2gray",                          (DL_FUNC) &OpenImageR_rgb_2gray,                           1},
    {"OpenImageR_rotate_nearest_bilinear",            (DL_FUNC) &OpenImageR_rotate_nearest_bilinear,             5},
    {"OpenImageR_rotate_nearest_bilinear_array_full", (DL_FUNC) &OpenImageR_rotate_nearest_bilinear_array_full,  4},
    {"OpenImageR_rotate_nearest_bilinear_array_same", (DL_FUNC) &OpenImageR_rotate_nearest_bilinear_array_same,  4},
    {"OpenImageR_rotate_rcpp",                        (DL_FUNC) &OpenImageR_rotate_rcpp,                         2},
    {"OpenImageR_round_rcpp",                         (DL_FUNC) &OpenImageR_round_rcpp,                          2},
    {"OpenImageR_seq_rcpp",                           (DL_FUNC) &OpenImageR_seq_rcpp,                            1},
    {"OpenImageR_seq_rcpp_range",                     (DL_FUNC) &OpenImageR_seq_rcpp_range,                      2},
    {"OpenImageR_svd_arma_econ",                      (DL_FUNC) &OpenImageR_svd_arma_econ,                       1},
    {"OpenImageR_translation_mat",                    (DL_FUNC) &OpenImageR_translation_mat,                     4},
    {"OpenImageR_vec2mat",                            (DL_FUNC) &OpenImageR_vec2mat,                             3},
    {"OpenImageR_vec2mat_colwise",                    (DL_FUNC) &OpenImageR_vec2mat_colwise,                     3},
    {"OpenImageR_Vectz",                              (DL_FUNC) &OpenImageR_Vectz,                               1},
    {"OpenImageR_zca_whiten_cube",                    (DL_FUNC) &OpenImageR_zca_whiten_cube,                     3},
    {"OpenImageR_zca_whitening",                      (DL_FUNC) &OpenImageR_zca_whitening,                       3},
    {NULL, NULL, 0}
};

void R_init_OpenImageR(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
