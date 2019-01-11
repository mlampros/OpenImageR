#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _OpenImageR_Array_range(SEXP, SEXP);
extern SEXP _OpenImageR_augment_array_list(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _OpenImageR_augment_transf(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _OpenImageR_augment_transf_array(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _OpenImageR_average_hash_binary(SEXP, SEXP, SEXP);
extern SEXP _OpenImageR_average_hash_string(SEXP, SEXP, SEXP);
extern SEXP _OpenImageR_bilinear_array(SEXP, SEXP, SEXP);
extern SEXP _OpenImageR_binary_to_hex(SEXP);
extern SEXP _OpenImageR_conv2d(SEXP, SEXP, SEXP);
extern SEXP _OpenImageR_conv3d(SEXP, SEXP, SEXP);
extern SEXP _OpenImageR_dhash_binary(SEXP, SEXP, SEXP);
extern SEXP _OpenImageR_dhash_string(SEXP, SEXP, SEXP);
extern SEXP _OpenImageR_diate_erode(SEXP, SEXP, SEXP, SEXP);
extern SEXP _OpenImageR_diate_erode_cube(SEXP, SEXP, SEXP, SEXP);
extern SEXP _OpenImageR_Gabor_export_Features(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _OpenImageR_Gabor_Filter_Bank(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _OpenImageR_Gabor_generate(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _OpenImageR_hash_image(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _OpenImageR_hash_image_cube(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _OpenImageR_hash_image_cube_hex(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _OpenImageR_hash_image_hex(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _OpenImageR_HOG_array(SEXP, SEXP, SEXP, SEXP);
extern SEXP _OpenImageR_hog_cpp(SEXP, SEXP, SEXP);
extern SEXP _OpenImageR_HOG_matrix(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _OpenImageR_im_flip(SEXP, SEXP);
extern SEXP _OpenImageR_im_flip_cube(SEXP, SEXP);
extern SEXP _OpenImageR_interface_superpixels(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _OpenImageR_levenshtein_dist(SEXP, SEXP);
extern SEXP _OpenImageR_list_2array_convert(SEXP);
extern SEXP _OpenImageR_LOAD_3d_data(SEXP);
extern SEXP _OpenImageR_meshgrid_x(SEXP, SEXP);
extern SEXP _OpenImageR_meshgrid_y(SEXP, SEXP);
extern SEXP _OpenImageR_MinMaxArray(SEXP);
extern SEXP _OpenImageR_MinMaxMatrix(SEXP);
extern SEXP _OpenImageR_Normalize_array(SEXP);
extern SEXP _OpenImageR_Normalize_matrix(SEXP);
extern SEXP _OpenImageR_phash_binary(SEXP, SEXP, SEXP, SEXP);
extern SEXP _OpenImageR_phash_string(SEXP, SEXP, SEXP, SEXP);
extern SEXP _OpenImageR_resize_bilinear_rcpp(SEXP, SEXP, SEXP);
extern SEXP _OpenImageR_resize_nearest_array(SEXP, SEXP, SEXP);
extern SEXP _OpenImageR_resize_nearest_rcpp(SEXP, SEXP, SEXP);
extern SEXP _OpenImageR_Rgb_2gray(SEXP);
extern SEXP _OpenImageR_RGB_to_hsv(SEXP);
extern SEXP _OpenImageR_rgbtolab(SEXP);
extern SEXP _OpenImageR_rotate_nearest_bilinear(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _OpenImageR_rotate_nearest_bilinear_array_full(SEXP, SEXP, SEXP, SEXP);
extern SEXP _OpenImageR_rotate_nearest_bilinear_array_same(SEXP, SEXP, SEXP, SEXP);
extern SEXP _OpenImageR_rotate_rcpp(SEXP, SEXP);
extern SEXP _OpenImageR_translation_mat(SEXP, SEXP, SEXP, SEXP);
extern SEXP _OpenImageR_zca_whiten_cube(SEXP, SEXP, SEXP);
extern SEXP _OpenImageR_zca_whitening(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_OpenImageR_Array_range",                        (DL_FUNC) &_OpenImageR_Array_range,                         2},
    {"_OpenImageR_augment_array_list",                 (DL_FUNC) &_OpenImageR_augment_array_list,                 15},
    {"_OpenImageR_augment_transf",                     (DL_FUNC) &_OpenImageR_augment_transf,                     15},
    {"_OpenImageR_augment_transf_array",               (DL_FUNC) &_OpenImageR_augment_transf_array,               15},
    {"_OpenImageR_average_hash_binary",                (DL_FUNC) &_OpenImageR_average_hash_binary,                 3},
    {"_OpenImageR_average_hash_string",                (DL_FUNC) &_OpenImageR_average_hash_string,                 3},
    {"_OpenImageR_bilinear_array",                     (DL_FUNC) &_OpenImageR_bilinear_array,                      3},
    {"_OpenImageR_binary_to_hex",                      (DL_FUNC) &_OpenImageR_binary_to_hex,                       1},
    {"_OpenImageR_conv2d",                             (DL_FUNC) &_OpenImageR_conv2d,                              3},
    {"_OpenImageR_conv3d",                             (DL_FUNC) &_OpenImageR_conv3d,                              3},
    {"_OpenImageR_dhash_binary",                       (DL_FUNC) &_OpenImageR_dhash_binary,                        3},
    {"_OpenImageR_dhash_string",                       (DL_FUNC) &_OpenImageR_dhash_string,                        3},
    {"_OpenImageR_diate_erode",                        (DL_FUNC) &_OpenImageR_diate_erode,                         4},
    {"_OpenImageR_diate_erode_cube",                   (DL_FUNC) &_OpenImageR_diate_erode_cube,                    4},
    {"_OpenImageR_Gabor_export_Features",              (DL_FUNC) &_OpenImageR_Gabor_export_Features,              11},
    {"_OpenImageR_Gabor_Filter_Bank",                  (DL_FUNC) &_OpenImageR_Gabor_Filter_Bank,                   5},
    {"_OpenImageR_Gabor_generate",                     (DL_FUNC) &_OpenImageR_Gabor_generate,                     12},
    {"_OpenImageR_hash_image",                         (DL_FUNC) &_OpenImageR_hash_image,                          8},
    {"_OpenImageR_hash_image_cube",                    (DL_FUNC) &_OpenImageR_hash_image_cube,                     6},
    {"_OpenImageR_hash_image_cube_hex",                (DL_FUNC) &_OpenImageR_hash_image_cube_hex,                 6},
    {"_OpenImageR_hash_image_hex",                     (DL_FUNC) &_OpenImageR_hash_image_hex,                      8},
    {"_OpenImageR_HOG_array",                          (DL_FUNC) &_OpenImageR_HOG_array,                           4},
    {"_OpenImageR_hog_cpp",                            (DL_FUNC) &_OpenImageR_hog_cpp,                             3},
    {"_OpenImageR_HOG_matrix",                         (DL_FUNC) &_OpenImageR_HOG_matrix,                          6},
    {"_OpenImageR_im_flip",                            (DL_FUNC) &_OpenImageR_im_flip,                             2},
    {"_OpenImageR_im_flip_cube",                       (DL_FUNC) &_OpenImageR_im_flip_cube,                        2},
    {"_OpenImageR_interface_superpixels",              (DL_FUNC) &_OpenImageR_interface_superpixels,               9},
    {"_OpenImageR_levenshtein_dist",                   (DL_FUNC) &_OpenImageR_levenshtein_dist,                    2},
    {"_OpenImageR_list_2array_convert",                (DL_FUNC) &_OpenImageR_list_2array_convert,                 1},
    {"_OpenImageR_LOAD_3d_data",                       (DL_FUNC) &_OpenImageR_LOAD_3d_data,                        1},
    {"_OpenImageR_meshgrid_x",                         (DL_FUNC) &_OpenImageR_meshgrid_x,                          2},
    {"_OpenImageR_meshgrid_y",                         (DL_FUNC) &_OpenImageR_meshgrid_y,                          2},
    {"_OpenImageR_MinMaxArray",                        (DL_FUNC) &_OpenImageR_MinMaxArray,                         1},
    {"_OpenImageR_MinMaxMatrix",                       (DL_FUNC) &_OpenImageR_MinMaxMatrix,                        1},
    {"_OpenImageR_Normalize_array",                    (DL_FUNC) &_OpenImageR_Normalize_array,                     1},
    {"_OpenImageR_Normalize_matrix",                   (DL_FUNC) &_OpenImageR_Normalize_matrix,                    1},
    {"_OpenImageR_phash_binary",                       (DL_FUNC) &_OpenImageR_phash_binary,                        4},
    {"_OpenImageR_phash_string",                       (DL_FUNC) &_OpenImageR_phash_string,                        4},
    {"_OpenImageR_resize_bilinear_rcpp",               (DL_FUNC) &_OpenImageR_resize_bilinear_rcpp,                3},
    {"_OpenImageR_resize_nearest_array",               (DL_FUNC) &_OpenImageR_resize_nearest_array,                3},
    {"_OpenImageR_resize_nearest_rcpp",                (DL_FUNC) &_OpenImageR_resize_nearest_rcpp,                 3},
    {"_OpenImageR_Rgb_2gray",                          (DL_FUNC) &_OpenImageR_Rgb_2gray,                           1},
    {"_OpenImageR_RGB_to_hsv",                         (DL_FUNC) &_OpenImageR_RGB_to_hsv,                          1},
    {"_OpenImageR_rgbtolab",                           (DL_FUNC) &_OpenImageR_rgbtolab,                            1},
    {"_OpenImageR_rotate_nearest_bilinear",            (DL_FUNC) &_OpenImageR_rotate_nearest_bilinear,             5},
    {"_OpenImageR_rotate_nearest_bilinear_array_full", (DL_FUNC) &_OpenImageR_rotate_nearest_bilinear_array_full,  4},
    {"_OpenImageR_rotate_nearest_bilinear_array_same", (DL_FUNC) &_OpenImageR_rotate_nearest_bilinear_array_same,  4},
    {"_OpenImageR_rotate_rcpp",                        (DL_FUNC) &_OpenImageR_rotate_rcpp,                         2},
    {"_OpenImageR_translation_mat",                    (DL_FUNC) &_OpenImageR_translation_mat,                     4},
    {"_OpenImageR_zca_whiten_cube",                    (DL_FUNC) &_OpenImageR_zca_whiten_cube,                     3},
    {"_OpenImageR_zca_whitening",                      (DL_FUNC) &_OpenImageR_zca_whitening,                       3},
    {NULL, NULL, 0}
};

void R_init_OpenImageR(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
