
MATR = matrix(100, 10, 10)

DF = as.data.frame(MATR)

ARRAY = array(unlist(list(matrix(runif(100), 10, 10), matrix(runif(100), 10, 10), matrix(runif(100), 10, 10))), dim = c(10, 10, 3))


context("Hog descriptors")


# HOG - function

testthat::test_that("if the input is a matrix it returns a vector of length cells ^ 2 * orientations ", {

  cls = 3
  ornt = 6

  res = HOG(MATR, cells = cls, orientations = ornt)

  testthat::expect_true( is.vector(res) && length(res) == cls ^ 2 * ornt )
})


testthat::test_that("if the input is a data frame it returns a vector of length cells ^ 2 * orientations ", {

  cls = 3
  ornt = 6

  res = HOG(DF, cells = cls, orientations = ornt)

  testthat::expect_true( is.vector(res) && length(res) == cls ^ 2 * ornt )
})


testthat::test_that("if the input is an array it returns a vector of length cells ^ 2 * orientations ", {

  cls = 3
  ornt = 6

  res = HOG(ARRAY, cells = cls, orientations = ornt)

  testthat::expect_true( is.vector(res) && length(res) == cls ^ 2 * ornt )
})


testthat::test_that("if the input is INVALID it returns an error", {

  cls = 3
  ornt = 6

  testthat::expect_error( HOG(list(MATR), cells = cls, orientations = ornt) )
})



# func_transform - function

testthat::test_that("the func_transform function loads a .png image successfully and returns the correct dimensions", {

  path = paste0(getwd(), path.expand("/image_files/"))

  lst_files = list.files(path)

  image_1 = lst_files[1]         # load .png image

  fl = strsplit(image_1, '[.]')[[1]][2]

  is_gray = func_transform(image_1, path, fl, T)

  testthat::expect_true( length(dim(is_gray)) == 2 && sum(dim(is_gray)) != 0)
})


testthat::test_that("the func_transform function loads a .jpg image successfully and returns the correct dimensions", {

  path = paste0(getwd(), path.expand("/image_files/"))

  lst_files = list.files(path)

  image_1 = lst_files[2]           # load .jpg image

  fl = strsplit(image_1, '[.]')[[1]][2]

  is_gray = func_transform(image_1, path, fl, T)

  testthat::expect_true( length(dim(is_gray)) == 2 && sum(dim(is_gray)) != 0)
})


testthat::test_that("the func_transform function loads a .jpeg image successfully and returns the correct dimensions", {

  path = paste0(getwd(), path.expand("/image_files/"))

  lst_files = list.files(path)

  image_1 = lst_files[3]           # load .jpeg image

  fl = strsplit(image_1, '[.]')[[1]][2]

  is_gray = func_transform(image_1, path, fl, T)

  testthat::expect_true( length(dim(is_gray)) == 2 && sum(dim(is_gray)) != 0)
})


testthat::test_that("the func_transform function loads a .tiff image successfully and returns the correct dimensions", {

  path = paste0(getwd(), path.expand("/image_files/"))

  lst_files = list.files(path)

  image_1 = lst_files[4]           # load .tiff image

  fl = strsplit(image_1, '[.]')[[1]][2]

  is_gray = func_transform(image_1, path, fl, F)

  testthat::expect_true( length(dim(is_gray)) == 2 && sum(dim(is_gray)) != 0)
})


testthat::test_that("the func_transform function returns an error for an invalid image type", {

  image_1 = 'invalid.inv'

  fl = 'inv'

  testthat::expect_error( func_transform(image_1, path, fl, T) )
})


# HOG_apply - function

# error - handling

testthat::test_that("the HOG_apply function returns an error if threads less than 1", {

  testthat::expect_error( HOG_apply(ARRAY, cells = 2, orientations = 2, rows = NULL, columns = NULL, threads = 0) )
})


testthat::test_that("the HOG_apply function returns an error if object is a matrix and columns or rows are NULL", {

  testthat::expect_error( HOG_apply(MATR, cells = 2, orientations = 2, rows = NULL, columns = NULL, threads = 1) )
})


testthat::test_that("the HOG_apply function returns an error if the cells or orientations are less than 1", {

  testthat::expect_error( HOG_apply(ARRAY, cells = 0, orientations = 2, rows = NULL, columns = NULL, threads = 1) )
})


testthat::test_that("the HOG_apply function returns an error if the path is incorrect (it lacks a slash)", {

  path = paste0(getwd(), path.expand("/image_files"))

  testthat::expect_error( HOG_apply(path, cells = 2, orientations = 2, rows = NULL, columns = NULL, threads = 1) )
})


testthat::test_that("the HOG_apply function returns an error if the path is correct but the image type is invalid", {

  path = paste0(getwd(), path.expand("/image_files/invalid_image/"))

  testthat::expect_error( HOG_apply(path, cells = 2, orientations = 2, rows = NULL, columns = NULL, threads = 1) )
})


testthat::test_that("the HOG_apply function returns an error if the input object is not one of the supported types", {

  testthat::expect_error( HOG_apply(list(MATR), cells = 2, orientations = 2, rows = NULL, columns = NULL, threads = 1) )
})


testthat::test_that("the HOG_apply function returns an error if each row of the input is not equal to rows * columns", {

  NROW = 5
  NCOL = 25
  tmp_x = matrix(runif(NROW * NCOL), ncol = NCOL, nrow = NROW)

  testthat::expect_error( HOG_apply(tmp_x, cells = 3, orientations = 5, rows = 5, columns = 6, threads = 1) )
})


# HOG_apply function


testthat::test_that("the HOG_apply function returns a list of length 2 when the input object is a path to a folder of images", {

  path = paste0(getwd(), path.expand("/image_files/HOG_apply_folder/"))

  len = list.files(path)

  cls = 3
  ornt = 6

  res = HOG_apply(path, cells = cls, orientations = ornt, rows = NULL, columns = NULL, threads = 1)

  testthat::expect_true( length(res) == 2 && length(len) == nrow(res$hog) && cls ^ 2 * ornt == ncol(res$hog) && sum(len %in% res$files) == length(len) )
})



testthat::test_that("the HOG_apply function returns a matrix when the input object is a matrix", {

  img_height = 10
  img_width = 10

  matrix1 = matrix(runif(1000), img_height ^ 2, img_width ^ 2)

  cls = 3
  ornt = 6

  res = HOG_apply(matrix1, cells = cls, orientations = ornt, rows = img_width, columns = img_height, threads = 1)

  testthat::expect_true( nrow(res) == nrow(matrix1) && cls ^ 2 * ornt == ncol(res) )
})



testthat::test_that("the HOG_apply function returns a matrix when the input object is an array", {

  img_height = 10
  img_width = 10

  ARRAY1 = array(unlist(list(matrix(runif(1000), img_width ^ 2, img_height ^ 2), matrix(runif(1000), img_width ^ 2, img_height ^ 2),

                             matrix(runif(1000), img_width ^ 2, img_height ^ 2))), dim = c(100, 100, 3))

  cls = 3
  ornt = 6

  res = HOG_apply(ARRAY1, cells = cls, orientations = ornt, threads = 1)

  testthat::expect_true( nrow(res) == dim(ARRAY1)[3] && cls ^ 2 * ornt == ncol(res) )
})
