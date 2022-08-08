

context("Warp Affine")


# Affine transformation function

testthat::test_that("the 'getAffineTransform' function gives an error if the number of rows and columns of the input matrices are not the same", {

  r = 600
  c = 600
  offset = 50

  original_points = matrix(data = c(0, 0, r, 0, 0, c, 0, 0),
                           nrow = 4,
                           ncol = 2,
                           byrow = TRUE)

  transformed_points = matrix(data = c(offset, 0, r, offset, 0, c-offset),
                              nrow = 3,
                              ncol = 2,
                              byrow = TRUE)

  testthat::expect_error( getAffineTransform(original_points = original_points,
                                             transformed_points = transformed_points) )
})


testthat::test_that("the output matrix has the same dimensions as the input matrices", {

  r = 600
  c = 600
  offset = 50

  original_points = matrix(data = c(0, 0, r, 0, 0, c),
                           nrow = 3,
                           ncol = 2,
                           byrow = TRUE)

  transformed_points = matrix(data = c(offset, 0, r, offset, 0, c-offset),
                              nrow = 3,
                              ncol = 2,
                              byrow = TRUE)

  M_aff = getAffineTransform(original_points = original_points,
                             transformed_points = transformed_points)

  testthat::expect_true( all(dim(M_aff) == rev(dim(original_points))) & all(dim(M_aff) == rev(dim(transformed_points))) )
})


# Warp Affine function


testthat::test_that("the warp affine function works for 2- and 3-dimensional data", {

  path = file.path(getwd(), 'image_files', '3.jpeg')
  img = readImage(path)
  img = img * 255

  r = ncol(img)
  c = nrow(img)
  offset = 10

  original_points = matrix(data = c(0, 0, r, 0, 0, c),
                           nrow = 3,
                           ncol = 2,
                           byrow = TRUE)

  transformed_points = matrix(data = c(offset, 0, r, offset, 0, c-offset),
                              nrow = 3,
                              ncol = 2,
                              byrow = TRUE)

  M_aff = getAffineTransform(original_points = original_points,
                             transformed_points = transformed_points)

  #..............
  # 2-dimensional
  #..............

  img_2d = rgb_2gray(img)

  res_2d = warpAffine(img = img_2d,
                      M = M_aff,
                      R = r,
                      C = c,
                      threads = 1,
                      verbose = FALSE)
  #..............
  # 3-dimensional
  #..............

  res_3d = warpAffine(img = img,
                      M = M_aff,
                      R = r,
                      C = c,
                      verbose = FALSE)

  testthat::expect_true( all(dim(res_2d) == c(32, 32)) & all(dim(res_3d) == c(32, 32, 3)) )
})

