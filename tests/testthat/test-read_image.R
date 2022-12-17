context('Test reading images')


testthat::test_that("the 'verify_image_extension' function gives an error if there is no file extension in the input image", {

  example_img_path = 'example_img'

  testthat::expect_error( verify_image_extension(image_path = example_img_path) )
})


testthat::test_that("the 'verify_image_extension' function returns only supported from the OpenImageR package file extensions", {

  vec_img_ext = c('png', 'PNG', 'jpg', 'JPG', 'jpeg', 'JPEG', 'tif', 'TIF', 'tiff', 'TIFF')

  vec_valid = sapply(vec_img_ext, function(x) {
    ext_iter = paste(c('example_image', x), collapse = '.')
    verify_image_extension(image_path = ext_iter)
  })

  testthat::expect_true( all(vec_img_ext == vec_valid) )
})


testthat::test_that("if the input is a not a valid path it returns an error", {

  testthat::expect_error( readImage(5) )
})


testthat::test_that("if the input is a not a valid path it returns an error", {

  testthat::expect_error( readImage('/home/lampros') )
})


testthat::test_that("if the input is a .png it retruns a 3-dimensional array", {

  Folder = paste0(getwd(), path.expand("/image_files/"))

  lst_files = list.files(Folder)

  path = lst_files[1]

  res = readImage( paste0(Folder, path) )

  testthat::expect_true( is.array(res) && length(dim(res)) == 3 && sum(dim(res)) > 0)
})



testthat::test_that("if the input is a .jpg it retruns a 3-dimensional array", {

  Folder = paste0(getwd(), path.expand("/image_files/"))

  lst_files = list.files(Folder)

  path = lst_files[2]

  res = readImage( paste0(Folder, path) )

  testthat::expect_true( is.array(res) && length(dim(res)) == 3 && sum(dim(res)) > 0)
})


testthat::test_that("if the input is a .jpeg it retruns a 3-dimensional array", {

  Folder = paste0(getwd(), path.expand("/image_files/"))

  lst_files = list.files(Folder)

  path = lst_files[3]

  res = readImage( paste0(Folder, path) )

  testthat::expect_true( is.array(res) && length(dim(res)) == 3 && sum(dim(res)) > 0)
})



testthat::test_that("if the input is a .tiff it retruns a matrix", {

  Folder = paste0(getwd(), path.expand("/image_files/"))

  lst_files = list.files(Folder)

  path = lst_files[4]

  res = readImage( paste0(Folder, path) )

  testthat::expect_true( is.matrix(res) && length(dim(res)) == 2 && sum(dim(res)) > 0)
})

