context('Test writing images')


testthat::test_that("if the file_name is not a valid path it returns an error", {

  x = matrix(100, 10, 10)

  testthat::expect_error( writeImage(x, 5) )
})


testthat::test_that("if the input is not a valid path it returns an error", {

  x = list(matrix(100, 10, 10))

  testthat::expect_error( writeImage(x, 'file.png') )
})


testthat::test_that("if the file_name is not a valid path it returns an error", {

  x = matrix(100, 10, 10)

  testthat::expect_error( writeImage(x, '/home/my_image') )
})


testthat::test_that("writeImage saves a .png successfully", {

  x = matrix(100, 10, 10)

  sav = writeImage(x, 'SAV_DEL.png')

  file.remove('SAV_DEL.png')

  testthat::expect_silent( sav )
})


testthat::test_that("writeImage saves a .png successfully, in case of data frame", {

  x = data.frame(matrix(100, 10, 10))

  sav = writeImage(x, 'SAV_DEL.png')

  file.remove('SAV_DEL.png')

  testthat::expect_silent( sav )
})


testthat::test_that("writeImage saves a .jpg successfully", {

  x = matrix(100, 10, 10)

  sav = writeImage(x, 'SAV_DEL.jpg')

  file.remove('SAV_DEL.jpg')

  testthat::expect_silent( sav )
})


testthat::test_that("writeImage saves a .jpeg successfully", {

  x = array(runif(300), dim = c(10, 10, 3))

  sav = writeImage(x, 'SAV_DEL.jpeg')

  file.remove('SAV_DEL.jpeg')

  testthat::expect_silent( sav )
})


testthat::test_that("writeImage saves a .tiff successfully", {

  x = matrix(100, 10, 10)

  sav = writeImage(x, 'SAV_DEL.tiff')

  file.remove('SAV_DEL.tiff')

  testthat::expect_silent( sav )
})
