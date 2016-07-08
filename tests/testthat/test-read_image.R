context('Test reading images')



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

