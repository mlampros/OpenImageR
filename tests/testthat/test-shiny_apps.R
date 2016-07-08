context("Shiny apps")

#  here I use the try catch function 
testthat::test_that("in case that the file_path argument of the image_show function is not an array, matrix or path then it returns an error", {
  
  image_array = list(image_array = matrix(runif(25), 5, 5))
  
  testthat::expect_error(imageShow(image_array))
})

