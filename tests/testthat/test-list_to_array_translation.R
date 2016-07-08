
test_mat = matrix(0, 100, 100)

test_df = data.frame(matrix(0, 100, 100))

test_array = array(0, dim = c(100, 100, 3))

test_lst = list(matrix(0, 10, 10), matrix(0, 10, 10), matrix(0, 10, 10), matrix(0, 10, 10), matrix(0, 10, 10), matrix(0, 10, 10))



context("List to array and translation")

########################
# List_2_array function
########################


testthat::test_that("the function List_2_Array returns an error if the data is not a List", {
  
  testthat::expect_error( List_2_Array(test_lst[[1]]) )
})


testthat::test_that("the function List_2_Array returns an error if the sublists of the data are not matrices", {
  
  tmp_lst = list(data.frame(0, 10, 10), matrix(0, 10, 10), matrix(0, 10, 10), matrix(0, 10, 10), matrix(0, 10, 10), matrix(0, 10, 10))
  
  testthat::expect_error( List_2_Array(tmp_lst) )
})


testthat::test_that("the function List_2_Array returns an error if the dimensions of the sublists are not equal", {
  
  tmp_lst = list(matrix(0, 100, 10), matrix(0, 10, 10), matrix(0, 10, 10), matrix(0, 10, 100), matrix(0, 10, 10), matrix(0, 10, 10))
  
  testthat::expect_error( List_2_Array(tmp_lst) )
})


testthat::test_that("the function List_2_Array returns the correct output, if verbose is TRUE", {
  
  res = List_2_Array(test_lst, verbose = T)
  
  testthat::expect_true( class(res) == 'array' && mean(apply(res, 3, nrow)) == mean(unlist(lapply(test_lst, nrow))) && mean(apply(res, 3, ncol)) == mean(unlist(lapply(test_lst, ncol))))
})


testthat::test_that("the function List_2_Array returns the correct output, if verbose is FALSE", {
  
  res = List_2_Array(test_lst, verbose = F)
  
  testthat::expect_true( class(res) == 'array' && mean(apply(res, 3, nrow)) == mean(unlist(lapply(test_lst, nrow))) && mean(apply(res, 3, ncol)) == mean(unlist(lapply(test_lst, ncol))))
})


#######################
# translation function
#######################

testthat::test_that("the function translation returns an error if both shift_rows and shift_cols are 0", {
  
  testthat::expect_error( translation(test_array, shift_rows = 0, shift_cols = 0) )
})


testthat::test_that("the function translation returns an error if the input image is not a matrix, data frame or array (in this case input data is a vector)", {
  
  testthat::expect_error( translation(1:10, shift_rows = 10, shift_cols = 0) )
})


testthat::test_that("the function translation returns an error if the input image is not a matrix, data frame or array (in this case input data is a list)", {
  
  testthat::expect_error( translation(list(1:10), shift_rows = 10, shift_cols = 0) )
})


# matrix

testthat::test_that("the function translation returns the correct output in case of matrix and shift_rows and shift_cols are positive", {
  
  res = translation(test_mat, shift_rows = 10, shift_cols = 10)
  
  testthat::expect_true( is.matrix(res) && nrow(res) == nrow(test_mat) && ncol(res) == ncol(test_mat) )
})


testthat::test_that("the function translation returns the correct output in case of matrix and shift_rows and shift_cols are negative", {
  
  res = translation(test_mat, shift_rows = -10, shift_cols = -10)
  
  testthat::expect_true( is.matrix(res) && nrow(res) == nrow(test_mat) && ncol(res) == ncol(test_mat) )
})


testthat::test_that("the function translation returns the correct output in case of matrix and shift_rows are positive and shift_cols are negative", {
  
  res = translation(test_mat, shift_rows = 10, shift_cols = -10)
  
  testthat::expect_true( is.matrix(res) && nrow(res) == nrow(test_mat) && ncol(res) == ncol(test_mat) )
})


testthat::test_that("the function translation returns the correct output in case of matrix and shift_rows are negative and shift_cols are positive", {
  
  res = translation(test_mat, shift_rows = -10, shift_cols = 10)
  
  testthat::expect_true( is.matrix(res) && nrow(res) == nrow(test_mat) && ncol(res) == ncol(test_mat) )
})


# data frame

testthat::test_that("the function translation returns the correct output in case of data frame and shift_rows and shift_cols are positive", {
  
  res = translation(test_df, shift_rows = 10, shift_cols = 10)
  
  testthat::expect_true( is.matrix(res) && nrow(res) == nrow(test_df) && ncol(res) == ncol(test_df) )
})


testthat::test_that("the function translation returns the correct output in case of data frame and shift_rows and shift_cols are negative", {
  
  res = translation(test_df, shift_rows = -10, shift_cols = -10)
  
  testthat::expect_true( is.matrix(res) && nrow(res) == nrow(test_df) && ncol(res) == ncol(test_df) )
})


testthat::test_that("the function translation returns the correct output in case of data frame and shift_rows are positive and shift_cols are negative", {
  
  res = translation(test_df, shift_rows = 10, shift_cols = -10)
  
  testthat::expect_true( is.matrix(res) && nrow(res) == nrow(test_df) && ncol(res) == ncol(test_df) )
})


testthat::test_that("the function translation returns the correct output in case of data frame and shift_rows are negative and shift_cols are positive", {
  
  res = translation(test_df, shift_rows = -10, shift_cols = 10)
  
  testthat::expect_true( is.matrix(res) && nrow(res) == nrow(test_df) && ncol(res) == ncol(test_df) )
})



# array

testthat::test_that("the function translation returns the correct output in case of array and shift_rows and shift_cols are positive", {
  
  res = translation(test_array, shift_rows = 10, shift_cols = 10)
  
  testthat::expect_true( is.array(res) && mean(apply(res, 3, nrow)) == mean(apply(test_array, 3, nrow)) && mean(apply(res, 3, ncol)) == mean(apply(test_array, 3, nrow)) )
})


testthat::test_that("the function translation returns the correct output in case of array and shift_rows and shift_cols are negative", {
  
  res = translation(test_array, shift_rows = -10, shift_cols = -10)
  
  testthat::expect_true( is.array(res) && mean(apply(res, 3, nrow)) == mean(apply(test_array, 3, nrow)) && mean(apply(res, 3, ncol)) == mean(apply(test_array, 3, nrow)) )
})


testthat::test_that("the function translation returns the correct output in case of array and shift_rows are positive and shift_cols are negative", {
  
  res = translation(test_array, shift_rows = 10, shift_cols = -10)
  
  testthat::expect_true( is.array(res) && mean(apply(res, 3, nrow)) == mean(apply(test_array, 3, nrow)) && mean(apply(res, 3, ncol)) == mean(apply(test_array, 3, nrow)) )
})


testthat::test_that("the function translation returns the correct output in case of array and shift_rows are negative and shift_cols are positive", {
  
  res = translation(test_array, shift_rows = -10, shift_cols = 10)
  
  testthat::expect_true( is.array(res) && mean(apply(res, 3, nrow)) == mean(apply(test_array, 3, nrow)) && mean(apply(res, 3, ncol)) == mean(apply(test_array, 3, nrow)) )
})



