
object_mat = matrix(runif(10000), 100, 100)                                                                            # a matrix

object_array = array(runif(10000), dim = c(100, 100, 3))                                                               # an array

object_array_mlt = array(runif(10000), dim = c(100, 100, 10))                                                          # an array (multiple matrices)

object_list = list(array(runif(10000), dim = c(100, 100, 3)), array(runif(10000), dim = c(100, 100, 3)), 
                   
                   array(runif(10000), dim = c(100, 100, 3)), array(runif(10000), dim = c(100, 100, 3)))               # a list of 3-dimensional arrays


context('Augmentation image')

#################
# error handling
#################

testthat::test_that("in case that the data is a vector it returns an error", {

  testthat::expect_error( Augmentation(1:10, resiz_width = 80, resiz_height = 80, rotate_angle = 40)  )
})


testthat::test_that("in case that crop_height, crop_width is not a sequence of values it returns an error", {
  
  testthat::expect_error( Augmentation(object_mat, crop_height = 10, crop_width = 10, resiz_width = 80, resiz_height = 80, rotate_angle = 40)  )
})


testthat::test_that("in case that resiz_height, resiz_width is not a single value it returns an error", {
  
  testthat::expect_error( Augmentation(object_mat, crop_height = 10:50, crop_width = 10:50, resiz_width = 5:10, resiz_height = 5:10, rotate_angle = 40)  )
})


testthat::test_that("in case that the resiz_method is not one of nearest, bilinear it returns an error", {
  
  testthat::expect_error( Augmentation(object_mat, crop_height = 10:50, crop_width = 10:50, resiz_width = 60, resiz_height = 60, resiz_method = 'invalid', rotate_angle = 40)  )
})


testthat::test_that("in case that the rotate_angle is not greater than 0 or less than 360 it returns an error", {
  
  testthat::expect_error( Augmentation(object_mat, crop_height = 10:50, crop_width = 10:50, resiz_width = 60, resiz_height = 60, resiz_method = 'nearest', rotate_angle = -1)  )
})


testthat::test_that("in case that the rotate_method is not one of nearest, bilinear it returns an error", {
  
  testthat::expect_error( Augmentation(object_mat, crop_height = 10:50, crop_width = 10:50, resiz_width = 60, resiz_height = 60, resiz_method = 'nearest', rotate_angle = 40, 
                                       
                                       rotate_method = 'invalid')  )
})


testthat::test_that("in case that the zca_comps is greater than the number of columns + 1 it returns an error", {
  
  testthat::expect_error( Augmentation(object_mat, crop_height = 10:50, crop_width = 10:50, resiz_width = 40, resiz_height = 40, resiz_method = 'nearest', rotate_angle = 40, 
                                       
                                       rotate_method = 'nearest', zca_comps = ncol(object_mat) + 1)  )
})


testthat::test_that("in case that the padded_value is not a numeric value it returns an error (if the input data is a matrix)", {
  
  testthat::expect_error( Augmentation(object_mat, crop_height = 10:50, crop_width = 10:50, resiz_width = 40, resiz_height = 40, resiz_method = 'nearest', rotate_angle = 40, 
                                       
                                       rotate_method = 'nearest', padded_value = c(0,1))  )
})


testthat::test_that("in case that the threads is less than 1 and input data is an array, it returns an error", {
  
  testthat::expect_error( Augmentation(object_array, crop_height = 10:50, crop_width = 10:50, resiz_width = 60, resiz_height = 60, resiz_method = 'nearest', rotate_angle = 40, 
                                       
                                       rotate_method = 'nearest', zca_comps = 10, zca_epsilon = 0.1, image_thresh = 0.5, threads = 0)  )
})


testthat::test_that("in case that the length of crop_height is greater than the columns of the image it returns an error", {
  
  testthat::expect_error( Augmentation(object_array, crop_height = 1:110, crop_width = 1:90, resiz_width = 20, resiz_height = 20, resiz_method = 'nearest', rotate_angle = 40, 
                                       
                                       rotate_method = 'nearest', zca_comps = 5, zca_epsilon = 0.1, image_thresh = 0.5, threads = 1)  )
})


testthat::test_that("in case that the length of crop_width is greater than the rows of the image it returns an error", {
  
  testthat::expect_error( Augmentation(object_array, crop_height = 1:90, crop_width = 1:110, resiz_width = 20, resiz_height = 20, resiz_method = 'nearest', rotate_angle = 40, 
                                       
                                       rotate_method = 'nearest', zca_comps = 5, zca_epsilon = 0.1, image_thresh = 0.5, threads = 1)  )
})



testthat::test_that("in case that the input list consists of matrices it returns an error", {
  
  tmp_lst = list(matrix(0, 10, 10), matrix(0, 10, 10))
  
  testthat::expect_error( Augmentation(tmp_lst, crop_height = 1:30, crop_width = 1:30, resiz_width = 20, resiz_height = 20, resiz_method = 'nearest', rotate_angle = 40, 
                                       
                                       rotate_method = 'nearest', zca_comps = 5, zca_epsilon = 0.1, image_thresh = 0.5, threads = 1)  )
})


testthat::test_that("in case that the crop_width in the input list is greater than the rows of the image it returns an error", {
  
  testthat::expect_error( Augmentation(object_list, crop_height = 1:90, crop_width = 1:110, resiz_width = 20, resiz_height = 20, resiz_method = 'nearest', rotate_angle = 40, 
                                       
                                       rotate_method = 'nearest', zca_comps = 5, zca_epsilon = 0.1, image_thresh = 0.5, threads = 1)  )
})


testthat::test_that("in case that the crop_height in the input list is greater than the columns of the image it returns an error", {
  
  testthat::expect_error( Augmentation(object_list, crop_height = 1:110, crop_width = 1:90, resiz_width = 20, resiz_height = 20, resiz_method = 'nearest', rotate_angle = 40, 
                                       
                                       rotate_method = 'nearest', zca_comps = 5, zca_epsilon = 0.1, image_thresh = 0.5, threads = 1)  )
})


testthat::test_that("in case that the flip_mode is incorrect it returns an error", {
  
  testthat::expect_error( Augmentation(object_list, flip_mode = 'invalid', crop_height = 1:80, crop_width = 1:90, resiz_width = 20, resiz_height = 20, resiz_method = 'nearest', rotate_angle = 40, 
                                       
                                       rotate_method = 'nearest', zca_comps = 5, zca_epsilon = 0.1, image_thresh = 0.5, threads = 1)  )
})


testthat::test_that("the function Augmentation returns an error if the padded_value parameter is not a numeric vector of length equal to the dimensions of the array (3-dimensional array)", {
  
  testthat::expect_error( Augmentation(object_array, crop_height = 1:80, crop_width = 1:90, resiz_width = 20, resiz_height = 20, resiz_method = 'nearest', rotate_angle = 40, 
                                       
                                       rotate_method = 'nearest', padded_value = c(0,1)) )
})


testthat::test_that("the function Augmentation returns an error if the padded_value parameter is not a numeric vector of length equal to the dimensions of the array (10-dimensional array -- array of matrices)", {
  
  testthat::expect_error( Augmentation(object_array_mlt, crop_height = 1:80, crop_width = 1:90, resiz_width = 20, resiz_height = 20, resiz_method = 'nearest', rotate_angle = 40, 
                                       
                                       rotate_method = 'nearest', padded_value = c(0,1)) )
})


testthat::test_that("the function Augmentation returns an error if the padded_value parameter is not a numeric vector of length equal to 3 (in case that the input is a list)", {
  
  testthat::expect_error( Augmentation(object_list, crop_height = 1:80, crop_width = 1:90, resiz_width = 20, resiz_height = 20, resiz_method = 'nearest', rotate_angle = 40, 
                                       
                                       rotate_method = 'nearest', padded_value = c(0,1)) )
})


########################
# Augmentation function
########################


testthat::test_that("in case of a matrix with different parameters each time it returns the correct output [no parameter setting for matrix ] ", {
  
  res = Augmentation(object_mat)
  
  testthat::expect_true( class(object_mat) == class(res) && nrow(object_mat) == nrow(res) && ncol(object_mat) == ncol(res) )
})


testthat::test_that("in case of an array with different parameters each time it returns the correct output [no parameter setting for array ] ", {
  
  res = Augmentation(object_array)
  
  testthat::expect_true( class(object_array) == class(res) && nrow(object_array) == nrow(res) && ncol(object_array) == ncol(res) && dim(res)[3] == dim(object_array)[3] )
})


testthat::test_that("in case of a list with different parameters each time it returns the correct output [no parameter setting for list ] ", {
  
  res = Augmentation(object_list)
  
  testthat::expect_true( class(object_list) == class(res) && mean(unlist(lapply(object_list, nrow))) == mean(unlist(lapply(res, nrow))) && 
                           
                           mean(unlist(lapply(object_list, ncol))) == mean(unlist(lapply(res, ncol))) && length(res) == length(object_list) )
})


# matrix


testthat::test_that("in case of a matrix with different parameters each time it returns the correct output", {
  
  res = Augmentation(object_mat, crop_height = 1:50, crop_width = 1:30)
  
  testthat::expect_true( class(object_mat) == class(res) && nrow(res) == 30 && ncol(res) == 50 )
})


testthat::test_that("in case of a matrix with different parameters each time it returns the correct output", {
  
  res = Augmentation(object_mat, crop_height = 1:10, crop_width = 1:30)
  
  testthat::expect_true( class(object_mat) == class(res) && nrow(res) == 30 && ncol(res) == 10 )
})


testthat::test_that("in case of a matrix with different parameters each time it returns the correct output", {
  
  res = Augmentation(object_mat, crop_height = NULL, crop_width = 1:30)
  
  testthat::expect_true( class(object_mat) == class(res) && nrow(res) == 30 && ncol(res) == ncol(object_mat) )
})


testthat::test_that("in case of a matrix with different parameters each time it returns the correct output", {
  
  res = Augmentation(object_mat, crop_height = 1:20, crop_width = NULL)
  
  testthat::expect_true( class(object_mat) == class(res) && nrow(res) == nrow(object_mat) && ncol(res) == 20 )
})


testthat::test_that("in case of a matrix with different parameters each time it returns the correct output", {
  
  res = Augmentation(object_mat, crop_height = 1:50, crop_width = 1:50, resiz_width = 20, resiz_height = 20)
  
  testthat::expect_true( class(object_mat) == class(res) && nrow(res) == 20 && ncol(res) == 20 )
})



testthat::test_that("in case of a matrix with different parameters each time it returns the correct output", {
  
  res = Augmentation(object_mat, crop_height = 1:50, crop_width = 1:50, resiz_width = 0, resiz_height = 20)
  
  testthat::expect_true( class(object_mat) == class(res) && nrow(res) == 50 && ncol(res) == 20 )
})


testthat::test_that("in case of a matrix with different parameters each time it returns the correct output", {
  
  res = Augmentation(object_mat, crop_height = 1:50, crop_width = 1:50, resiz_width = 20, resiz_height = 0)
  
  testthat::expect_true( class(object_mat) == class(res) && nrow(res) == 20 && ncol(res) == 50 )
})


testthat::test_that("in case of a matrix with different parameters each time it returns the correct output", {
  
  res = Augmentation(object_mat, crop_height = 1:50, crop_width = 1:50, resiz_width = 20, resiz_height = 0, resiz_method = 'bilinear')
  
  testthat::expect_true( class(object_mat) == class(res) && nrow(res) == 20 && ncol(res) == 50 )
})


testthat::test_that("in case of a matrix with different parameters each time it returns the correct output", {
  
  res = Augmentation(object_mat, crop_height = 1:30, crop_width = 1:30, resiz_width = 0, resiz_height = 50, resiz_method = 'bilinear')
  
  testthat::expect_true( class(object_mat) == class(res) && nrow(res) == 30 && ncol(res) == 50 )
})


testthat::test_that("in case of a matrix with different parameters each time it returns the correct output", {
  
  res = Augmentation(object_mat, crop_height = 1:30, crop_width = 1:30, resiz_width = 0, resiz_height = 50, resiz_method = 'bilinear', rotate_angle = 40, rotate_method = 'nearest')
  
  testthat::expect_true( class(object_mat) == class(res) && nrow(res) == 30 && ncol(res) == 50 )
})


testthat::test_that("in case of a matrix with different parameters each time it returns the correct output", {
  
  res = Augmentation(object_mat, crop_height = 1:30, crop_width = 1:30, resiz_width = 0, resiz_height = 50, resiz_method = 'bilinear', rotate_angle = 40, rotate_method = 'bilinear')
  
  testthat::expect_true( class(object_mat) == class(res) && nrow(res) == 30 && ncol(res) == 50 )
})


testthat::test_that("in case of a matrix with different parameters each time it returns the correct output", {
  
  res = Augmentation(object_mat, crop_height = 1:30, crop_width = 1:30, resiz_width = 50, resiz_height = 50, resiz_method = 'nearest', rotate_angle = 40, rotate_method = 'bilinear',
                     
                     zca_comps = 5, zca_epsilon = 0.1)
  
  testthat::expect_true( class(object_mat) == class(res) && nrow(res) == 50 && ncol(res) == 50 )
})


testthat::test_that("in case of a matrix with different parameters each time it returns the correct output", {
  
  res = Augmentation(object_mat, crop_height = 1:30, crop_width = 1:30, resiz_width = 50, resiz_height = 50, resiz_method = 'bilinear', rotate_angle = 40, rotate_method = 'bilinear',
                     
                     zca_comps = 5, zca_epsilon = 0.1, image_thresh = 0.5)
  
  testthat::expect_true( class(object_mat) == class(res) && nrow(res) == 50 && ncol(res) == 50 )
})


testthat::test_that("in case of a matrix with different parameters each time it returns the correct output", {
  
  res = Augmentation(object_mat, flip_mode = 'horizontal', crop_height = 1:30, crop_width = 1:30, resiz_width = 50, resiz_height = 50, resiz_method = 'bilinear', rotate_angle = 40,
                     
                     rotate_method = 'bilinear', zca_comps = 5, zca_epsilon = 0.1, image_thresh = 0.5)
  
  testthat::expect_true( class(object_mat) == class(res) && nrow(res) == 50 && ncol(res) == 50 )
})


testthat::test_that("in case of a matrix with different parameters each time it returns the correct output", {
  
  res = Augmentation(object_mat, flip_mode = 'vertical', crop_height = 1:30, crop_width = 1:30, resiz_width = 50, resiz_height = 50, resiz_method = 'bilinear', rotate_angle = 40,
                     
                     rotate_method = 'bilinear', zca_comps = 5, zca_epsilon = 0.1, image_thresh = 0.5)
  
  testthat::expect_true( class(object_mat) == class(res) && nrow(res) == 50 && ncol(res) == 50 )
})



# Array


testthat::test_that("in case of a array with different parameters each time it returns the correct output", {
  
  res = Augmentation(object_array, crop_height = 1:50, crop_width = 1:30)
  
  testthat::expect_true( class(object_array) == class(res) && nrow(res) == 30 && ncol(res) == 50 && dim(res)[3] == dim(object_array)[3])
})


testthat::test_that("in case of a array with different parameters each time it returns the correct output", {
  
  res = Augmentation(object_array, crop_height = 1:10, crop_width = 1:30)
  
  testthat::expect_true( class(object_array) == class(res) && nrow(res) == 30 && ncol(res) == 10 && dim(res)[3] == dim(object_array)[3])
})


testthat::test_that("in case of a array with different parameters each time it returns the correct output", {
  
  res = Augmentation(object_array, crop_height = 1:30, crop_width = NULL, resiz_method = 'nearest')
  
  testthat::expect_true( class(object_array) == class(res) && nrow(res) == nrow(object_array) && ncol(res) == 30 && dim(res)[3] == dim(object_array)[3])
})


testthat::test_that("in case of a array with different parameters each time it returns the correct output", {
  
  res = Augmentation(object_array, crop_height = 1:50, crop_width = 1:50, resiz_width = 20, resiz_height = 20)
  
  testthat::expect_true( class(object_array) == class(res) && nrow(res) == 20 && ncol(res) == 20 && dim(res)[3] == dim(object_array)[3])
})



testthat::test_that("in case of a array with different parameters each time it returns the correct output", {
  
  res = Augmentation(object_array, crop_height = 1:50, crop_width = 1:50, resiz_width = 0, resiz_height = 20)
  
  testthat::expect_true( class(object_array) == class(res) && nrow(res) == 50 && ncol(res) == 20 && dim(res)[3] == dim(object_array)[3])
})


testthat::test_that("in case of a array with different parameters each time it returns the correct output", {
  
  res = Augmentation(object_array, crop_height = 1:50, crop_width = 1:50, resiz_width = 20, resiz_height = 0)
  
  testthat::expect_true( class(object_array) == class(res) && nrow(res) == 20 && ncol(res) == 50 && dim(res)[3] == dim(object_array)[3])
})


testthat::test_that("in case of a array with different parameters each time it returns the correct output", {
  
  res = Augmentation(object_array, crop_height = 1:50, crop_width = 1:50, resiz_width = 20, resiz_height = 0, resiz_method = 'bilinear')
  
  testthat::expect_true( class(object_array) == class(res) && nrow(res) == 20 && ncol(res) == 50 && dim(res)[3] == dim(object_array)[3])
})


testthat::test_that("in case of a array with different parameters each time it returns the correct output", {
  
  res = Augmentation(object_array, crop_height = 1:30, crop_width = 1:30, resiz_width = 0, resiz_height = 50, resiz_method = 'bilinear')
  
  testthat::expect_true( class(object_array) == class(res) && nrow(res) == 30 && ncol(res) == 50 && dim(res)[3] == dim(object_array)[3])
})


testthat::test_that("in case of a array with different parameters each time it returns the correct output", {
  
  res = Augmentation(object_array, crop_height = 1:30, crop_width = 1:30, resiz_width = 0, resiz_height = 50, resiz_method = 'bilinear', rotate_angle = 40, rotate_method = 'nearest')
  
  testthat::expect_true( class(object_array) == class(res) && nrow(res) == 30 && ncol(res) == 50 && dim(res)[3] == dim(object_array)[3])
})


testthat::test_that("in case of a array with different parameters each time it returns the correct output", {
  
  res = Augmentation(object_array, crop_height = 1:30, crop_width = 1:30, resiz_width = 0, resiz_height = 50, resiz_method = 'bilinear', rotate_angle = 40, rotate_method = 'bilinear')
  
  testthat::expect_true( class(object_array) == class(res) && nrow(res) == 30 && ncol(res) == 50 && dim(res)[3] == dim(object_array)[3])
})


testthat::test_that("in case of a array with different parameters each time it returns the correct output", {
  
  res = Augmentation(object_array, crop_height = 1:30, crop_width = 1:30, resiz_width = 50, resiz_height = 50, resiz_method = 'nearest', rotate_angle = 40, rotate_method = 'bilinear',
                     
                     zca_comps = 5, zca_epsilon = 0.1)
  
  testthat::expect_true( class(object_array) == class(res) && nrow(res) == 50 && ncol(res) == 50 && dim(res)[3] == dim(object_array)[3])
})


testthat::test_that("in case of a array with different parameters each time it returns the correct output", {
  
  res = Augmentation(object_array, crop_height = 1:30, crop_width = 1:30, resiz_width = 50, resiz_height = 50, resiz_method = 'bilinear', rotate_angle = 40, rotate_method = 'bilinear',
                     
                     zca_comps = 5, zca_epsilon = 0.1, image_thresh = 0.5)
  
  testthat::expect_true( class(object_array) == class(res) && nrow(res) == 50 && ncol(res) == 50 && dim(res)[3] == dim(object_array)[3])
})


testthat::test_that("in case of a array with different parameters each time it returns the correct output", {
  
  res = Augmentation(object_array, flip_mode = 'horizontal', crop_height = 1:30, crop_width = 1:30, resiz_width = 50, resiz_height = 50, resiz_method = 'bilinear', rotate_angle = 40, rotate_method = 'bilinear',
                     
                     zca_comps = 5, zca_epsilon = 0.1, image_thresh = 0.5)
  
  testthat::expect_true( class(object_array) == class(res) && nrow(res) == 50 && ncol(res) == 50 && dim(res)[3] == dim(object_array)[3])
})


testthat::test_that("in case of a array with different parameters each time it returns the correct output", {
  
  res = Augmentation(object_array, flip_mode = 'vertical', crop_height = 1:30, crop_width = 1:30, resiz_width = 50, resiz_height = 50, resiz_method = 'bilinear', rotate_angle = 40, rotate_method = 'bilinear',
                     
                     zca_comps = 5, zca_epsilon = 0.1, image_thresh = 0.5)
  
  testthat::expect_true( class(object_array) == class(res) && nrow(res) == 50 && ncol(res) == 50 && dim(res)[3] == dim(object_array)[3])
})


# List of arrays


testthat::test_that("in case of a List with different parameters each time it returns the correct output", {
  
  res = Augmentation(object_list, crop_height = 1:50, crop_width = 1:30)
  
  testthat::expect_true( class(object_list) == class(res) && mean(unlist(lapply(res, nrow))) == 30 && mean(unlist(lapply(res, ncol))) == 50 && length(res) == length(object_list))
})


testthat::test_that("in case of a List with different parameters each time it returns the correct output", {
  
  res = Augmentation(object_list, crop_height = 1:10, crop_width = 1:30)
  
  testthat::expect_true( class(object_list) == class(res) && mean(unlist(lapply(res, nrow))) == 30 && mean(unlist(lapply(res, ncol))) == 10 && length(res) == length(object_list))
})


testthat::test_that("in case of a List with different parameters each time it returns the correct output", {
  
  res = Augmentation(object_list, crop_height = 1:30, crop_width = NULL, resiz_method = 'nearest')
  
  testthat::expect_true( class(object_list) == class(res) && mean(unlist(lapply(res, nrow))) == mean(unlist(lapply(object_list, nrow))) && mean(unlist(lapply(res, ncol))) == 30 && length(res) == length(object_list))
})


testthat::test_that("in case of a List with different parameters each time it returns the correct output", {
  
  res = Augmentation(object_list, crop_height = 1:50, crop_width = 1:50, resiz_width = 20, resiz_height = 20)
  
  testthat::expect_true( class(object_list) == class(res) && mean(unlist(lapply(res, nrow))) == 20 && mean(unlist(lapply(res, ncol))) == 20 && length(res) == length(object_list))
})



testthat::test_that("in case of a List with different parameters each time it returns the correct output", {
  
  res = Augmentation(object_list, crop_height = 1:50, crop_width = 1:50, resiz_width = 0, resiz_height = 20)
  
  testthat::expect_true( class(object_list) == class(res) && mean(unlist(lapply(res, nrow))) == 50 && mean(unlist(lapply(res, ncol))) == 20 && length(res) == length(object_list))
})


testthat::test_that("in case of a List with different parameters each time it returns the correct output", {
  
  res = Augmentation(object_list, crop_height = 1:50, crop_width = 1:50, resiz_width = 20, resiz_height = 0)
  
  testthat::expect_true( class(object_list) == class(res) && mean(unlist(lapply(res, nrow))) == 20 && mean(unlist(lapply(res, ncol))) == 50 && length(res) == length(object_list))
})


testthat::test_that("in case of a List with different parameters each time it returns the correct output", {
  
  res = Augmentation(object_list, crop_height = 1:50, crop_width = 1:50, resiz_width = 20, resiz_height = 0, resiz_method = 'bilinear')
  
  testthat::expect_true( class(object_list) == class(res) && mean(unlist(lapply(res, nrow))) == 20 && mean(unlist(lapply(res, ncol))) == 50 && length(res) == length(object_list))
})


testthat::test_that("in case of a List with different parameters each time it returns the correct output", {
  
  res = Augmentation(object_list, crop_height = 1:30, crop_width = 1:30, resiz_width = 0, resiz_height = 50, resiz_method = 'bilinear')
  
  testthat::expect_true( class(object_list) == class(res) && mean(unlist(lapply(res, nrow))) == 30 && mean(unlist(lapply(res, ncol))) == 50 && length(res) == length(object_list))
})


testthat::test_that("in case of a List with different parameters each time it returns the correct output", {
  
  res = Augmentation(object_list, crop_height = 1:30, crop_width = 1:30, resiz_width = 0, resiz_height = 50, resiz_method = 'bilinear', rotate_angle = 40, rotate_method = 'nearest')
  
  testthat::expect_true( class(object_list) == class(res) && mean(unlist(lapply(res, nrow))) == 30 && mean(unlist(lapply(res, ncol))) == 50 && length(res) == length(object_list))
})


testthat::test_that("in case of a List with different parameters each time it returns the correct output", {
  
  res = Augmentation(object_list, crop_height = 1:30, crop_width = 1:30, resiz_width = 0, resiz_height = 50, resiz_method = 'bilinear', rotate_angle = 40, rotate_method = 'bilinear', verbose = T)
  
  testthat::expect_true( class(object_list) == class(res) && mean(unlist(lapply(res, nrow))) == 30 && mean(unlist(lapply(res, ncol))) == 50 && length(res) == length(object_list))
})


testthat::test_that("in case of a List with different parameters each time it returns the correct output", {
  
  res = Augmentation(object_list, crop_height = 1:30, crop_width = 1:30, resiz_width = 50, resiz_height = 50, resiz_method = 'nearest', rotate_angle = 40, rotate_method = 'bilinear',
                     
                     zca_comps = 5, zca_epsilon = 0.1)
  
  testthat::expect_true( class(object_list) == class(res) && mean(unlist(lapply(res, nrow))) == 50 && mean(unlist(lapply(res, ncol))) == 50 && length(res) == length(object_list))
})


testthat::test_that("in case of a List with different parameters each time it returns the correct output", {
  
  res = Augmentation(object_list, crop_height = 1:30, crop_width = 1:30, resiz_width = 50, resiz_height = 50, resiz_method = 'bilinear', rotate_angle = 40, rotate_method = 'bilinear',
                     
                     zca_comps = 5, zca_epsilon = 0.1, image_thresh = 0.5, verbose = T)
  
  testthat::expect_true( class(object_list) == class(res) && mean(unlist(lapply(res, nrow))) == 50 && mean(unlist(lapply(res, ncol))) == 50 && length(res) == length(object_list))
})


testthat::test_that("in case of a List with different parameters each time it returns the correct output", {
  
  res = Augmentation(object_list, flip_mode = 'horizontal', crop_height = 1:30, crop_width = 1:30, resiz_width = 50, resiz_height = 50, resiz_method = 'bilinear', rotate_angle = 40, rotate_method = 'bilinear',
                     
                     zca_comps = 5, zca_epsilon = 0.1, image_thresh = 0.5, verbose = T)
  
  testthat::expect_true( class(object_list) == class(res) && mean(unlist(lapply(res, nrow))) == 50 && mean(unlist(lapply(res, ncol))) == 50 && length(res) == length(object_list))
})


testthat::test_that("in case of a List with different parameters each time it returns the correct output", {
  
  res = Augmentation(object_list, flip_mode = 'vertical', crop_height = 1:30, crop_width = 1:30, resiz_width = 50, resiz_height = 50, resiz_method = 'bilinear', rotate_angle = 40, rotate_method = 'bilinear',
                     
                     zca_comps = 5, zca_epsilon = 0.1, image_thresh = 0.5, verbose = T)
  
  testthat::expect_true( class(object_list) == class(res) && mean(unlist(lapply(res, nrow))) == 50 && mean(unlist(lapply(res, ncol))) == 50 && length(res) == length(object_list))
})



# additional tests for shift_rows, shift_cols [ separately ]


testthat::test_that("in case of a matrix , change the values for both shift_rows, shift_cols", {
  
  res = Augmentation(object_mat, flip_mode = 'vertical', crop_height = 1:30, crop_width = 1:30, resiz_width = 50, resiz_height = 50, resiz_method = 'bilinear', shift_rows = 10, 
                     
                     shift_cols = -10, rotate_angle = 40, rotate_method = 'bilinear', zca_comps = 5, zca_epsilon = 0.1, image_thresh = 0.5)
  
  testthat::expect_true( class(object_mat) == class(res) && nrow(res) == 50 && ncol(res) == 50 )
})


testthat::test_that("in case of an array , change the values for both shift_rows, shift_cols", {
  
  res = Augmentation(object_array, flip_mode = 'vertical', crop_height = 1:30, crop_width = 1:30, resiz_width = 50, resiz_height = 50, resiz_method = 'bilinear', shift_rows = 10, 
                     
                     shift_cols = -10, rotate_angle = 40, rotate_method = 'bilinear', zca_comps = 5, zca_epsilon = 0.1, image_thresh = 0.5)
  
  testthat::expect_true( class(object_array) == class(res) && nrow(res) == 50 && ncol(res) == 50 && dim(res)[3] == dim(object_array)[3])
})


testthat::test_that("in case of a List , change the values for both shift_rows, shift_cols", {
  
  res = Augmentation(object_list, flip_mode = 'vertical', crop_height = 1:30, crop_width = 1:30, resiz_width = 50, resiz_height = 50, resiz_method = 'bilinear', shift_rows = 10, 
                     
                     shift_cols = 0, rotate_angle = 40, rotate_method = 'bilinear', zca_comps = 5, zca_epsilon = 0.1, image_thresh = 0.5, verbose = T)
  
  testthat::expect_true( class(object_list) == class(res) && mean(unlist(lapply(res, nrow))) == 50 && mean(unlist(lapply(res, ncol))) == 50 && length(res) == length(object_list))
})