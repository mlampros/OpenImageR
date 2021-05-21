
object_mat = matrix(runif(10000), 100, 100)                                                                            # a matrix

object_array = array(runif(30000), dim = c(100, 100, 3))                                                               # an array

object_list = list(array(runif(30000), dim = c(100, 100, 3)), array(runif(30000), dim = c(100, 100, 3)),

                   array(runif(30000), dim = c(100, 100, 3)), array(runif(30000), dim = c(100, 100, 3)))               # a list of 3-dimensional arrays


context('NormalizeObject and MinMaxObject')



# NormalizeObject


testthat::test_that("it returns an error if the object is not a vector, data frame, matrix or array", {

  testthat::expect_error( NormalizeObject(object_list)  )
})


testthat::test_that("in case of a vector it returns the same class object in range of values between 0 and 1", {

  res = NormalizeObject(1:10)

  testthat::expect_true( min(res) == 0.0 && max(res) == 1.0 && is.vector(res) && length(res) == length(1:10)  )
})


testthat::test_that("in case of a data frame it returns a matrix in range of values between 0 and 1", {

  res = NormalizeObject(data.frame(object_mat))

  testthat::expect_true( min(res) == 0.0 && max(res) == 1.0 && is.matrix(res) && nrow(res) == nrow(object_mat) && ncol(res) == ncol(object_mat)  )
})


testthat::test_that("in case of a matrix it returns a matrix in range of values between 0 and 1", {

  res = NormalizeObject(object_mat)

  testthat::expect_true( min(res) == 0.0 && max(res) == 1.0 && is.matrix(res) && nrow(res) == nrow(object_mat) && ncol(res) == ncol(object_mat)  )
})



testthat::test_that("in case of an array it returns an array in range of values between 0 and 1", {

  res = NormalizeObject(object_array)

  testthat::expect_true( min(res) == 0.0 && max(res) == 1.0 && is.array(res) && nrow(res) == nrow(object_mat) && ncol(res) == ncol(object_mat)  && dim(res)[3] == 3)
})



# MinMaxObject


testthat::test_that("it returns an error if the object is not a vector, data frame, matrix or array", {

  testthat::expect_error( MinMaxObject(object_list)  )
})


testthat::test_that("in case of a vector it returns the same class object in range of values between 0 and 1", {

  res = MinMaxObject(1:10)

  testthat::expect_true( is.list(res) && length(res) == 2 && res$min == min(1:10) && res$max == max(1:10) )
})


testthat::test_that("in case of a data frame it returns a matrix in range of values between 0 and 1", {

  res = MinMaxObject(data.frame(object_mat))

  testthat::expect_true( is.list(res) && length(res) == 2 && res$min == min(object_mat) && res$max == max(object_mat) )
})


testthat::test_that("in case of a matrix it returns a matrix in range of values between 0 and 1", {

  res = MinMaxObject(object_mat)

  testthat::expect_true( is.list(res) && length(res) == 2 && res$min == min(object_mat) && res$max == max(object_mat) )
})


testthat::test_that("in case of an array it returns an array in range of values between 0 and 1", {

  res = MinMaxObject(object_array)

  testthat::expect_true( is.list(res) && length(res) == 2 && mean(unlist(lapply(res, length))) == dim(object_array)[3] && min(res$min) == min(apply(object_array, 3, min)) && max(res$max) == max(apply(object_array, 3, max)))
})
