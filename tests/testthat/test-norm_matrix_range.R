
#---------------------------------------------------------------------------------
# data
#-----

set.seed(1)
mt = matrix(1:48, 8, 6)
MIN = -1
MAX = 1

MIN_ = 0
MAX_ = 255

#---------------------------------------------------------------------------------
# results using the interp function in numpy [ python ]
#---------------------------------------------------------------------------------

# np = reticulate::import("numpy")
# np$interp(mt, reticulate::tuple(min(mt), max(mt)), reticulate::tuple(MIN, MAX))

# using dput() and rounded to 8 digits

np_res = structure(c(-1, -0.95744681, -0.91489362, -0.87234043, -0.82978723, 
                     -0.78723404, -0.74468085, -0.70212766, -0.65957447, -0.61702128, 
                     -0.57446809, -0.53191489, -0.4893617, -0.44680851, -0.40425532, 
                     -0.36170213, -0.31914894, -0.27659574, -0.23404255, -0.19148936, 
                     -0.14893617, -0.10638298, -0.06382979, -0.0212766, 0.0212766, 
                     0.06382979, 0.10638298, 0.14893617, 0.19148936, 0.23404255, 0.27659574, 
                     0.31914894, 0.36170213, 0.40425532, 0.44680851, 0.4893617, 0.53191489, 
                     0.57446809, 0.61702128, 0.65957447, 0.70212766, 0.74468085, 0.78723404, 
                     0.82978723, 0.87234043, 0.91489362, 0.95744681, 1), .Dim = c(8L, 6L))

np_res_255 = structure(c(0, 5.42553191, 10.85106383, 16.27659574, 21.70212766, 
                         27.12765957, 32.55319149, 37.9787234, 43.40425532, 48.82978723, 
                         54.25531915, 59.68085106, 65.10638298, 70.53191489, 75.95744681, 
                         81.38297872, 86.80851064, 92.23404255, 97.65957447, 103.08510638, 
                         108.5106383, 113.93617021, 119.36170213, 124.78723404, 130.21276596, 
                         135.63829787, 141.06382979, 146.4893617, 151.91489362, 157.34042553, 
                         162.76595745, 168.19148936, 173.61702128, 179.04255319, 184.46808511, 
                         189.89361702, 195.31914894, 200.74468085, 206.17021277, 211.59574468, 
                         217.0212766, 222.44680851, 227.87234043, 233.29787234, 238.72340426, 
                         244.14893617, 249.57446809, 255), .Dim = c(8L, 6L))

#---------------------------------------------------------------------------------


context('Output of data normalized in specific range')


testthat::test_that("it returns an error if the data is not a matrix", {

  testthat::expect_error(norm_matrix_range(list(mt), min_value = MIN, max_value = MAX))
})


testthat::test_that("the 'norm_matrix_range' returns the correct output ( range = c(-1,1) )", {
  
  # rounded to 8 digits as in np$interp()
  res = round(norm_matrix_range(mt, min_value = MIN, max_value = MAX), 8)

  testthat::expect_true(identical(res, np_res))
})


testthat::test_that("the 'norm_matrix_range' returns the correct output ( range = c(0,255) )", {
  
  # rounded to 8 digits as in np$interp()
  res = round(norm_matrix_range(mt, min_value = MIN_, max_value = MAX_), 8)
  
  testthat::expect_true(identical(res, np_res_255))
})
