MATR = matrix(100, 10, 10)

DF = as.data.frame(MATR)

ARRAY = array(unlist(list(matrix(runif(1000), 100, 100), matrix(runif(1000), 100, 100), matrix(runif(1000), 100, 100))), dim = c(100, 100, 3))


context("Hashing")


# binary_to_hex function

testthat::test_that("the binary_to_hex function takes a binary matrix and returns a hash string", {
  
  image_array = matrix(sample(0:1, 64, replace = T), 8, 8)
  
  res = binary_to_hex(image_array)
  
  testthat::expect_true( class(res) == 'character' && nchar(res) > 0 )
})


# average_hash

testthat::test_that("the average_hash function returns an error if the image is not 2-dimensional", {

  image_array = list(image_array = matrix(runif(25), 5, 5))
  
  testthat::expect_error( average_hash(image_array, hash_size = 3, MODE = 'hash') )
})


testthat::test_that("the average_hash function returns an error if the hash_size is greater than the dimensions of the original image", {
  
  image_array = matrix(runif(25), 5, 5)
  
  testthat::expect_error( average_hash(image_array, hash_size = 6, MODE = 'hash') )
})


testthat::test_that("the average_hash function returns a hex string if MODE = 'hash'", {
  
  image_array = matrix(runif(100), 10, 10)
  
  res = average_hash(image_array, hash_size = 6, MODE = 'hash')
  
  testthat::expect_true( class(res) == 'character' && nchar(res) > 0 )
})


testthat::test_that("the average_hash function returns a binary 1-row matrix from the image if MODE = 'binary'", {
  
  image_array = matrix(runif(100), 10, 10)
  
  res = average_hash(image_array, hash_size = 6, MODE = 'binary')
  
  testthat::expect_true( is.matrix(res) && nrow(res) == 1 && sum(c(0,1) %in% unique(res)) == 2 )
})


testthat::test_that("the average_hash function returns a binary 1-row matrix from the image if MODE = 'binary' and resize = bilinear", {
  
  image_array = matrix(runif(100), 10, 10)
  
  res = average_hash(image_array, hash_size = 6, MODE = 'binary', resize = 'bilinear')
  
  testthat::expect_true( is.matrix(res) && nrow(res) == 1 && sum(c(0,1) %in% unique(res)) == 2 )
})


testthat::test_that("the average_hash function returns a binary 1-row matrix from the image if MODE = 'hash' and resize = bilinear", {
  
  image_array = matrix(runif(100), 10, 10)
  
  res = average_hash(image_array, hash_size = 6, MODE = 'hash', resize = 'bilinear')
  
  testthat::expect_true( is.character(res) && length(res) == 1 && res != "" )
})


testthat::test_that("the average_hash function returns a binary 1-row matrix from the image if MODE = 'binary' and resize = invalid method", {
  
  image_array = matrix(runif(100), 10, 10)

  testthat::expect_error( average_hash(image_array, hash_size = 6, MODE = 'binary', resize = 'invalid') )
})

# phash function


testthat::test_that("the phash function returns an error if the image is not 2-dimensional", {
  
  image_array = list(image_array = matrix(runif(100), 10, 10))
  
  testthat::expect_error( phash(image_array, hash_size = 2, highfreq_factor = 2, MODE = 'hash') )
})


testthat::test_that("the phash function returns an error if the hash_size * highfreq_factor is greater than the dimensions of the original image", {
  
  image_array = image_array = matrix(runif(100), 10, 10)
  
  testthat::expect_error( phash(image_array, hash_size = 3, highfreq_factor = 4, MODE = 'hash') )
})



testthat::test_that("the phash function returns a hex string if MODE = 'hash'", {
  
  image_array = matrix(runif(100), 10, 10)
  
  res = phash(image_array, hash_size = 3, highfreq_factor = 2, MODE = 'hash')
  
  testthat::expect_true( class(res) == 'character' && nchar(res) > 0 )
})


testthat::test_that("the phash function returns a binary 1-row matrix from the image if MODE = 'binary'", {
  
  image_array = matrix(runif(100), 10, 10)
  
  res = phash(image_array, hash_size = 3, highfreq_factor = 2, MODE = 'binary')
  
  testthat::expect_true( is.matrix(res) && nrow(res) == 1 && sum(c(0,1) %in% unique(res)) == 2 )
})


testthat::test_that("the phash function returns a binary 1-row matrix from the image if MODE = 'binary', resize = 'bilinear'", {
  
  image_array = matrix(runif(100), 10, 10)
  
  res = phash(image_array, hash_size = 3, highfreq_factor = 2, MODE = 'binary', resize = 'bilinear')
  
  testthat::expect_true( is.matrix(res) && nrow(res) == 1 && sum(c(0,1) %in% unique(res)) == 2 )
})


testthat::test_that("the phash function returns a binary 1-row matrix from the image if MODE = 'hash', resize = 'bilinear'", {
  
  image_array = matrix(runif(100), 10, 10)
  
  res = phash(image_array, hash_size = 3, highfreq_factor = 2, MODE = 'hash', resize = 'bilinear')
  
  testthat::expect_true( is.character(res) && length(res) == 1 && res != "" )
})


testthat::test_that("the phash function returns a binary 1-row matrix from the image if MODE = 'binary', resize = 'invalid method'", {
  
  image_array = matrix(runif(100), 10, 10)
  
  testthat::expect_error( phash(image_array, hash_size = 3, highfreq_factor = 2, MODE = 'binary', resize = 'invalid') )
})

# dhash function


testthat::test_that("the dhash function returns an error if the image is not 2-dimensional", {
  
  image_array = list(image_array = matrix(runif(25), 5, 5))
  
  testthat::expect_error( dhash(image_array, hash_size = 3, MODE = 'hash') )
})


testthat::test_that("the dhash function returns an error if the hash_size is greater than the dimensions of the original image", {
  
  image_array = matrix(runif(25), 5, 5)
  
  testthat::expect_error( dhash(image_array, hash_size = 5, MODE = 'hash') )
})


testthat::test_that("the dhash function returns a hex string if MODE = 'hash'", {
  
  image_array = matrix(runif(100), 10, 10)
  
  res = dhash(image_array, hash_size = 6, MODE = 'hash')
  
  testthat::expect_true( class(res) == 'character' && nchar(res) > 0 )
})


testthat::test_that("the dhash function returns a binary 1-row matrix from the image if MODE = 'binary'", {
  
  image_array = matrix(runif(100), 10, 10)
  
  res = dhash(image_array, hash_size = 6, MODE = 'binary')
  
  testthat::expect_true( is.matrix(res) && nrow(res) == 1 && sum(c(0,1) %in% unique(res)) == 2 )
})


testthat::test_that("the dhash function returns a binary 1-row matrix from the image if MODE = 'binary', resize = 'bilinear'", {
  
  image_array = matrix(runif(100), 10, 10)
  
  res = dhash(image_array, hash_size = 6, MODE = 'binary', resize = 'bilinear')
  
  testthat::expect_true( is.matrix(res) && nrow(res) == 1 && sum(c(0,1) %in% unique(res)) == 2 )
})


testthat::test_that("the dhash function returns a binary 1-row matrix from the image if MODE = 'hash', resize = 'bilinear'", {
  
  image_array = matrix(runif(100), 10, 10)
  
  res = dhash(image_array, hash_size = 6, MODE = 'hash', resize = 'bilinear')
  
  testthat::expect_true( is.character(res) && length(res) == 1 && res != "" )
})


testthat::test_that("the dhash function returns a binary 1-row matrix from the image if MODE = 'binary', resize = 'invalid method'", {
  
  image_array = matrix(runif(100), 10, 10)
  
  testthat::expect_error( dhash(image_array, hash_size = 6, MODE = 'binary', resize = 'invalid') )
})


#=====================
# hash_apply function
#=====================

#================
# Error Handling
#================


testthat::test_that("the hash_apply function returns an error if threads less than 1", {
  
  testthat::expect_error( hash_apply(ARRAY, rows = 10, columns = 10, hash_size = 4, highfreq_factor = 4, method = 'phash', mode = 'binary', threads = 0, resize = 'nearest') )
})


testthat::test_that("the hash_apply function returns an error if object is a matrix and columns or rows are NULL", {
  
  testthat::expect_error( hash_apply(MATR, rows = NULL, columns = NULL, hash_size = 4, highfreq_factor = 4, method = 'phash', mode = 'binary', threads = 1, resize = 'nearest') )
})


testthat::test_that("the hash_apply function returns an error if object is a matrix and hash size times highfreq_factor greater than dimensions of image", {
  
  testthat::expect_error( hash_apply(ARRAY[,,1], rows = 10, columns = 10, hash_size = 10, highfreq_factor = 11, method = 'phash', mode = 'binary', threads = 1, resize = 'nearest') )
})


testthat::test_that("the hash_apply function returns an error if object is a matrix and hash size times highfreq_factor greater than dimensions of image, if mode = hash", {
  
  testthat::expect_error( hash_apply(ARRAY[,,1], rows = 10, columns = 10, hash_size = 10, highfreq_factor = 11, method = 'phash', mode = 'hash', threads = 1, resize = 'nearest') )
})


testthat::test_that("the hash_apply function returns an error if object is an array and hash size times highfreq_factor greater than dimensions of image, mode = 'hash'", {
  
  testthat::expect_error( hash_apply(ARRAY, rows = 10, columns = 10, hash_size = 10, highfreq_factor = 11, method = 'phash', mode = 'hash', threads = 1, resize = 'nearest') )
})


testthat::test_that("the hash_apply function returns an error if object is an array and hash size times highfreq_factor greater than dimensions of image, mode = 'hash'", {
  
  testthat::expect_error( hash_apply(ARRAY[,,1], rows = 10, columns = 10, hash_size = 10, highfreq_factor = 11, method = 'phash', mode = 'hash', threads = 1, resize = 'nearest') )
})

testthat::test_that("the hash_apply function returns an error if the path is incorrect (it lacks a slash)", {
  
  path = paste0(getwd(), path.expand("/image_files"))
  
  testthat::expect_error( hash_apply(path, rows = NULL, columns = NULL, hash_size = 4, highfreq_factor = 4, method = 'phash', mode = 'binary', threads = 1, resize = 'nearest') )
})


testthat::test_that("the hash_apply function returns an error if the path is correct but the image type is invalid", {
  
  path = paste0(getwd(), path.expand("/image_files/invalid_image/"))
  
  testthat::expect_error(  hash_apply(path, rows = NULL, columns = NULL, hash_size = 4, highfreq_factor = 4, method = 'phash', mode = 'binary', threads = 1, resize = 'nearest') )
})


testthat::test_that("the hash_apply function returns an error if the input object is not one of the supported types", {
  
  testthat::expect_error( hash_apply(list(MATR), rows = NULL, columns = NULL, hash_size = 4, highfreq_factor = 4, method = 'phash', mode = 'binary', threads = 1, resize = 'nearest')  )
})


testthat::test_that("the hash_apply function returns an error if the method is invalid", {
  
  image_array = matrix(runif(7840), nrow = 10, ncol = 784)
  
  testthat::expect_error( hash_apply(image_array, rows = 28, columns = 28, hash_size = 8, highfreq_factor = 3, method = 'invalid', threads = 1) )
})


#=================
# test hash_apply
#=================


testthat::test_that("the hash_apply function returns a binary matrix with equal number of rows as the input matrix for method = 'phash'", {
  
  image_array = matrix(runif(7840), nrow = 10, ncol = 784)
  
  res = hash_apply(image_array, rows = 28, columns = 28, hash_size = 8, highfreq_factor = 3, method = 'phash', threads = 1)
  
  testthat::expect_true( is.matrix(res) && nrow(res) == nrow(image_array) && sum(apply(res, 2, function(x) all(unique(x) %in% c(0,1)))) == ncol(res) )
})


testthat::test_that("the hash_apply function returns a binary matrix with equal number of rows as the input matrix for method = 'average_hash'", {
  
  image_array = matrix(runif(7840), nrow = 10, ncol = 784)
  
  res = hash_apply(image_array, rows = 28, columns = 28, hash_size = 8, highfreq_factor = 3, method = 'average_hash', threads = 1)
  
  testthat::expect_true( is.matrix(res) && nrow(res) == nrow(image_array) && sum(apply(res, 2, function(x) all(unique(x) %in% c(0,1)))) == ncol(res) )
})


testthat::test_that("the hash_apply function returns a binary matrix with equal number of rows as the input matrix for method = 'dhash'", {
  
  image_array = matrix(runif(7840), nrow = 10, ncol = 784)
  
  res = hash_apply(image_array, rows = 28, columns = 28, hash_size = 8, highfreq_factor = 3, method = 'dhash', threads = 1)
  
  testthat::expect_true( is.matrix(res) && nrow(res) == nrow(image_array) && sum(apply(res, 2, function(x) all(unique(x) %in% c(0,1)))) == ncol(res) )
})


testthat::test_that("the hash_apply function returns a binary matrix with equal number of rows as the input matrix for method = 'dhash', resize = 'bilinear'", {
  
  image_array = matrix(runif(7840), nrow = 10, ncol = 784)
  
  res = hash_apply(image_array, rows = 28, columns = 28, hash_size = 8, highfreq_factor = 3, method = 'dhash', threads = 1, resize = 'bilinear')
  
  testthat::expect_true( is.matrix(res) && nrow(res) == nrow(image_array) && sum(apply(res, 2, function(x) all(unique(x) %in% c(0,1)))) == ncol(res) )
})


testthat::test_that("the hash_apply function returns an error if resize is an 'invalid' method", {
  
  image_array = matrix(runif(7840), nrow = 10, ncol = 784)

  testthat::expect_error( hash_apply(image_array, rows = 28, columns = 28, hash_size = 8, highfreq_factor = 3, method = 'dhash', threads = 1, resize = 'invalid') )
})


testthat::test_that("the hash_apply function returns a correct output when the input object is a path to a folder of images, if mode is binary", {
  
  path = paste0(getwd(), path.expand("/image_files/HOG_apply_folder/"))
  
  len = list.files(path)
  
  hashs = 3
  
  highfreqf = 3
  
  res = hash_apply(path, rows = 28, columns = 28, hash_size = hashs, highfreq_factor = highfreqf, method = 'phash', mode = 'binary', threads = 1, resize = 'nearest')
  
  testthat::expect_true( length(res) == 2 && length(len) == nrow(res$hash) && hashs * highfreqf == ncol(res$hash) && sum(len %in% res$files) == length(len) )
})


testthat::test_that("the hash_apply function returns an error if the mode is invalid", {
  
  path = paste0(getwd(), path.expand("/image_files/HOG_apply_folder/"))
  
  len = list.files(path)
  
  hashs = 3
  
  highfreqf = 3

  testthat::expect_error( hash_apply(path, rows = 28, columns = 28, hash_size = hashs, highfreq_factor = highfreqf, method = 'phash', mode = 'invalid', threads = 1, resize = 'nearest') )
})


testthat::test_that("the hash_apply function returns a correct output when the input object is a path to a folder of images, if mode is hash", {
  
  path = paste0(getwd(), path.expand("/image_files/HOG_apply_folder/"))
  
  len = list.files(path)
  
  hashs = 3
  
  highfreqf = 3
  
  res = hash_apply(path, rows = 28, columns = 28, hash_size = hashs, highfreq_factor = highfreqf, method = 'phash', mode = 'hash', threads = 1, resize = 'nearest')
  
  testthat::expect_true( length(res) == 2 && length(len) == length(res$hash) && sum(len %in% res$files) == length(len) )
})


testthat::test_that("the hash_apply function returns a correct output when the input object is a matrix, if mode is hash", {
  
  hashs = 3
  
  highfreqf = 3
  
  res = hash_apply(ARRAY[,,1], rows = 10, columns = 10, hash_size = hashs, highfreq_factor = highfreqf, method = 'phash', mode = 'hash', threads = 1, resize = 'nearest')
  
  testthat::expect_true( length(res) == nrow(ARRAY[,,1]) )
})


testthat::test_that("the hash_apply function returns a correct output when the input object is a matrix, if mode is binary", {
  
  hashs = 3
  
  highfreqf = 3
  
  res = hash_apply(ARRAY[,,1], rows = 10, columns = 10, hash_size = hashs, highfreq_factor = highfreqf, method = 'phash', mode = 'binary', threads = 1, resize = 'nearest')
  
  testthat::expect_true( nrow(res) == nrow(ARRAY[,,1]) && ncol(res) == hashs * highfreqf )
})


testthat::test_that("the hash_apply function returns a correct output when the input object is a matrix, if mode is hash", {
  
  hashs = 3
  
  highfreqf = 3
  
  res = hash_apply(ARRAY[,,1], rows = 10, columns = 10, hash_size = hashs, highfreq_factor = highfreqf, method = 'phash', mode = 'hash', threads = 1, resize = 'nearest')
  
  testthat::expect_true( length(res) == nrow(ARRAY[,,1]) && is.character(res) )
})


testthat::test_that("the hash_apply function returns a correct output when the input object is an array, if mode is binary, method = average_hash", {
  
  hashs = 3

  res = hash_apply(ARRAY, rows = 10, columns = 10, hash_size = hashs, highfreq_factor = 3, method = 'average_hash', mode = 'binary', threads = 1, resize = 'nearest')
  
  testthat::expect_true( nrow(res) == dim(ARRAY)[3] && ncol(res) == hashs * hashs )
})


testthat::test_that("the hash_apply function returns a correct output when the input object is an array, if mode is binary, method = dhash", {
  
  hashs = 3

  res = hash_apply(ARRAY, rows = 10, columns = 10, hash_size = hashs, highfreq_factor = 2, method = 'dhash', mode = 'binary', threads = 1, resize = 'nearest')
  
  testthat::expect_true( nrow(res) == dim(ARRAY)[3] && ncol(res) == hashs * hashs )
})


testthat::test_that("the hash_apply function returns a correct output when the input object is an array, if mode is hash, method = average_hash", {
  
  hashs = 3
  
  res = hash_apply(ARRAY, rows = 10, columns = 10, hash_size = hashs, highfreq_factor = 2, method = 'average_hash', mode = 'hash', threads = 1, resize = 'nearest')
  
  testthat::expect_true( is.character(res) && length(res) == dim(ARRAY)[3] )
})


testthat::test_that("the hash_apply function returns a correct output when the input object is an array, if mode is hash, method = dhash", {
  
  hashs = 3
  
  res = hash_apply(ARRAY, rows = 10, columns = 10, hash_size = hashs, highfreq_factor = 2, method = 'dhash', mode = 'hash', threads = 1, resize = 'nearest')
  
  testthat::expect_true( is.character(res) && length(res) == dim(ARRAY)[3] )
})


testthat::test_that("the hash_apply function returns a correct output when the input object is a matrix, if mode is hash, method = average_hash", {
  
  hashs = 3
  
  res = hash_apply(ARRAY[,,1], rows = 10, columns = 10, hash_size = hashs, highfreq_factor = 2, method = 'average_hash', mode = 'hash', threads = 1, resize = 'nearest')
  
  testthat::expect_true( is.character(res) && length(res) == dim(ARRAY)[1] )
})

testthat::test_that("the hash_apply function returns a correct output when the input object is an matrix, if mode is hash, method = dhash", {
  
  hashs = 3
  
  res = hash_apply(ARRAY[,,1], rows = 10, columns = 10, hash_size = hashs, highfreq_factor = 2, method = 'dhash', mode = 'hash', threads = 1, resize = 'nearest')
  
  testthat::expect_true( is.character(res) && length(res) == dim(ARRAY)[1] )
})

#===========================
# 'invariant_hash' function
#===========================


testthat::test_that("the invariant_hash function returns an error if any of the flip, rotate or crop are not of type boolean", {
  
  testthat::expect_error( invariant_hash(ARRAY, ARRAY, method = 'phash', mode = 'binary', hash_size = 8, highfreq_factor = 4, resize = 'nearest', flip = 'TRUE', rotate = T, crop = T) )
})

testthat::test_that("the invariant_hash function returns an error if the image or the new_image are not 2-dimensional (gray images)", {

  testthat::expect_error( invariant_hash(ARRAY, ARRAY, method = 'phash', mode = 'binary', hash_size = 8, highfreq_factor = 4, resize = 'nearest', flip = T, rotate = T, crop = T) )
})


testthat::test_that("the invariant_hash function returns an error if the mode is invalid", {
  
  testthat::expect_error( invariant_hash(ARRAY[,,1], ARRAY[,,1], method = 'phash', mode = 'invalid', hash_size = 8, highfreq_factor = 4, resize = 'nearest', flip = T, rotate = T, crop = T) )
})


testthat::test_that("the invariant_hash function returns an error in case of crop = TRUE and method = 'phash', when the image size * 0.8 < hash_size * highfreq_factor", {
  
  mtrx = matrix(runif(1000), 100, 100)

  testthat::expect_error( invariant_hash(mtrx, mtrx, method = 'phash', mode = 'hash', hash_size = 10, highfreq_factor = 8, resize = 'nearest', flip = F, rotate = F, crop = T) )
})



testthat::test_that("the invariant_hash function returns an error in case of crop = TRUE and method = 'average_hash', when the image size * 0.8 < hash_size ", {
  
  mtrx = matrix(runif(1000), 100, 100)
  
  testthat::expect_error( invariant_hash(mtrx, mtrx, method = 'average_hash', mode = 'binary', hash_size = 80, highfreq_factor = 8, resize = 'nearest', flip = F, rotate = F, crop = T) )
})



testthat::test_that("the invariant_hash function returns an error in case of crop = TRUE and method = 'dhash', when the (image size - 1) * 0.8 < hash_size ", {
  
  mtrx = matrix(runif(1000), 100, 100)
  
  testthat::expect_error( invariant_hash(mtrx, mtrx, method = 'dhash', mode = 'hash', hash_size = 79, highfreq_factor = 8, resize = 'nearest', flip = F, rotate = F, crop = T) )
})


testthat::test_that("the invariant_hash function returns an error in case that rotate = TRUE and and angle_bidirectional is not valid [ angle_bidirectional is a string ] ", {
  
  mtrx = matrix(runif(1000), 100, 100)
  
  testthat::expect_error(  invariant_hash(mtrx, mtrx, method = 'dhash', mode = 'hash', hash_size = 78, highfreq_factor = 8, resize = 'nearest', flip = F, rotate = T, angle_bidirectional = 'a') )
})


testthat::test_that("the invariant_hash function returns an error in case that rotate = TRUE and and angle_bidirectional is not valid [ angle_bidirectional is NULL ] ", {
  
  mtrx = matrix(runif(1000), 100, 100)
  
  testthat::expect_error(  invariant_hash(mtrx, mtrx, method = 'dhash', mode = 'hash', hash_size = 78, highfreq_factor = 8, resize = 'nearest', flip = F, rotate = T, angle_bidirectional = NULL) )
})


testthat::test_that("the invariant_hash function returns an error in case that rotate = TRUE and and angle_bidirectional is not valid [ angle_bidirectional is >= 360 ] ", {
  
  mtrx = matrix(runif(1000), 100, 100)
  
  testthat::expect_error(  invariant_hash(mtrx, mtrx, method = 'dhash', mode = 'hash', hash_size = 78, highfreq_factor = 8, resize = 'nearest', flip = F, rotate = T, angle_bidirectional = 360) )
})


testthat::test_that("the invariant_hash function returns an error in case that rotate = TRUE and and angle_bidirectional is not valid [ angle_bidirectional is <= 0 ] ", {
  
  mtrx = matrix(runif(1000), 100, 100)
  
  testthat::expect_error(  invariant_hash(mtrx, mtrx, method = 'dhash', mode = 'hash', hash_size = 78, highfreq_factor = 8, resize = 'nearest', flip = F, rotate = T, angle_bidirectional = 0) )
})


testthat::test_that("the invariant_hash function returns the correct output [here a vector of length 2 ] if mode = binary and any of the flip, rotate, crop is not FALSE ", {
  
  mtrx = matrix(runif(1000), 100, 100)
  
  mtrx1 = matrix(runif(1000), 100, 100)
  
  res = invariant_hash(mtrx, mtrx1, method = 'dhash', mode = 'binary', hash_size = 10, highfreq_factor = 5, resize = 'nearest', flip = F, rotate = F, crop = T)
  
  testthat::expect_true( length(res) == 2 && is.data.frame(res) )
})


testthat::test_that("the invariant_hash function returns the correct output [here a vector of length 2 ] if mode = hash and any of the flip, rotate, crop is not FALSE ", {
  
  mtrx = matrix(runif(1000), 100, 100)
  
  mtrx1 = matrix(runif(1000), 100, 100)
  
  res = invariant_hash(mtrx, mtrx1, method = 'dhash', mode = 'hash', hash_size = 10, highfreq_factor = 5, resize = 'nearest', flip = F, rotate = F, crop = T)
  
  testthat::expect_true( length(res) == 2 && is.data.frame(res) )
})


testthat::test_that("the invariant_hash function returns the correct output [here a single number ] if mode = binary and all (flip, rotate, crop) is FALSE ", {
  
  mtrx = matrix(runif(1000), 100, 100)
  
  mtrx1 = matrix(runif(1000), 100, 100)
  
  res = invariant_hash(mtrx, mtrx1, method = 'phash', mode = 'binary', hash_size = 10, highfreq_factor = 5, resize = 'nearest', flip = F, rotate = F, crop = F)
  
  testthat::expect_true( length(res) == 1 && is.numeric(res) )
})


testthat::test_that("the invariant_hash function returns the correct output [here a single number ] if mode = hash and all (flip, rotate, crop) is FALSE ", {
  
  mtrx = matrix(runif(1000), 100, 100)
  
  mtrx1 = matrix(runif(1000), 100, 100)
  
  res = invariant_hash(mtrx, mtrx1, method = 'phash', mode = 'hash', hash_size = 10, highfreq_factor = 5, resize = 'nearest', flip = F, rotate = F, crop = F)
  
  testthat::expect_true( length(res) == 1 && is.numeric(res) )
})


testthat::test_that("the invariant_hash function returns the correct output [here a single number ] if mode = binary and all (flip, rotate, crop) is FALSE ", {
  
  mtrx = matrix(runif(1000), 100, 100)
  
  mtrx1 = matrix(runif(1000), 100, 100)
  
  res = invariant_hash(mtrx, mtrx1, method = 'average_hash', mode = 'binary', hash_size = 10, highfreq_factor = 5, resize = 'nearest', flip = F, rotate = F, crop = F)
  
  testthat::expect_true( length(res) == 1 && is.numeric(res) )
})


testthat::test_that("the invariant_hash function returns the correct output [here a single number ] if mode = hash and all (flip, rotate, crop) is FALSE ", {
  
  mtrx = matrix(runif(1000), 100, 100)
  
  mtrx1 = matrix(runif(1000), 100, 100)
  
  res = invariant_hash(mtrx, mtrx1, method = 'average_hash', mode = 'hash', hash_size = 10, highfreq_factor = 5, resize = 'nearest', flip = F, rotate = F, crop = F)
  
  testthat::expect_true( length(res) == 1 && is.numeric(res) )
})


testthat::test_that("the invariant_hash function returns the correct output [here a single number ] if mode = hash and all (flip, rotate, crop) is FALSE ", {
  
  mtrx = matrix(runif(1000), 100, 100)
  
  mtrx1 = matrix(runif(1000), 100, 100)
  
  res = invariant_hash(mtrx, mtrx1, method = 'average_hash', mode = 'hash', hash_size = 10, highfreq_factor = 5, resize = 'nearest', flip = T, rotate = T, crop = T)
  
  testthat::expect_true( class(res) == 'data.frame' && sum(dim(res)) > 0 )
})


testthat::test_that("the invariant_hash function returns the correct output [here a single number ] if mode = hash and all (flip, rotate, crop) is FALSE ", {
  
  mtrx = matrix(runif(1000), 100, 100)
  
  mtrx1 = matrix(runif(1000), 100, 100)
  
  res = invariant_hash(mtrx, mtrx1, method = 'average_hash', mode = 'hash', hash_size = 10, highfreq_factor = 5, resize = 'nearest', flip = T, rotate = T, angle_bidirectional = 10, crop = T)
  
  testthat::expect_true( class(res) == 'data.frame' && sum(dim(res)) > 0 )
})


testthat::test_that("the invariant_hash function returns the correct output [here a single number ] if mode = hash and all (flip, rotate, crop) is FALSE ", {
  
  mtrx = matrix(1, 100, 100)
  
  mtrx1 = matrix(1, 100, 100)
  
  res = invariant_hash(mtrx, mtrx1, method = 'average_hash', mode = 'hash', hash_size = 10, highfreq_factor = 5, resize = 'nearest', flip = T, rotate = T, angle_bidirectional = 10, crop = T)
  
  testthat::expect_true( class(res) == 'data.frame' && sum(dim(res)) > 0 )
})


testthat::test_that("the invariant_hash function returns the correct output [here a single number ] if mode = hash and all (flip, rotate, crop) is FALSE ", {
  
  mtrx = matrix(1, 100, 100)
  
  mtrx1 = matrix(1, 100, 100)
  
  res = invariant_hash(mtrx, mtrx1, method = 'average_hash', mode = 'hash', hash_size = 10, highfreq_factor = 5, resize = 'nearest', flip = T, rotate = T, angle_bidirectional = 90, crop = T)
  
  testthat::expect_true( class(res) == 'data.frame' && sum(dim(res)) > 0 )
})