
context("F4: padding() speed and layout")

pad_reference = function(x, new_rows, new_cols, fill_value) {

  dif_r = new_rows - nrow(x)
  dif_c = new_cols - ncol(x)
  top = dif_r %/% 2
  left = dif_c %/% 2

  out = matrix(fill_value, new_rows, new_cols)
  out[top + seq_len(nrow(x)), left + seq_len(ncol(x))] = x

  list(data = out, padded_start = top, padded_end = dif_r - top, padded_left = left, padded_right = dif_c - left)
}

testthat::test_that("padding() matches the reference layout for even, odd and zero differences", {

  set.seed(1)
  x = matrix(runif(35), 5, 7)

  for (shape in list(c(5, 7), c(6, 7), c(5, 8), c(8, 12), c(9, 11), c(10, 8))) {

    res = padding(x, shape[1], shape[2], fill_value = 2.5)

    testthat::expect_equal(res, pad_reference(x, shape[1], shape[2], 2.5))
  }
})

testthat::test_that("padding() of an array pads every slice the same way", {

  set.seed(2)
  a = array(runif(5 * 7 * 3), c(5, 7, 3))
  res = padding(a, 8, 12, fill_value = 1)

  for (i in 1:3) {

    testthat::expect_equal(res$data[, , i], pad_reference(a[, , i], 8, 12, 1)$data)
  }
})

testthat::test_that("padding() of a 1024 x 1024 matrix to 2048 x 2048 takes less than 5 seconds", {

  testthat::skip_on_cran()

  g = matrix(0.5, 1024, 1024)

  elapsed = system.time(res <- padding(g, 2048, 2048))[["elapsed"]]

  testthat::expect_lt(elapsed, 5)                # the row-by-row version took 21.8 s (FINDINGS F4)
  testthat::expect_equal(dim(res$data), c(2048L, 2048L))
})
