
# F12: edge_detection(method = 'LoG') must use the range_gauss it is given (it used a hard-coded 2)

testthat::test_that("edge_detection('LoG') honours range_gauss", {

  set.seed(1); x = matrix(runif(400), 20, 20)

  testthat::expect_false(isTRUE(all.equal(gaussian_kernel(5, 1, 2), gaussian_kernel(5, 1, 10))))

  e2 = edge_detection(x, method = 'LoG', conv_mode = 'same', range_gauss = 2)
  e10 = edge_detection(x, method = 'LoG', conv_mode = 'same', range_gauss = 10)

  testthat::expect_false(isTRUE(all.equal(e2, e10)))
  testthat::expect_equal(edge_detection(x, method = 'LoG', conv_mode = 'same'), e2)     # the default is range_gauss = 2
})
