
context("F3: rotateImage() is exact at multiples of 90 degrees")

testthat::test_that("rotating by multiples of 90 degrees in mode 'same' with 'nearest' is exact", {

  for (n in c(8, 20, 21)) {

    m = matrix(as.numeric(seq_len(n * n)), n, n)

    testthat::expect_equal(rotateImage(m, 180, 'nearest', 'same'), m[n:1, n:1])
    testthat::expect_equal(rotateImage(m, 90, 'nearest', 'same'), t(m)[n:1, ])
    testthat::expect_equal(rotateImage(m, 270, 'nearest', 'same'), t(m)[, n:1])
    testthat::expect_equal(rotateImage(m, 360, 'nearest', 'same'), m)
  }
})

testthat::test_that("rotating a non-square image by 180 degrees in mode 'same' with 'nearest' is exact", {

  m = matrix(as.numeric(1:(21 * 13)), 21, 13)

  testthat::expect_equal(rotateImage(m, 180, 'nearest', 'same'), m[21:1, 13:1])
})

testthat::test_that("a full-size image rotated by 180 degrees keeps no all-zero row or column", {

  m = matrix(as.numeric(1:400), 20, 20)
  r = rotateImage(m, 180, 'nearest', 'same')

  testthat::expect_equal(sum(rowSums(r) == 0), 0)
  testthat::expect_equal(sum(colSums(r) == 0), 0)
})

testthat::test_that("mode 'full' keeps every pixel at multiples of 90 degrees, also with an odd canvas difference", {

  for (n in c(8, 13, 20)) {                                        # canvas 11, 18, 28: differences 3, 5, 8

    m = matrix(as.numeric(seq_len(n * n)), n, n)

    for (a in c(90, 180, 270)) {

      testthat::expect_setequal(setdiff(as.vector(rotateImage(m, a, 'nearest', 'full')), 0), as.vector(m))
    }
  }
})

testthat::test_that("the centre of a centred disk stays in place with 'nearest'", {

  n = 101
  g = expand.grid(r = 1:n, c = 1:n)
  m = matrix(as.numeric((g$r - 51)^2 + (g$c - 51)^2 <= 20^2), n, n)

  for (a in c(30, 90, 180)) {

    x = rotateImage(m, a, 'nearest', 'same')

    testthat::expect_equal(sum(row(x) * x) / sum(x), 51, tolerance = 1e-6)
    testthat::expect_equal(sum(col(x) * x) / sum(x), 51, tolerance = 1e-6)
  }
})
