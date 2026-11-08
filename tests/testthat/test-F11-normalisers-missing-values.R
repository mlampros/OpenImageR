# F11 (reopened after REVIEW2_2026-11-08_1512.md): the flat-input guard of the normalisers must not fail on NA values

testthat::test_that("norm_matrix_range() and NormalizeObject() of an input with an NA return NA, as at 401bab4", {

  out = norm_matrix_range(matrix(c(1, NA, 3, 4), 2, 2))              # at a22c8da: "missing value where TRUE/FALSE needed"
  testthat::expect_equal(dim(out), c(2L, 2L))
  testthat::expect_true(all(is.na(out)))

  testthat::expect_true(all(is.na(NormalizeObject(c(1, NA, 3)))))
  testthat::expect_equal(length(NormalizeObject(c(1, NA, 3))), 3L)
})
