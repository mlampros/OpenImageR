
# F55: rotateImage() and rotateFixed() turn in opposite directions; the documentation must say so (D16: documentation only)

testthat::test_that("rotateImage() turns counter-clockwise and rotateFixed() clockwise, as documented", {

  m = matrix(as.numeric(1:64), 8, 8)

  testthat::expect_identical(rotateImage(m, 90, 'nearest', 'same'), rotateFixed(m, 270))
  testthat::expect_identical(rotateImage(m, 270, 'nearest', 'same'), rotateFixed(m, 90))
  testthat::expect_identical(rotateFixed(matrix(as.numeric(1:6), 2, 3), 90)[1, ], c(2, 1))       # row 1 at the top: the top-left value moves to the top-right

  rd_dir = testthat::test_path("..", "..", "man")
  testthat::skip_if_not(dir.exists(rd_dir), "the man/ folder is only available in the source tree")

  rd_image = paste(readLines(file.path(rd_dir, "rotateImage.Rd")), collapse = " ")
  rd_fixed = paste(readLines(file.path(rd_dir, "rotateFixed.Rd")), collapse = " ")

  testthat::expect_match(rd_image, "counter-clockwise")
  testthat::expect_match(rd_fixed, "clockwise")
  testthat::expect_match(rd_image, "rotateFixed")
  testthat::expect_match(rd_fixed, "rotateImage\\(image, 270")
})
