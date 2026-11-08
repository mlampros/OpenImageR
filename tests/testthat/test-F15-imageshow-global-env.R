
# F15: imageShow(<path>) must not assign 'file_path' in the global environment; the shiny app gets the path another way

testthat::test_that("imageShow(<path>) leaves the global environment alone and gives the app the path", {

  path = system.file("tmp_images", "1.png", package = "OpenImageR")

  had = exists("file_path", envir = globalenv(), inherits = FALSE)
  if (had) {
    old = get("file_path", envir = globalenv())
    on.exit(assign("file_path", old, envir = globalenv()), add = TRUE)
  }
  assign("file_path", "user value", envir = globalenv())
  on.exit(if (!had) rm("file_path", envir = globalenv()), add = TRUE)

  seen = NULL
  testthat::local_mocked_bindings(runApp = function(appDir, ...) {
    server = source(file.path(appDir, "server.R"), local = new.env())$value
    seen <<- environment(server)                                       # the server reads the path when the app runs
    seen$path_option <<- shiny::getShinyOption("OpenImageR_file_path")
    invisible(NULL)
  }, .package = "shiny")

  imageShow(path)

  testthat::expect_identical(get("file_path", envir = globalenv()), "user value")
  testthat::expect_identical(seen$path_option, normalizePath(path))
})
