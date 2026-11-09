# display an image

This function displays an image

## Usage

``` r
imageShow(file_path, clear_viewer = FALSE)
```

## Arguments

- file_path:

  if file_path is a character string, then a shiny application is
  utilized. If file_path is a matrix, data.frame OR a 3-dimensional
  array (where the third dimension is equal to 3) then the grid.raster
  function of the base grid package is used.

- clear_viewer:

  a boolean. If TRUE then the previous image will be removed in the
  viewer before displaying the next one

## Value

displays an image

## Details

This function displays an image using either a character path, a 2- or a
3-dimensional object where the third dimension is equal to 3

## Examples

``` r

# path = system.file("tmp_images", "1.png", package = "OpenImageR")

# imageShow(path)
```
