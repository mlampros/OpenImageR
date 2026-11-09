# Rotate an image using the 'nearest' or 'bilinear' method

Rotate an image by angle using the 'nearest' or 'bilinear' method

## Usage

``` r
rotateImage(image, angle, method = "nearest", mode = "same", threads = 1)
```

## Arguments

- image:

  matrix, data frame or 3-dimensional array where the third dimension is
  equal to 3

- angle:

  specifies the number of degrees

- method:

  a string specifying the interpolation method when rotating an image (
  'nearest', 'bilinear' )

- mode:

  one of 'full', 'same' (same indicates that the ouput image will have
  the same dimensions with initial image)

- threads:

  the number of cores to run in parallel

## Value

depending on the input, either a matrix or an array

## Details

This function rotates an image by a user-specified angle. A positive
angle turns the image counter-clockwise (as displayed with the first row
at the top, for instance by imageShow()), the opposite direction of
rotateFixed(): rotateImage(image, 90) gives the same result as
rotateFixed(image, 270) for a square image.

## Examples

``` r

path = system.file("tmp_images", "2.jpg", package = "OpenImageR")

image = readImage(path)

r = rotateImage(image, 75, threads = 1)
```
