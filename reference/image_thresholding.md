# image thresholding

image thresholding

## Usage

``` r
image_thresholding(image, thresh)
```

## Arguments

- image:

  matrix or 3-dimensional array where the third dimension is equal to 3

- thresh:

  the threshold parameter should be between 0 and 1 if the data is
  normalized or between 0-255 otherwise

## Value

a matrix

## Details

This function applies thresholding to a matrix or to a 3-dimensional
array where the third dimension is equal to 3.

## Author

Lampros Mouselimis

## Examples

``` r

path = system.file("tmp_images", "1.png", package = "OpenImageR")

image = readImage(path)

filt = image_thresholding(image, thresh = 0.5)
```
