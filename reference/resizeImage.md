# resize an image using the 'nearest neighbors' or the 'bilinear' method

resize an image using the 'nearest neighbors' or the 'bilinear' method

## Usage

``` r
resizeImage(image, width, height, method = "nearest", normalize_pixels = FALSE)
```

## Arguments

- image:

  matrix or 3-dimensional array where the third dimension is equal to 3

- width:

  a number specifying the new width of the image. Corresponds to the
  image-rows.

- height:

  a number specifying the new height of the image. Corresponds to the
  image-columns.

- method:

  one of 'nearest', 'bilinear'

- normalize_pixels:

  a boolean. If TRUE, then the output pixel values will be divided by
  255.0

## Value

depending on the input, either a matrix or an array

## Details

This function down- or upsamples an image using the 'nearest neighbors'
or the 'bilinear' method

## Author

Lampros Mouselimis

## Examples

``` r

path = system.file("tmp_images", "2.jpg", package = "OpenImageR")

image = readImage(path)

resiz = resizeImage(image, width = 32, height = 32, method = 'nearest')
```
