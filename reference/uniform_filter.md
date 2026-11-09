# uniform filter (convolution with uniform kernel)

uniform filter (convolution with uniform kernel)

## Usage

``` r
uniform_filter(image, size, conv_mode = "same")
```

## Arguments

- image:

  matrix or 3-dimensional array where the third dimension is equal to 3

- size:

  a 2-item vector specifying the horizontal and vertical dimensions of
  the uniform kernel, e.g. c(3,3)

- conv_mode:

  the convolution mode should be one of 'same', 'full'

## Value

depending on the input, either a matrix or an array

## Details

This function applies a uniform filter to a matrix or to a 3-dimensional
array where the third dimension is equal to 3

## Author

Lampros Mouselimis

## Examples

``` r

path = system.file("tmp_images", "1.png", package = "OpenImageR")

image = readImage(path)

filt = uniform_filter(image, c(4,4), conv_mode = "same")
```
