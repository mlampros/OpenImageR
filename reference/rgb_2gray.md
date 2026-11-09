# convert an RGB image to Gray

convert an RGB image to Gray

## Usage

``` r
rgb_2gray(RGB_image)
```

## Arguments

- RGB_image:

  a 3-dimensional array where the third dimension is equal to 3

## Value

a matrix

## Details

This function converts an RGB image to gray

## Author

Lampros Mouselimis

## Examples

``` r

path = system.file("tmp_images", "1.png", package = "OpenImageR")

image = readImage(path)

gray = rgb_2gray(image)
```
