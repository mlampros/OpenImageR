# Gamma correction

Gamma correction

## Usage

``` r
gamma_correction(image, gamma)
```

## Arguments

- image:

  matrix or 3-dimensional array where the third dimension is equal to 3

- gamma:

  a positive value

## Value

depending on the input, either a matrix or an array

## Details

This function applies gamma correction to a matrix or to a 3-dimensional
array where the third dimension is equal to 3. The gamma correction
controls the overall brightness of an image.

## Author

Lampros Mouselimis

## Examples

``` r

path = system.file("tmp_images", "2.jpg", package = "OpenImageR")

image = readImage(path)

filt = gamma_correction(image, gamma = 0.5)
```
