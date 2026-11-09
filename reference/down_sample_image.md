# downsampling an image ( by a factor ) using gaussian blur

downsampling an image ( by a factor ) using gaussian blur

## Usage

``` r
down_sample_image(
  image,
  factor,
  gaussian_blur = FALSE,
  gauss_sigma = 1,
  range_gauss = 2
)
```

## Arguments

- image:

  matrix or 3-dimensional array where the third dimension is equal to 3

- factor:

  a positive number greater or equal to 1.0

- gaussian_blur:

  a boolean (TRUE,FALSE) specifying if gaussian blur should be applied
  when downsampling

- gauss_sigma:

  float parameter sigma for the gaussian filter

- range_gauss:

  float number specifying the range of values for the gaussian filter

## Value

depending on the input, either a matrix or an array

## Details

This function downsamples an image with the option to use gaussian blur
for optimal output.

## Author

Lampros Mouselimis

## Examples

``` r

path = system.file("tmp_images", "2.jpg", package = "OpenImageR")

image = readImage(path)

dsamp = down_sample_image(image, factor = 2.0, gaussian_blur = TRUE)
```
