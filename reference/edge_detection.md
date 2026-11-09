# edge detection (Frei_chen, LoG, Prewitt, Roberts_cross, Scharr, Sobel)

edge detection (Frei_chen, LoG, Prewitt, Roberts_cross, Scharr, Sobel)

## Usage

``` r
edge_detection(
  image,
  method = NULL,
  conv_mode = "same",
  approx = F,
  gaussian_dims = 5,
  sigma = 1,
  range_gauss = 2,
  laplacian_type = 1
)
```

## Arguments

- image:

  matrix or 3-dimensional array and the third dimension must be equal to
  3

- method:

  the method should be one of 'Frei_chen', 'LoG' (Laplacian of
  Gaussian), 'Prewitt', 'Roberts_cross', 'Scharr', 'Sobel'

- conv_mode:

  the convolution mode should be one of 'same', 'full'

- approx:

  if TRUE, approximate calculation of gradient (applies to all filters
  except for 'LoG')

- gaussian_dims:

  integer specifying the horizontal and vertical dimensions of the
  gaussian filter

- sigma:

  float parameter sigma for the gaussian filter

- range_gauss:

  float number specifying the range of values for the gaussian filter

- laplacian_type:

  integer value specifying the type for the laplacian kernel (one of 1,
  2, 3, 4)

## Value

depending on the input, either a matrix or an array

## Details

This function takes either a matrix or a 3-dimensional array (where the
third dimension is equal to 3) and it performs edge detection using one
of the following filters : 'Frei_chen', 'LoG' (Laplacian of Gaussian),
'Prewitt', 'Roberts_cross', 'Scharr', 'Sobel'

## Author

Lampros Mouselimis

## Examples

``` r

path = system.file("tmp_images", "1.png", package = "OpenImageR")

image = readImage(path)

res = edge_detection(image, method = 'Frei_chen', conv_mode = 'same')
```
