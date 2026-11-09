# flip image horizontally or vertically

flip an image row-wise (horizontally) or column-wise (vertically)

## Usage

``` r
flipImage(image, mode = "horizontal")
```

## Arguments

- image:

  a matrix, data frame or 3-dimensional array where the third dimension
  is equal to 3

- mode:

  one of 'horizontal', 'vertical'

## Value

a matrix or 3-dimensional array where the third dimension is equal to 3

## Details

This function flips an image row-wise or column-wise

## Examples

``` r

path = system.file("tmp_images", "1.png", package = "OpenImageR")

im = readImage(path)

flp = flipImage(im, mode = 'vertical')
```
