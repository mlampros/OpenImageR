# Conversion of RGB to HSV colour type

Conversion of RGB to HSV colour type

## Usage

``` r
RGB_to_HSV(input_data)
```

## Arguments

- input_data:

  a 3-dimensional array (RGB image) where the third dimension is equal
  to 3

## Details

Meaning: RGB (Red-Green-Blue) to HSV (Hue, Saturation, Value) colour
conversion

## Examples

``` r

library(OpenImageR)

set.seed(1)
array_3d = array(sample(1:255, 675, replace = TRUE), c(15, 15, 3))

res = RGB_to_HSV(array_3d)
```
