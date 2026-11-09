# Conversion of RGB to Lab colour type

Conversion of RGB to Lab colour type

## Usage

``` r
RGB_to_Lab(input_data)
```

## Arguments

- input_data:

  a 3-dimensional array (RGB image) where the third dimension is equal
  to 3. Values either in \[0, 1\] (they are multiplied by 255) or in
  0-255 (they are truncated to integers)

## Details

Meaning: RGB (Red-Green-Blue) to LAB (Lightness, A-colour-dimension,
B-colour-dimension) colour conversion

## References

https://www.epfl.ch/labs/ivrl/research/snic-superpixels/

## Examples

``` r

library(OpenImageR)

set.seed(1)
array_3d = array(sample(1:255, 675, replace = TRUE), c(15, 15, 3))

res = RGB_to_Lab(array_3d)
```
