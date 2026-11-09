# image translation

shift the position of an image by adding/subtracting a value to/from the
X or Y coordinates

## Usage

``` r
translation(image, shift_rows = 0, shift_cols = 0, padded_value = 0)
```

## Arguments

- image:

  a matrix, data frame or 3-dimensional array where the third dimension
  is equal to 3

- shift_rows:

  a positive or negative integer specifying the direction that the rows
  should be shifted

- shift_cols:

  a positive or negative integer specifying the direction that the
  columns should be shifted

- padded_value:

  either a numeric value or a numeric vector of length 3 (corresponding
  to RGB). If it's not equal to 0 then the values of the shifted rows or
  columns will be filled with the user-defined padded_value

## Value

a matrix or 3-dimensional array where the third dimension is equal to 3

## Details

If shift_rows is not zero then the image will be sifted row-wise
(upsides or downsides depending on the sign). If shift_cols is not zero
then the image will be sifted column-wise (right or left depending on
the sign).

## Examples

``` r

path = system.file("tmp_images", "1.png", package = "OpenImageR")

image = readImage(path)

res_tr = translation(image, shift_rows = 10, shift_cols = -10)
```
