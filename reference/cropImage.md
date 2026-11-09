# crop an image

crop an image

## Usage

``` r
cropImage(image, new_width, new_height, type = "equal_spaced")
```

## Arguments

- image:

  matrix or 3-dimensional array where the third dimension is equal to 3

- new_width:

  Corresponds to the image-rows. If 'equal_spaced' then the new_width
  should be numeric of length 1. If 'user_defined' then the new_width
  should be a sequence of numeric values.

- new_height:

  Corresponds to the image-columns. If 'equal_spaced' then the
  new_height should be numeric of length 1. If 'user_defined' then the
  new_height should be a sequence of numeric values.

- type:

  a string specifying the type ('equal_spaced' or 'user_defined'). If
  'equal_spaced' the image will be cropped towards the center (equal
  distances horizontaly and verticaly). If 'user_defined' the user
  specifies the cropped region.

## Value

depending on the input, either a matrix or an array

## Details

This function crops an image in two different ways.

## Author

Lampros Mouselimis

## Examples

``` r

path = system.file("tmp_images", "2.jpg", package = "OpenImageR")

image = readImage(path)

# IF 'equal_spaced':
crop1 = cropImage(image, new_width = 20, new_height = 20, type = 'equal_spaced')

# IF 'user_defined':
crop2 = cropImage(image, new_width = 5:20, new_height = 5:20, type = 'user_defined')
```
