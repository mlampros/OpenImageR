# image augmentations of a matrix, data frame, array or a list of 3-dimensional arrays (where the third dimension is equal to 3)

image augmentations of a matrix, data frame, array or a list of
3-dimensional arrays (where the third dimension is equal to 3)

## Usage

``` r
Augmentation(
  image,
  flip_mode = NULL,
  crop_width = NULL,
  crop_height = NULL,
  resiz_width = 0,
  resiz_height = 0,
  resiz_method = "nearest",
  shift_rows = 0,
  shift_cols = 0,
  rotate_angle = 0,
  rotate_method = "nearest",
  zca_comps = 0,
  zca_epsilon = 0,
  image_thresh = 0,
  padded_value = 0,
  verbose = FALSE
)
```

## Arguments

- image:

  a matrix, data frame, array or list of 3-dimensional arrays where the
  third dimension is equal to 3

- flip_mode:

  a character string ('horizontal', 'vertical')

- crop_width:

  an integer specifying the new width of the image, after the image is
  cropped. Corresponds to the image-rows.

- crop_height:

  an integer specifying the new height of the image, after the image is
  cropped. Corresponds to the image-columns.

- resiz_width:

  an integer specifying the new width of the image, after the image is
  resized. Corresponds to the image-rows.

- resiz_height:

  an integer specifying the new height of the image, after the image is
  resized. Corresponds to the image-columns.

- resiz_method:

  a string specifying the interpolation method when resizing an image
  ('nearest', 'bilinear')

- shift_rows:

  a positive or negative integer specifying the direction that the rows
  should be shifted

- shift_cols:

  a positive or negative integer specifying the direction that the
  columns should be shifted

- rotate_angle:

  an integer specifying the rotation angle of the image

- rotate_method:

  a string specifying the interpolation method when rotating an image
  ('nearest', 'bilinear')

- zca_comps:

  an integer specifying the number of components to keep by zca
  whitening, when svd is performed

- zca_epsilon:

  a float specifying the regularization parameter by zca whitening

- image_thresh:

  the threshold parameter, by image thresholding, should be between 0
  and 1 if the data is normalized or between 0-255 otherwise

- padded_value:

  either a numeric value or a numeric vector of length equal to N of an
  N-dimensional array. If it's not equal to 0 then the values of the
  shifted rows or columns will be filled with the user-defined
  padded_value. Applies only to the shift_rows and shift_cols
  parameters.

- verbose:

  a boolean (TRUE, FALSE). If TRUE, then the total time of the
  preprocessing task will be printed.

## Value

the output is of the same type with the input (in case of a data frame
it returns a matrix)

## Details

This function takes advantage of various methods to accomplish image
augmentations. The order of the preprocessing steps, in case that all
transformations are applied to an image, is : 1st flip image, 2nd crop
image, 3rd resize image, 4th shift rows or columns, 5th rotate image,
6th zca-whitening and 7th image-thresholding.

## Author

Lampros Mouselimis

## Examples

``` r

if (FALSE) { # \dontrun{

# a matrix
object = matrix(1, 10, 10)

res = Augmentation(object, resiz_width = 8, resiz_height = 8, rotate_angle = 40)


# an array
object = array(0, dim = c(10, 10, 3))

res = Augmentation(object, resiz_width = 8, resiz_height = 8, rotate_angle = 30)


# an array (multiple matrices)
object = array(0, dim = c(10, 10, 10))

res = Augmentation(object, resiz_width = 8, resiz_height = 8, rotate_angle = 20)


# a list of 3-dimensional arrays (where the third dimension is equal to 3)
object = list(array(0, dim = c(10, 10, 3)), array(0, dim = c(10, 10, 3)))

res = Augmentation(object, resiz_width = 8, resiz_height = 8, rotate_angle = 40)
} # }
```
