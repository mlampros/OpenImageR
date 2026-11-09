# calculate the HOG (Histogram of oriented gradients) for an image

The function is a modification of the 'findHOGFeatures' function of the
SimpleCV package \[ please consult the COPYRIGHT file \] The function
takes either an RGB (it will be converted to gray) or a gray image and
returns a vector of the HOG descriptors. The main purpose of the
function is to create a vector of features, which can be used in
classification tasks.

## Usage

``` r
HOG(image, cells = 3, orientations = 6)
```

## Arguments

- image:

  matrix or 3-dimensional array where the third dimension is equal to 3

- cells:

  the number of divisions ( cells )

- orientations:

  number of orientation bins

## Value

a numeric vector

## Details

This function takes either a matrix, a data frame or a 3-dimensional
array (where the third dimension is equal to 3) and returns a vector
with the HOG-descriptors (histogram of oriented gradients).

## Examples

``` r

if (FALSE) { # \dontrun{

path = system.file("tmp_images", "1.png", package = "OpenImageR")

image = readImage(path)

res = HOG(image, cells = 3, orientations = 6)
} # }
```
