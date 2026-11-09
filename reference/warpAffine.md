# Warp Affine

Warp Affine

## Usage

``` r
warpAffine(img, M, R, C, threads = 1, verbose = FALSE)
```

## Arguments

- img:

  either a matrix or a 3-dimensional array (where the third dimension is
  equal to 3) with a range of values between 0 and 255

- M:

  a matrix corresponding to the transformation matrix

- R:

  a value corresponding to the destination number of rows

- C:

  a value corresponding to the destination number of columns

- threads:

  an integer specifying the number of threads to run in parallel. This
  parameter applies only if the input "img" parameter is of type matrix.

- verbose:

  a boolean. If TRUE then information will be printed in the console

## Value

either a matrix or a 3-dimensional array (where the third dimension is
equal to 3)

## References

https://github.com/OlehOnyshchak/ImageTransformations/blob/master/AffineTransformation.ipynb

## Examples

``` r

require(OpenImageR)

path = system.file("tmp_images", "landscape.jpg", package = "OpenImageR")
img = readImage(path)
img = img * 255

#.............................
# compute the affine transform
#.............................

r = ncol(img)
c = nrow(img)
offset = 50

original_points = matrix(data = c(0, 0, r, 0, 0, c),
                         nrow = 3,
                         ncol = 2,
                         byrow = TRUE)

transformed_points = matrix(data = c(offset, 0, r, offset, 0, c-offset),
                            nrow = 3,
                            ncol = 2,
                            byrow = TRUE)

M_aff = getAffineTransform(original_points = original_points,
                           transformed_points = transformed_points)

#..............
# 2-dimensional
#..............

img_2d = rgb_2gray(img)

res_2d = warpAffine(img = img_2d,
                    M = M_aff,
                    R = r,
                    C = c,
                    threads = 1,
                    verbose = TRUE)
#> time to complete : 0.0074 secs 

# imageShow(res_2d)

#..............
# 3-dimensional
#..............

res_3d = warpAffine(img = img,
                    M = M_aff,
                    R = r,
                    C = c,
                    verbose = TRUE)
#> time to complete : 0.03 secs 

# imageShow(res_3d)
```
