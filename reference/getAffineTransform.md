# Get Affine Transform

Get Affine Transform

## Usage

``` r
getAffineTransform(original_points, transformed_points)
```

## Arguments

- original_points:

  a matrix object that corresponds to the original points

- transformed_points:

  a matrix object that corresponds to the transformed points

## Value

a matrix

## References

https://github.com/OlehOnyshchak/ImageTransformations/blob/master/AffineTransformation.ipynb

## Examples

``` r

require(OpenImageR)

r = 600
c = 600
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
M_aff
#>            [,1]        [,2] [,3]
#> [1,] 0.91666667 -0.08333333   50
#> [2,] 0.08333333  0.91666667    0
```
