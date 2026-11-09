# Normalize a matrix to specific range of values

Normalize a matrix to specific range of values

## Usage

``` r
norm_matrix_range(data, min_value = -1, max_value = 1)
```

## Arguments

- data:

  a matrix

- min_value:

  the new minimum value for the input *data*

- max_value:

  the new maximum value for the input *data*

## Value

a matrix

## Examples

``` r

set.seed(1)
mt = matrix(1:48, 8, 6)

res = norm_matrix_range(mt, min_value = -1, max_value = 1)
```
