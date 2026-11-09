# normalize a vector, matrix or array (in the range between 0 and 1)

normalize a vector, matrix or array (in the range between 0 and 1)

## Usage

``` r
NormalizeObject(x)
```

## Arguments

- x:

  either a vector, matrix, data frame or array

## Value

either a normalized vector, matrix, or array

## Details

This is a helper function which normalizes all pixel values of the
object to the range between 0 and 1. The function takes either a vector,
matrix, data frame or array as input and returns a normalized object of
the same type (in case of data frame it returns a matrix).

## Author

Lampros Mouselimis

## Examples

``` r

# vector
x = 1:10

res = NormalizeObject(x)


# matrix
x = matrix(runif(100), 10, 10)

res = NormalizeObject(x)


# data frame
x = data.frame(matrix(runif(100), 10, 10))

res = NormalizeObject(x)


# array
x = array(runif(300), dim = c(10, 10, 3))

res = NormalizeObject(x)
```
