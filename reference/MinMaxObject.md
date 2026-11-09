# minimum and maximum values of vector, matrix, data frame or array

minimum and maximum values of vector, matrix, data frame or array

## Usage

``` r
MinMaxObject(x)
```

## Arguments

- x:

  either a vector, matrix, data frame or array

## Value

a list

## Details

This helper function returns the minimum and maximum values of a vector,
2-dimensional or 3-dimensional objects (where the third dimension is
equal to 3). In case of a vector, matrix or data frame it returns a
single value for the minimum and maximum of the object. In case of an
array it returns the minimum and maximum values for each slice of the
array.

## Author

Lampros Mouselimis

## Examples

``` r

# vector
x = 1:10

res = MinMaxObject(x)


# matrix
x = matrix(runif(100), 10, 10)

res = MinMaxObject(x)


# data frame
x = data.frame(matrix(runif(100), 10, 10))

res = MinMaxObject(x)


# array
x = array(runif(300), dim = c(10, 10, 3))

res = MinMaxObject(x)
```
