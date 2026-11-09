# convert a list of matrices to an array of matrices

convert a list of matrices to an array of matrices

## Usage

``` r
List_2_Array(data, verbose = FALSE)
```

## Arguments

- data:

  a list of matrices

- verbose:

  if TRUE then the time taken to complete the task will be printed

## Value

an array

## Details

This is a helper function mainly for the HOG and hash functions. In case
that matrices are stored in a list, this function converts the list to
an array of 2-dimensional data.

## Author

Lampros Mouselimis

## Examples

``` r

lst = list(matrix(0, 100, 100), matrix(1, 100, 100))

arr = List_2_Array(lst, verbose = FALSE)
```
