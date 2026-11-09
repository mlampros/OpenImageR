# Padding of matrices or n-dimensional arrays with a user specified value

Padding of matrices or n-dimensional arrays with a user specified value

## Usage

``` r
padding(input_data, new_rows, new_cols, fill_value = 0)
```

## Arguments

- input_data:

  either a matrix or a 3-dimensional array where the third dimension is
  equal to 3

- new_rows:

  an integer specifying the new rows of the output matrix or array

- new_cols:

  an integer specifying the new columns of the output matrix or array

- fill_value:

  a numeric value to fill the extended rows / columns of the initial
  input data

## Value

a list

## Details

The *padding* function returns a list, where *data* is the padded /
extended matrix or array and *padded_start*, *padded_end*, *padded_left*
and *padded_right* are integer values specifying how many rows or
columsn in up-, down-, left- or right-direction the input matrix or
array was padded / extended with the specified fill-value.

## Examples

``` r

library(OpenImageR)


#-------
# matrix
#-------

set.seed(1)
mt = matrix(runif(100), 10, 10)

res_mt = padding(mt, 15, 20, fill_value = -1)


#------
# array
#------

lst = list(matrix(1, 10, 10), matrix(2, 10, 10))

arr = List_2_Array(lst, verbose = FALSE)

res_arr = padding(arr, 15, 20, fill_value = mean(as.vector(mt)))
```
