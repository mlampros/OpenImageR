# convolution

convolution

## Usage

``` r
convolution(image, kernel, mode = "same")
```

## Arguments

- image:

  either a matrix, data frame or array

- kernel:

  a kernel in form of a matrix

- mode:

  the convolution mode (one of 'same', 'full')

## Value

either a matrix or an array, depending on the input data

## Details

This function performs convolution using a kernel matrix. When mode
'same' the output object has the same dimensions with the input, whereas
when mode 'full' the rows and columns of the output object equals : ROWS
= nrow(image) + nrow(kernel) - 1 and COLUMNS = ncol(image) +
ncol(kernel) - 1

## Author

Lampros Mouselimis

## Examples

``` r


# kernel
x = matrix(1, nrow = 4, ncol = 4) / 16   # uniform


# matrix
image_matrix = matrix(runif(100), 10, 10)

res = convolution(image_matrix, x, "same")


# array
image_array = array(runif(300), dim = c(10, 10, 3))

res = convolution(image_array, x, "same")
```
