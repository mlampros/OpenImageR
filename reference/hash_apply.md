# calculate the binary or the hexadecimal hash for a matrix, array or a folder of images for the average_hash, phash or dhash functions

This function takes either a matrix, array or a folder and returns
either the binary hash features or the hashes (as a character vector)

## Usage

``` r
hash_apply(
  object,
  rows = 28,
  columns = 28,
  hash_size = 8,
  highfreq_factor = 3,
  method = "phash",
  mode = "binary",
  threads = 1,
  resize = "nearest"
)
```

## Arguments

- object:

  a matrix, a data frame, a 3-dimensional array (where the third
  dimension is equal to 3) or a path to a folder of files (images)

- rows:

  a number specifying the number of rows of the matrix

- columns:

  a number specifying the number of columns of the matrix

- hash_size:

  an integer specifying the hash size. IF method = 'phash' : the
  hash_size \* highfreq_factor should be less than number of rows or
  columns of the gray_image. IF method = 'dhash' or 'average_hash' : the
  hash_size should be less than number of rows or columns of the
  gray_image

- highfreq_factor:

  an integer specyfing the highfrequency factor (IF method = 'phash' :
  the hash_size \* highfreq_factor should be less than number of rows or
  columns of the gray_image)

- method:

  one of 'phash', 'average_hash', 'dhash'

- mode:

  one of 'binary', 'hash'

- threads:

  the number of cores to run in parallel

- resize:

  corresponds to one of 'nearest', 'bilinear' (resizing method)

## Value

If the input is a matrix, data frame or array this function returns a
matrix (if mode = 'binary') or a character vector (if mode =
'hex_hash'). If the input is a path to a folder the function returns a
list of length 2, the 1st sublist is a vector with the names of the
image files (the order of the files in the vector corresponds to the
order of the rows of the output matrix), the 2nd sublist is a matrix (if
mode = 'binary') or a character vector (if mode = 'hex_hash').

## Details

This function calculates the binary hash or the hexadecimal hash for
various types of objects.

## Examples

``` r

path = paste0(system.file("tmp_images", "same_type", package = "OpenImageR"), '/')

res_phash = hash_apply(path, method = 'phash', mode = 'binary')
#> 
#> time to complete : 0.0005927086 secs 
#> 
```
