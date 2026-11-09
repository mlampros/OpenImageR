# loads either 2- or 3-dimensional data (where the third dimension is equal to 3) from a binary file

loads either 2- or 3-dimensional data (where the third dimension is
equal to 3) from a binary file

## Usage

``` r
load_binary(path, type)
```

## Arguments

- path:

  a character string specifying a file path ( where the binary data is
  saved )

- type:

  a character string. Either '2d' or '3d' to indicate what kind of data
  data will be loaded from the specified *path*

## Details

This function can be used to load either 2- or 3-dimensional data (where
the third dimension is equal to 3) from a binary file. It is used in
combination with the *superpixels* function in case that the
*write_slic* parameter is not an empty string ("").

## Examples

``` r

if (FALSE) { # \dontrun{

library(OpenImageR)

#------------------------------------------
# assuming the saved data are 2-dimensional
#------------------------------------------

path = "/my_dir/data.bin"

res = load_binary(path, type = '2d')

} # }
```
