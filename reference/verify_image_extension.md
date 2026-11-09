# Verify that the input image extension is valid

Verify that the input image extension is valid

## Usage

``` r
verify_image_extension(image_path, regex_img = "jpe?g|png|tif$|tiff$")
```

## Arguments

- image_path:

  a character string specifying the path to the saved image

- regex_img:

  a character string specifying the regex used to verify if the image
  extension is valid

## Value

either the image path extension or an error

## Details

The OpenImageR package uses the 'readPNG', 'readJPEG' and 'readTIFF' R
packages in the background. Thus, only image file path extensions that
can be processed from these R packages should be used as input to the
'readImage' function

## References

https://github.com/mlampros/OpenImageR/issues/25

## Examples

``` r

vec_img_ext = c('png', 'PNG', 'jpg', 'JPG', 'jpeg', 'JPEG', 'tif', 'TIF', 'tiff', 'TIFF')

vec_valid = sapply(vec_img_ext, function(x) {
  ext_iter = paste(c('example_image', x), collapse = '.')
  verify_image_extension(image_path = ext_iter)
})

all(vec_img_ext == vec_valid)
#> [1] TRUE
```
