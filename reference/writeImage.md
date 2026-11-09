# This function writes 2- or 3-dimensional image (where the third dimension is equal to 3) data to a file

This function writes 2- or 3-dimensional image (where the third
dimension is equal to 3) data to a file. Supported types are .png,
.jpeg, .jpg, .tiff (or .tif, .TIFF, .TIF)

## Usage

``` r
writeImage(data, file_name, ...)
```

## Arguments

- data:

  a 2- or 3-dimensional object (matrix, data frame or array where the
  third dimension is equal to 3)

- file_name:

  a string specifying the name of the new file

- ...:

  further arguments for the writePNG, writeJPEG and writeTIFF functions

## Value

a saved image file

## Details

This function takes as input a matrix, data frame or array and saves the
data in one of the supported image types ( .png, .jpeg, .jpg, .tiff ).
Extension types similar to .tiff such as .tif, .TIFF, .TIF are also
supported

## Examples

``` r

# path = system.file("tmp_images", "1.png", package = "OpenImageR")

# im = readImage(path)

# writeImage(im, 'new_image.jpeg')
```
