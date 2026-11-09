# this function reads various types of images

Reads images of type .png, .jpeg, .jpg, .tiff

## Usage

``` r
readImage(path, ...)
```

## Arguments

- path:

  a character string specifying the path to the saved image

- ...:

  further arguments for the readPNG, readJPEG and readTIFF functions

## Value

the image in a matrix or array form

## Details

This function takes as input a string-path and returns the image in a
matrix or array form. Supported types of images are .png, .jpeg, .jpg,
.tiff. Extension types similar to .tiff such as .tif, .TIFF, .TIF are
also supported

## Examples

``` r

path = system.file("tmp_images", "1.png", package = "OpenImageR")

image = readImage(path)
```
