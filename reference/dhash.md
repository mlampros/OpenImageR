# calculation of the 'dhash' of an image

This function calculates the dhash of an image

## Usage

``` r
dhash(gray_image, hash_size = 8, MODE = "hash", resize = "nearest")
```

## Arguments

- gray_image:

  a (2-dimensional) matrix or data frame

- hash_size:

  an integer specifying the hash size (should be less than number of
  rows or columns of the gray_image)

- MODE:

  one of 'hash' (returns the hash of the image), 'binary' (returns
  binary identifier of the image)

- resize:

  corresponds to one of 'nearest', 'bilinear'

## Value

either a hash-string or a binary vector

## Details

The function is a modification of the 'dhash' function of the imagehash
package \[ please consult the COPYRIGHT file \]. In comparison to
average_hash and phash, the dhash algorithm takes into consideration the
difference between adjacent pixels.

## Examples

``` r

image = readImage(system.file("tmp_images", "3.jpeg", package = "OpenImageR"))

image = rgb_2gray(image)

res_hash = dhash(image, hash_size = 8, MODE = 'hash')

res_binary = dhash(image, hash_size = 8, MODE = 'binary')
```
