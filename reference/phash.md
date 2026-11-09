# calculation of the 'phash' of an image

This function calculates the phash of an image

## Usage

``` r
phash(
  gray_image,
  hash_size = 8,
  highfreq_factor = 4,
  MODE = "hash",
  resize = "nearest"
)
```

## Arguments

- gray_image:

  a (2-dimensional) matrix or data frame

- hash_size:

  an integer specifying the hash size (hash_size \* highfreq_factor
  should be less than number of rows or columns of the gray_image)

- highfreq_factor:

  an integer specyfing the highfrequency factor (hash_size \*
  highfreq_factor should be less than number of rows or columns of the
  gray_image)

- MODE:

  one of 'hash' (returns the hash of the image), 'binary' (returns
  binary identifier of the image)

- resize:

  corresponds to one of 'nearest', 'bilinear' (resizing method)

## Value

either a hash-string or a binary vector

## Details

The function is a modification of the 'phash' function of the imagehash
package \[ please consult the COPYRIGHT file \]. The phash algorithm
extends the average_hash by using the discrete cosine transform.

## Examples

``` r

image = readImage(system.file("tmp_images", "2.jpg", package = "OpenImageR"))

image = rgb_2gray(image)

res_hash = phash(image, hash_size = 6, highfreq_factor = 3, MODE = 'hash')

res_binary = phash(image, hash_size = 6, highfreq_factor = 3, MODE = 'binary')
```
