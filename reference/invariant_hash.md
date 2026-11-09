# invariant hashing (caclulation of the hamming or the levenshtein distance when the image is flipped, rotated or cropped)

flip-rotate-crop an image and caclulate the hamming or the levenshtein
distance for phash, average_hash, dhash

## Usage

``` r
invariant_hash(
  image,
  new_image,
  method = "phash",
  mode = "binary",
  hash_size = 8,
  highfreq_factor = 4,
  resize = "nearest",
  flip = T,
  rotate = T,
  angle_bidirectional = 10,
  crop = T
)
```

## Arguments

- image:

  a 2-dimensional matrix or data frame (only gray-scale images are
  valid)

- new_image:

  a new image to be compared with the previous input image

- method:

  one of 'phash', 'average_hash', 'dhash'

- mode:

  one of 'binary', 'hash'

- hash_size:

  an integer specifying the hash size. IF method = 'phash' : the
  hash_size \* highfreq_factor should be less than number of floor(rows
  \* 0.8) or floor(columns \* 0.8) of the gray_image IF method = 'dhash'
  or 'average_hash' : the hash_size should be less than number of
  floor(rows \* 0.8) or floor(columns \* 0.8) of the gray_image

- highfreq_factor:

  an integer specyfing the highfrequency factor (IF method = 'phash' :
  the hash_size \* highfreq_factor should be less than number of
  floor(rows \* 0.8) or floor(columns \* 0.8) of the gray_image)

- resize:

  corresponds to one of 'nearest', 'bilinear' (resizing method)

- flip:

  if TRUE the new_image will be flipped both horizontal and vertical

- rotate:

  if TRUE the new_image will be rotated for a specified angle (see
  angle_bidirectional)

- angle_bidirectional:

  a float specifying the angle that the images should be rotated in both
  directions. For instance, if angle_bidirectional = 10 then the image
  will be rotated for 10 and 350 (360-10) degrees.

- crop:

  if TRUE the new_image will be cropped 10 or 20 percent (equally spaced
  horizontally and vertically)

## Value

If flip, rotate and crop are all FALSE then the function returns either
the hamming distance (if mode = 'binary') or the levenshtein distance
(if mode = 'hash') for the two images. If any of the flip, rotate, crop
is TRUE then it returns the MIN, MAX of the hamming distance (if mode =
'binary') or the MIN,MAX of the levenshtein distance (if mode = 'hash').

## Details

This function performs the following transformations : flips an image
(no-flip, horizonal-flip, vertical-flip), rotates an image (no-angle,
angle_bidirectional, 360-angle_bidirectional) and crops an image
(no-crop, 10-percent-crop, 20-percent-crop). Depending on the type of
mode ('binary', 'hash'), after each transformation the hamming or the
levenshtein distance between the two images is calculated.

## Examples

``` r

if (FALSE) { # \dontrun{

path1 = system.file("tmp_images", "1.png", package = "OpenImageR")

path2 = system.file("tmp_images", "2.jpg", package = "OpenImageR")

image1 = rgb_2gray(readImage(path1))

image2 = rgb_2gray(readImage(path2))

res1 = invariant_hash(image1, image2, hash_size = 3, flip = TRUE, crop = FALSE)

res2 = invariant_hash(image1, image2, mode = 'hash', hash_size = 3, angle_bidirectional = 10)
} # }
```
