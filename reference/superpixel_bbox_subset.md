# Bounding box for a subset of superpixel labels

Bounding box for a subset of superpixel labels

## Usage

``` r
superpixel_bbox_subset(superpixel_labels, superpixel_subset)
```

## Arguments

- superpixel_labels:

  a matrix. The *superpixel_labels* parameter corresponds to the output
  *labels* of the *superpixels* function

- superpixel_subset:

  a numeric or integer vector specifying the subset of superpixel
  segments.

## Details

This function should be utilized to return the bounding box for a subset
of superpixel segments. To compute the bounding box for all superpixels
use the *superpixel_bbox* function.

## Examples

``` r

library(OpenImageR)


#-----------
# read image
#-----------

path = system.file("tmp_images", "slic_im.png", package = "OpenImageR")

im = readImage(path)

im = im[,, 1:3]


#--------------------
# compute superpixels
#--------------------

res = superpixels(input_image = im, method = "slic", superpixel = 200,

                  compactness = 20, return_labels = TRUE)
#> Warning: The input data has values between 0.094118 and 1.000000. The image-data will be multiplied by the value: 255!


#-------------------------
# compute the bounding box   ( for subset of superpixels )
#-------------------------

bbox = superpixel_bbox_subset(res$labels, superpixel_subset = c(0, 10, 30))
```
