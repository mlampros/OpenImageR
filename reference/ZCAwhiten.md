# zca whiten of an image

this function performs zca-whitening to a 2- or 3- dimensional image

## Usage

``` r
ZCAwhiten(image, k, epsilon)
```

## Arguments

- image:

  a matrix, data frame or 3-dimensional array where the third dimension
  is equal to 3

- k:

  an integer specifying the number of components to keep when svd is
  performed (reduced dimension representation of the data), between 1
  and the number of rows of the image

- epsilon:

  a float specifying the regularization parameter

## Value

a matrix or 3-dimensional array where the third dimension is equal to 3

## Details

Whitening (or sphering) is the preprocessing needed for some algorithms.
If we are training on images, the raw input is redundant, since adjacent
pixel values are highly correlated. When using whitening the features
become less correlated and all features have the same variance.

## References

http://ufldl.stanford.edu/tutorial/unsupervised/PCAWhitening/

## Examples

``` r

path = system.file("tmp_images", "1.png", package = "OpenImageR")

image = readImage(path)

res = ZCAwhiten(image, k = 20, epsilon = 0.1)
```
