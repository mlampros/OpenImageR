# Warp Affine using R

The **getAffineTransform()** and **warpAffine()** functions of the
OpenImageR package is an RcppArmadillo re-implementation of [existing
Python
Code](https://github.com/OlehOnyshchak/ImageTransformations/blob/master/AffineTransformation.ipynb)
and this vignette shows how these functions can be used from within R
based on the [author’s
.ipynb](https://github.com/OlehOnyshchak/ImageTransformations/blob/master/AffineTransformation.ipynb)
file

  

``` r

require(OpenImageR)
```

    ## Loading required package: OpenImageR

``` r

path = system.file('tmp_images', 'landscape.jpg', package = "OpenImageR")

img = readImage(path)
print(dim(img))
```

    ## [1] 600 600   3

  

![Input
Image](../../../_temp/Library/OpenImageR/tmp_images/landscape.jpg)

Input Image

  

``` r

r = ncol(img)
c = nrow(img)
offset = 50

original_points = matrix(data = c(0, 0, r, 0, 0, c),
                         nrow = 3,
                         ncol = 2,
                         byrow = TRUE)

transformed_points = matrix(data = c(offset, 0, r, offset, 0, c-offset),
                            nrow = 3,
                            ncol = 2,
                            byrow = TRUE)

M_aff = getAffineTransform(original_points = original_points,
                           transformed_points = transformed_points)
```

  

The following is the Affine transformation matrix,

``` r

print(M_aff)
```

    ##            [,1]        [,2] [,3]
    ## [1,] 0.91666667 -0.08333333   50
    ## [2,] 0.08333333  0.91666667    0

  

The Affine transformation matrix can be used as input in the
*warpAffine()* function,

``` r

res_3d = warpAffine(img = img,
                    M = M_aff,
                    R = r,
                    C = c,
                    verbose = TRUE)
```

    ## time to complete : 0.0357 secs

``` r

str(res_3d)
```

    ##  num [1:600, 1:600, 1:3] 0 0 0 0 0 0 0 0 0 0 ...

  

The next image shows the output based on the input data and parameters,

``` r

imageShow(res_3d, clear_viewer = FALSE)
```

![Output of Warp Affine
function](Warp_Affine_files/figure-html/unnamed-chunk-6-1.png)

Output of Warp Affine function

  
