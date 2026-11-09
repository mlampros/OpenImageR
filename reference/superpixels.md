# SLIC and SLICO superpixel implementations

SLIC and SLICO superpixel implementations

## Usage

``` r
superpixels(
  input_image,
  method = "slic",
  superpixel = 200,
  compactness = 20,
  return_slic_data = FALSE,
  return_lab_data = FALSE,
  return_labels = FALSE,
  write_slic = "",
  verbose = FALSE
)
```

## Arguments

- input_image:

  either a 2-dimensional or a 3-dimensional input image where the third
  dimension is equal to 3 (the range of the pixel values should be
  preferably in the range 0 to 255)

- method:

  a character string specifying the method to use. Either "slic" or
  "slico"

- superpixel:

  a numeric value specifying the number of superpixels to use

- compactness:

  a numeric value specifying the compactness parameter. The
  *compactness* parameter is needed only if *method* is "slic". The
  "slico" method adaptively chooses the compactness parameter for each
  superpixel differently.

- return_slic_data:

  a boolean. If TRUE then the resulted slic or slico data will be
  returned

- return_lab_data:

  a boolean. If TRUE then the Lab data will be returned ( the Lab-colour
  format )

- return_labels:

  a boolean. If TRUE then the labels will be returned

- write_slic:

  a character string. If not an empty string ("") then it should be a
  path to the output file with extension .bin ( for instance
  "/my_dir/output.bin" ). The data will be saved in binary format.

- verbose:

  a boolean. If TRUE then information will be printed in the R session

## References

https://www.epfl.ch/labs/ivrl/research/slic-superpixels/

## Examples

``` r

library(OpenImageR)

#-------------------
# 3-dimensional data
#-------------------

path = system.file("tmp_images", "slic_im.png", package = "OpenImageR")

im = readImage(path)

res = superpixels(input_image = im, method = "slic", superpixel = 200,

                  compactness = 20, return_slic_data = TRUE)
#> Warning: The input data has more than 3 dimensions. The dimensions were reduced from 4 to 3!
#> Warning: The input data has values between 0.094118 and 1.000000. The image-data will be multiplied by the value: 255!


#-------------------
# 2-dimensional data
#-------------------

im_2d = im[,,1]

res_mt = superpixels(input_image = im_2d, method = "slic", superpixel = 200,

                     compactness = 20, return_slic_data = TRUE)
#> Warning: The input data has values between 0.149020 and 1.000000. The image-data will be multiplied by the value: 255!
```
