[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/OpenImageR)](http://cran.r-project.org/package=OpenImageR)
[![Travis-CI Build Status](https://travis-ci.org/mlampros/OpenImageR.svg?branch=master)](https://travis-ci.org/mlampros/OpenImageR)
[![codecov.io](https://codecov.io/github/mlampros/OpenImageR/coverage.svg?branch=master)](https://codecov.io/github/mlampros/OpenImageR?branch=master)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/mlampros/OpenImageR?branch=master&svg=true)](https://ci.appveyor.com/project/mlampros/OpenImageR/branch/master)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/OpenImageR?color=blue)](http://www.r-pkg.org/pkg/OpenImageR)



## OpenImageR
<br>

The OpenImageR package is an image processing library. It includes functions for image preprocessing, filtering and image recognition. More details on the functionality of OpenImageR can be found in the [first](http://mlampros.github.io/2016/07/08/OpenImageR/), [second](http://mlampros.github.io/2018/08/08/Gabor_Feature_Extraction/) and [third](http://mlampros.github.io/2018/11/09/Image_Segmentation_Superpixels_Clustering/) blog-posts, and in the package Documentation. 
<br><br>


**UPDATE 06-11-2018**

As of version 1.1.2 the *OpenImageR* package allows R package maintainers to perform **linking between packages at a C++ code (Rcpp) level**. This means that the Rcpp functions of the *OpenImageR* package can be called in the C++ files of another package. In the next lines I'll give detailed explanations on how this can be done:

<br>

Assumming that an R package ('PackageA') calls one of the *OpenImageR* Rcpp functions. Then the maintainer of 'PackageA' has to :

<br>

* **1st.** install the *OpenImageR* package to take advantage of the new functionality either from CRAN using,

<br>


```R

install.packages("OpenImageR")
 

```

<br>

or download the latest version from Github using the *devtools* package,

<br>

```R

devtools::install_github('mlampros/OpenImageR')
 

```

<br>

* **2nd.** update the **DESCRIPTION** file of 'PackageA' and especially the *LinkingTo* field by adding the *OpenImageR* package (besides any other packages),

<br>

```R

LinkingTo: OpenImageR

```

<br>

* **3rd.** open a **new C++ file** (for instance in Rstudio) and at the top of the file add the following 'headers', 'depends' and 'plugins',

<br>

```R

# include <RcppArmadillo.h>
# include <OpenImageRheader.h>
// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::depends(OpenImageR)]]
// [[Rcpp::plugins(cpp11)]]


```
<br>

The available C++ classes (*Utility_functions*, *Gabor_Features*, *Gabor_Features_Rcpp*, *HoG_features*, *Image_Hashing*) can be found in the **inst/include/OpenImageRheader.h** file.

<br>

A *complete minimal example* would be :

<br>

```R

# include <RcppArmadillo.h>
# include <OpenImageRheader.h>
// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::depends(OpenImageR)]]
// [[Rcpp::plugins(cpp11)]]


// [[Rcpp::export]]
arma::mat rgb_2gray(arma::cube RGB_image) {

  oimageR::Utility_functions UTLF;
  return UTLF.rgb_2gray(RGB_image);
}


```

<br>

Then, by opening an R file a user can call the *rgb_2gray* function using,

<br>

```R

Rcpp::sourceCpp('example.cpp')              # assuming that the previous Rcpp code is included in 'example.cpp' 
             
set.seed(1)
im_rgb = array(runif(30000), c(100, 100, 3))

im_grey = rgb_2gray(im_rgb)

str(im_grey)

```

<br>

Use the following link to report bugs/issues,
<br><br>

[https://github.com/mlampros/OpenImageR/issues](https://github.com/mlampros/OpenImageR/issues)
