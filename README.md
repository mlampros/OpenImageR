
[![tic](https://github.com/mlampros/OpenImageR/workflows/tic/badge.svg?branch=master)](https://github.com/mlampros/OpenImageR/actions)
[![codecov.io](https://codecov.io/github/mlampros/OpenImageR/coverage.svg?branch=master)](https://codecov.io/github/mlampros/OpenImageR?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/OpenImageR)](http://cran.r-project.org/package=OpenImageR)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/OpenImageR?color=blue)](http://www.r-pkg.org/pkg/OpenImageR)
<a href="https://www.buymeacoffee.com/VY0x8snyh" target="_blank"><img src="https://www.buymeacoffee.com/assets/img/custom_images/orange_img.png" alt="Buy Me A Coffee" height="21px" ></a>
[![](https://img.shields.io/docker/automated/mlampros/openimager.svg)](https://hub.docker.com/r/mlampros/openimager)
[![Dependencies](https://tinyverse.netlify.com/badge/OpenImageR)](https://cran.r-project.org/package=OpenImageR)


## OpenImageR
<br>

The OpenImageR package is an image processing library. It includes functions for image preprocessing, filtering and image recognition. More details on the functionality of OpenImageR can be found in the [first](http://mlampros.github.io/2016/07/08/OpenImageR/), [second](http://mlampros.github.io/2018/08/08/Gabor_Feature_Extraction/) and [third](http://mlampros.github.io/2018/11/09/Image_Segmentation_Superpixels_Clustering/) blog-posts, and in the package Documentation  ( *scroll down for information on how to use the* **docker image** )
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
arma::mat rgb_2gray_exp(arma::cube RGB_image) {

  oimageR::Utility_functions UTLF;
  return UTLF.rgb_2gray_rcpp(RGB_image);
}


```

<br>

Then, by opening an R file a user can call the *rgb_2gray_exp* function using,

<br>

```R

Rcpp::sourceCpp('example.cpp')              # assuming that the previous Rcpp code is included in 'example.cpp' 
             
set.seed(1)
im_rgb = array(runif(30000), c(100, 100, 3))

im_grey = rgb_2gray_exp(im_rgb)

str(im_grey)

```

<br>

Use the following link to report bugs/issues,
<br><br>

[https://github.com/mlampros/OpenImageR/issues](https://github.com/mlampros/OpenImageR/issues)


<br>


**UPDATE 29-11-2019**

<br>

**Docker images** of the *OpenImageR* package are available to download from my [dockerhub](https://hub.docker.com/r/mlampros/openimager) account. The images come with *Rstudio* and the *R-development* version (latest) installed. The whole process was tested on Ubuntu 18.04. To **pull** & **run** the image do the following,

<br>

```R

docker pull mlampros/openimager:rstudiodev

docker run -d --name rstudio_dev -e USER=rstudio -e PASSWORD=give_here_your_password --rm -p 8787:8787 mlampros/openimager:rstudiodev

```

<br>

The user can also **bind** a home directory / folder to the image to use its files by specifying the **-v** command,

<br>

```R

docker run -d --name rstudio_dev -e USER=rstudio -e PASSWORD=give_here_your_password --rm -p 8787:8787 -v /home/YOUR_DIR:/home/rstudio/YOUR_DIR mlampros/openimager:rstudiodev


```

<br>

In the latter case you might have first give permission privileges for write access to **YOUR_DIR** directory (not necessarily) using,

<br>

```R

chmod -R 777 /home/YOUR_DIR


```

<br>

The **USER** defaults to *rstudio* but you have to give your **PASSWORD** of preference (see [www.rocker-project.org](https://www.rocker-project.org/) for more information).

<br>

Open your web-browser and depending where the docker image was *build / run* give, 

<br>

**1st. Option** on your personal computer,

<br>

```R
http://0.0.0.0:8787 

```

<br>

**2nd. Option** on a cloud instance, 

<br>

```R
http://Public DNS:8787

```

<br>

to access the Rstudio console in order to give your username and password.

<br>

### **Citation:**

If you use the code of this repository in your paper or research please cite both **OpenImageR** and the **original articles / software** `https://CRAN.R-project.org/package=OpenImageR`:

<br>

```R
@Manual{,
  title = {{OpenImageR}: An Image Processing Toolkit},
  author = {Lampros Mouselimis},
  year = {2022},
  note = {R package version 1.2.2},
  url = {https://CRAN.R-project.org/package=OpenImageR},
}
```

<br>

