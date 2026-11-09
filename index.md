## OpenImageR

  

The OpenImageR package is an image processing library. It includes
functions for image preprocessing, filtering and image recognition. More
details on the functionality of OpenImageR can be found in the
[first](http://mlampros.github.io/2016/07/08/OpenImageR/),
[second](http://mlampros.github.io/2018/08/08/Gabor_Feature_Extraction/)
and
[third](http://mlampros.github.io/2018/11/09/Image_Segmentation_Superpixels_Clustering/)
blog-posts, and in the package Documentation ( *scroll down for
information on how to use the* **docker image** )  
  

**UPDATE 06-11-2018**

As of version 1.1.2 the *OpenImageR* package allows R package
maintainers to perform **linking between packages at a C++ code (Rcpp)
level**. This means that the Rcpp functions of the *OpenImageR* package
can be called in the C++ files of another package. In the next lines
I’ll give detailed explanations on how this can be done:

  

Assumming that an R package (‘PackageA’) calls one of the *OpenImageR*
Rcpp functions. Then the maintainer of ‘PackageA’ has to :

  

- **1st.** install the *OpenImageR* package to take advantage of the new
  functionality either from CRAN using,

  

``` r


install.packages("OpenImageR")
 
```

  

or download the latest version from Github using the *remotes* package,

  

``` r


remotes::install_github('mlampros/OpenImageR')
 
```

  

- **2nd.** update the **DESCRIPTION** file of ‘PackageA’ and especially
  the *LinkingTo* field by adding the *OpenImageR* package (besides any
  other packages),

  

``` r


LinkingTo: OpenImageR
```

  

- **3rd.** open a **new C++ file** (for instance in Rstudio) and at the
  top of the file add the following ‘headers’, ‘depends’ and ‘plugins’,

  

``` r

# include <RcppArmadillo.h>
# include <OpenImageRheader.h>
// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::depends(OpenImageR)]]
```

  

The available C++ classes (*Utility_functions*, *Gabor_Features*,
*Gabor_Features_Rcpp*, *HoG_features*, *Image_Hashing*) can be found in
the **inst/include/OpenImageRheader.h** file.

  

A *complete minimal example* would be :

  

``` r

# include <RcppArmadillo.h>
# include <OpenImageRheader.h>
// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::depends(OpenImageR)]]


// [[Rcpp::export]]
arma::mat rgb_2gray_exp(arma::cube RGB_image) {

  oimageR::Utility_functions UTLF;
  return UTLF.rgb_2gray_rcpp(RGB_image);
}
```

  

Then, by opening an R file a user can call the *rgb_2gray_exp* function
using,

  

``` r


Rcpp::sourceCpp('example.cpp')              # assuming that the previous Rcpp code is included in 'example.cpp' 
             
set.seed(1)
im_rgb = array(runif(30000), c(100, 100, 3))

im_grey = rgb_2gray_exp(im_rgb)

str(im_grey)
```

  

Use the following link to report bugs/issues,  
  

<https://github.com/mlampros/OpenImageR/issues>

  

**UPDATE 29-11-2019**

  

**Docker images** of the *OpenImageR* package are available to download
from my [dockerhub](https://hub.docker.com/r/mlampros/openimager)
account. The images come with *Rstudio* and the *R-development* version
(latest) installed. The whole process was tested on Ubuntu 18.04. To
**pull** & **run** the image do the following,

  

``` r

docker pull mlampros/openimager:rstudiodev

docker run -d --name rstudio_dev -e USER=rstudio -e PASSWORD=give_here_your_password --rm -p 8787:8787 mlampros/openimager:rstudiodev
```

  

The user can also **bind** a home directory / folder to the image to use
its files by specifying the **-v** command,

  

``` r

docker run -d --name rstudio_dev -e USER=rstudio -e PASSWORD=give_here_your_password --rm -p 8787:8787 -v /home/YOUR_DIR:/home/rstudio/YOUR_DIR mlampros/openimager:rstudiodev
```

  

In the latter case you might have first give permission privileges for
write access to **YOUR_DIR** directory (not necessarily) using,

  

``` r

chmod -R 777 /home/YOUR_DIR
```

  

The **USER** defaults to *rstudio* but you have to give your
**PASSWORD** of preference (see <https://rocker-project.org/> for more
information).

  

Open your web-browser and depending where the docker image was *build /
run* give,

  

**1st. Option** on your personal computer,

  

``` r
http://0.0.0.0:8787 
```

  

**2nd. Option** on a cloud instance,

  

``` r
http://Public DNS:8787
```

  

to access the Rstudio console in order to give your username and
password.

  

### **Citation:**

If you use the code of this repository in your paper or research please
cite both **OpenImageR** and the **original articles / software**
`https://CRAN.R-project.org/package=OpenImageR`:

  

``` r
@Manual{,
  title = {{OpenImageR}: An Image Processing Toolkit},
  author = {Lampros Mouselimis},
  year = {2026},
  note = {R package version 1.4.0},
  url = {https://CRAN.R-project.org/package=OpenImageR},
}
```

  
