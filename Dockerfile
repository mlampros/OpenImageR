FROM rocker/rstudio:devel 

 
LABEL maintainer='Lampros Mouselimis' 

 
RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update && \ 
 apt-get install -y libfftw3-dev libpng-dev libssl-dev pandoc pandoc-citeproc make libcurl4-openssl-dev && \ 
 apt-get install -y sudo && \ 
 apt-get install -y libarmadillo-dev && \ 
 apt-get install -y libblas-dev && \ 
 apt-get install -y liblapack-dev && \ 
 apt-get install -y libarpack++2-dev && \ 
 apt-get install -y gfortran && \ 
 apt-get install -y libjpeg-dev && \ 
 apt-get install -y libpng-dev && \ 
 apt-get install -y libfftw3-dev && \ 
 apt-get install -y libtiff5-dev  && \ 
 apt-get install -y libxml2-dev libssh2-1-dev zlib1g-dev git-core && \ 
 R -e "install.packages('devtools', dependencies = TRUE, repos = 'https://cloud.r-project.org/')" && \ 
 R -e "install.packages(c( 'Rcpp', 'graphics', 'grid', 'shiny', 'jpeg', 'png', 'tiff', 'R6', 'RcppArmadillo', 'testthat', 'knitr', 'rmarkdown', 'covr', 'remotes' ), repos =  'https://cloud.r-project.org/' )" && \ 
 R -e "remotes::install_github('mlampros/OpenImageR', upgrade = 'never', dependencies = FALSE, repos = 'https://cloud.r-project.org/')" && \ 
 apt-get autoremove -y && \ 
 apt-get clean 

 
ENV USER rstudio 

 
