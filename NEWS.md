
## OpenImageR 1.0.4

I modified the R script files to accept extension types similar to .tiff such as .tif, .TIFF or .TIF


## OpenImageR 1.0.3

I modified the Augmentation function to correct an issue which occured due to the zca_comps parameter


## OpenImageR 1.0.2

I did OpenMP optional for all cpp files due to the fact that the OSX OS does not currently support openMP ( r-devel-osx-x86_64-clang )


I removed the floor() function from the lines 45, 46 of the hog_features.cpp, because otherwise Solaris OS throws the following error:

* error: call of overloaded ‘floor(int)’ is ambiguous


I can not fix the following error in the hog_features.cpp file for the r-oldrel-windows platform as it has either to do with the RcppArmadillo version or is GCC specific,

* error: could not convert '{{-0x00000000000000001, 0, 1}, {-0x00000000000000002, 0, 2}, {-0x00000000000000001, 0, 1}}' from '<brace-enclosed initializer list>' to 'arma::mat {aka arma::Mat<double>}'

Platforms, which support newer versions of GCC (GCC >= 4.9.0) and a recent version of RcppArmadillo (>= 0.7)  do not throw such an error.


## OpenImageR 1.0.1

I modified the Makevars files, so that OpenImageR can be installed error-free in the following OS's,

* r-devel-osx-x86_64-clang
* r-patched-solaris-sparc
* r-patched-solaris-x86
* r-oldrel-windows-ix86+x86_64


## OpenImageR 1.0.0

