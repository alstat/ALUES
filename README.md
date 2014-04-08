About
=====
Agricultural Land Use Evaluation System (ALUES) is an R package that evaluates land suitability for
different crop production. The package is based on the Food and Agriculture Organization ([FAO](http://www.fao.org/home/en/)) and the
International Rice Research Institute ([IRRI](http://irri.org/)) methodology for land evaluation. Development of ALUES is
inspired by similar tool for land evaluation, Land Use Suitability Evaluation Tool (LUSET). The package
uses fuzzy logic approach to evaluate land suitability of a particular area based on inputs such as rainfall,
temperature, topography, and soil properties. The membership functions used for fuzzy modeling are the
following: Triangular, Trapezoidal, Gaussian, Sigmoidal and custom models with functions that can be
defined by the user. The package also aims on complicated methods like considering more than one fuzzy
membership function on different suitability class. The methods for computing the overall suitability of a
particular area are also included, and these are the Minimum, Maximum, Product, Sum, Average,
Exponential and Gamma. Finally, ALUES utilizes the power of Rcpp library for efficient computation.

## Installation
This package is currently under development here on github and not yet on CRAN, to install this run the following:
```{coffee}
library(devtools)
install_github(repo = 'ALUES', username = 'alstat')
```
The development was done in Ubuntu Linux operating system, and the above codes works fine; but if you are using other platforms (Windows or Mac), do let us know if there are issues by submitting [here](https://github.com/alstat/LUSET/issues).

## Authors
* [Arnold R. Salvacion](https://github.com/arsalvacion)
 * blog: http://r-nold.blogspot.com/
* [Al-Ahmadgaid B. Asaad](https://github.com/alstat) (Maintainer)
 * email: alstated@gmail.com
 * blog: http://alstatr.blogspot.com/

---
**CAUTION**: This package is under development.
