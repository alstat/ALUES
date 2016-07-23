About
=====
Agricultural Land Use Evaluation System (ALUES) is an R package that evaluates land suitability for
different crop production. The package is based on the Food and Agriculture Organization ([FAO](http://www.fao.org/home/en/)) and the
International Rice Research Institute ([IRRI](http://irri.org/)) methodology for land evaluation. Development of ALUES is
inspired by similar tool for land evaluation, Land Use Suitability Evaluation Tool (LUSET). The package
uses fuzzy logic approach to evaluate land suitability of a particular area based on inputs such as rainfall,
temperature, topography, and soil properties. The membership functions used for fuzzy modeling are the
following: Triangular, Trapezoidal and Gaussian. The methods for computing the overall suitability of a particular area are also included, and these are the Minimum, Maximum, Product, Sum, Average, Exponential and Gamma. Finally, ALUES uses the power of Rcpp library for efficient computation.

## Installation
The package is not yet on CRAN, and is currently under development on github. To install it, run the following:
```{coffee}
install.packages('devtools')

library(devtools)
install_github(repo = 'ALUES', username = 'alstat')
```
We want to hear some feedbacks, so if you have any suggestion or issues regarding this package, please do submit it [here](https://github.com/alstat/ALUES/issues).

## Authors
* [Al-Ahmadgaid B. Asaad](https://github.com/alstat) (Maintainer)
 * email: alstated@gmail.com
 * blog: http://alstatr.blogspot.com/
* [Arnold R. Salvacion](https://github.com/arsalvacion)
 * email: arsalvacion@gmail.com
 * blog: http://r-nold.blogspot.com/

