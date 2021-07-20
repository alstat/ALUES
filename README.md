<!-- badges: start -->
  [![R-CMD-check](https://github.com/alstat/ALUES/workflows/R-CMD-check/badge.svg)](https://github.com/alstat/ALUES/actions)
[![R-CMD-check](https://github.com/alstat/ALUES/workflows/R-CMD-check/badge.svg)](https://github.com/alstat/ALUES/actions)
<!-- badges: end -->
  
About
=====
Agricultural Land Use Evaluation System (ALUES) is an R package that evaluates land suitability for
different crop production. The package is based on the Food and Agriculture Organization ([FAO](http://www.fao.org/home/en/)) and the
International Rice Research Institute ([IRRI](http://irri.org/)) methodology for land evaluation. Development of ALUES is
inspired by similar tool for land evaluation, Land Use Suitability Evaluation Tool (LUSET). The package
uses fuzzy logic approach to evaluate land suitability of a particular area based on inputs such as rainfall,
temperature, topography, and soil properties. The membership functions used for fuzzy modeling are the
following: _Triangular_, _Trapezoidal_ and _Gaussian_. The methods for computing the overall suitability of a particular area are also included, and these are the _Minimum_, _Maximum_, _Product_, _Sum_, _Average_, _Exponential_ and _Gamma_. Finally, ALUES is highly optimized with core algorithm written in C++.

## Installation
The package is not yet on CRAN, and is currently under development on github. To install it, run the following:
```{r}
install.packages("devtools")

library(devtools)
install_github("alstat/ALUES")
```
We want to hear some feedbacks, so if you have any suggestion or issues regarding this package, please do submit it [here](https://github.com/alstat/ALUES/issues).

## Authors
1. [Al-Ahmadgaid B. Asaad](https://github.com/alstat) (Maintainer)
    * email: alahmadgaid@gmail.com
    * website: https://al-asaad.github.io/
<br><br>

2. [Arnold R. Salvacion](https://github.com/arsalvacion)
    * email: arsalvacion@gmail.com
    * blog: http://r-nold.blogspot.com/

