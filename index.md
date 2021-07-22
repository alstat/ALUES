# Agricultural Land Use Evaluation System 
<!-- badges: start -->
[![R-CMD-check](https://github.com/alstat/ALUES/workflows/R-CMD-check/badge.svg)](https://github.com/alstat/ALUES/actions)
[![codecov](https://codecov.io/gh/alstat/ALUES/branch/master/graph/badge.svg?token=UE1J3JZK48)](https://codecov.io/gh/alstat/ALUES)
[![](https://img.shields.io/badge/docs-dev-blue.svg)](https://alstat.github.io/ALUES)
[![MIT License](https://img.shields.io/badge/license-MIT-green.svg)](https://github.com/alstat/ALUES/blob/master/LICENSE.md)
<!-- badges: end -->

An R package that evaluates land suitability for
different crops production. The package is based on the Food and Agriculture Organization ([FAO](http://www.fao.org/home/en/)) and the
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
<br>
<br>
<br>
<img src="logo.svg" align="left" width="100"/>