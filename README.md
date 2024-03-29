# ALUES <img src="logo.svg" align="right" width="100"/>

<!-- badges: start -->
[![R-CMD-check](https://github.com/alstat/ALUES/workflows/R-CMD-check/badge.svg)](https://github.com/alstat/ALUES/actions)
[![codecov](https://codecov.io/gh/alstat/ALUES/branch/master/graph/badge.svg?token=UE1J3JZK48)](https://app.codecov.io/gh/alstat/ALUES/)
[![](https://img.shields.io/badge/docs-dev-blue.svg)](https://alstat.github.io/ALUES/)
[![MIT License](https://img.shields.io/badge/license-MIT-green.svg)](https://github.com/alstat/ALUES/blob/master/LICENSE.md)
[![DOI](https://joss.theoj.org/papers/10.21105/joss.04228/status.svg)](https://doi.org/10.21105/joss.04228)
<!-- badges: end -->

Agricultural Land Use Evaluation System (ALUES) is an R package that evaluates land suitability for
different crops production. The package is based on the Food and Agriculture Organization ([FAO](https://www.fao.org/home/en/)) and the
International Rice Research Institute ([IRRI](https://www.irri.org/)) methodology for land evaluation. Development of ALUES is inspired by similar tool for land evaluation, Land Use Suitability Evaluation Tool (LUSET). The package
uses fuzzy logic approach to evaluate land suitability of a particular area based on inputs such as rainfall,
temperature, topography, and soil properties. The membership functions used for fuzzy modeling are the
following: _Triangular_, _Trapezoidal_ and _Gaussian_. The methods for computing the overall suitability of a particular area are also included, and these are the _Minimum_, _Maximum_ and _Average_. Finally, ALUES is a highly optimized library with core algorithms written in C++.

## Statement of Need
Several computer systems have been developed for agricultural land suitability assessments. Examples of these are ALES ([Johnson & Cramb, 1991](https://doi.org/10.1111/j.1475-2743.1991.tb00881.x)), LEIGIS ([Kalogirou, 2002](https://doi.org/10.1016/S0198-9715(01)00031-X)) and ALSE ([Elsheikh et al., 2013](https://doi.org/10.1016/j.compag.2013.02.003)). However, most of these systems are either proprietary or lacking features for land suitability assessments. It is therefore the goal of this software, ALUES, to address some of the limitations of the aforementioned software.

## Installation
The package is available on CRAN, and can be installed as follows:

```{r}
install.packages("ALUES")
```
To install the development version, run the following instead:
```{r}
install.packages("devtools")

library(devtools)
install_github("alstat/ALUES")
```
We want to hear some feedbacks, so if you have any suggestion or issues regarding this package, please do submit it [here](https://github.com/alstat/ALUES/issues/). As for those who would want to contribute please read the [CONTRIBUTING.md](https://github.com/alstat/ALUES/blob/master/CONTRIBUTING.md) file.

## Citation
```
@article{Asaad2022,
  doi = {10.21105/joss.04228},
  url = {https://doi.org/10.21105/joss.04228},
  year = {2022},
  publisher = {The Open Journal},
  volume = {7},
  number = {73},
  pages = {4228},
  author = {Al-Ahmadgaid B. Asaad and Arnold R. Salvacion and Bui Tan Yen},
  title = {ALUES: R package for Agricultural Land Use Evaluation System},
  journal = {Journal of Open Source Software}
}
```

## Authors (with Contribution)
1. [Al-Ahmadgaid B. Asaad](https://github.com/alstat/) (Maintainer)
    * email: alahmadgaid@gmail.com
    * website: https://al-asaad.github.io/
    * role: main developer of the package, responsible for the mathematical theory
<br><br>
2. [Arnold R. Salvacion](https://github.com/arsalvacion/)
    * email: arsalvacion@gmail.com
    * blog: http://r-nold.blogspot.com/
    * role: supervisor and domain expert on agriculture, and provider of all the necessary articles 
<br><br>
3. Bui Tan Yen
    * role: LUSET main developer, which is the basis of ALUES
    


