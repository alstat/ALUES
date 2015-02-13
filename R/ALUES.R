#' Agricultural Land Use Evaluation System
#'
#' Agricultural Land Use Evaluation System (ALUES) is 
#' an R package that evaluates land suitability for
#' different crop production. The package is based on 
#' the Food and Agriculture Organization (FAO) and the 
#' International Rice Research Institute (IRRI) methodology 
#' for land evaluation. Development of ALUES is inspired by 
#' similar tool for land evaluation, Land Use Suitability 
#' Evaluation Tool (LUSET). The package uses fuzzy logic 
#' approach to evaluate land suitability of a particular
#' area based on inputs such as rainfall,
#' temperature, topography, and soil properties. The 
#' membership functions used for fuzzy modeling are the
#' following: Triangular, Trapezoidal, Gaussian, Sigmoidal 
#' and custom models with functions that can be
#' defined by the user. The package also aims on complicated 
#' methods like considering more than one fuzzy membership 
#' function on different suitability class. The methods for 
#' computing the overall suitability of a particular area are 
#' also included, and these are the Minimum, Maximum, Product, 
#' Sum, Average, Exponential and Gamma. Finally, ALUES utilizes 
#' the power of Rcpp library for efficient computation.
#' 
#' @author 
#' Arnold Salvacion <arsalvacion@@gmail.com>
#' 
#' Al-Ahmadgaid Bahauddin Asaad <alstated@@gmail.com>
#' 
#' @import Rcpp ggmap reshape2
#' @docType package
#' @useDynLib ALUES
#' @name ALUES-package
NULL