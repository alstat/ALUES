#' Overall Suitability Scores/Class of the Land Units
#' @export
#' 
#' @description
#' This function computes the overall suitability scores and class of the land units.
#' 
#' @param x a data frame consisting the suitability scores of a given characteristics
#'          (terrain, soil, water and temperature) for a 
#'          given crop (e.g. coconut, cassava, etc.);
#' @param method the method for computing the overall suitability, which includes the
#'        \code{"minimum"}, \code{"maximum"}, \code{"sum"}, \code{"product"}, and
#'        \code{"average"}.
#' @param interval if \code{NULL}, the interval used are the following: 0-25% (Not
#'        suitable, N), 25%-50% (Marginally Suitable, S3), 50%-75% (Moderately Suitable, S2), and
#'        75%-100% (Highly Suitable, S1). But users can assign a custom intervals by specificying
#'        the values of the end points of the intervals.
#' @param output the output to be returned, either the scores or class. If \code{NULL},
#'        both are returned.
#' 
#' @examples
#' library(ALUES)
#' x <- LaoCaiLT
#' y <- COCONUTSoilCR
#' 
#' coconut_tersuit <- suitability(x = x, y = y)
#' lapply(coconut_tersuit, function(x) head(x, n = 10))
#' 
#' head(overall_suit(coconut_tersuit[[2]]))
overall_suit <- function(x, method = NULL, 
                         interval = NULL, output = NULL) {
  x <- x[[2L]]
  if (!is.character(method)) { 
    stop("method should be character, please choose either: 'minimum', 'maximum', 'sum', 'product', 'average'.")
  }
  if (is.character(interval))
    stop("interval should be numeric if not NULL.")
  if (is.null(method))
    method <- "minimum"; suitScore <- apply(x, 1, min)
  if (method == "maximum")
    suitScore <- apply(x, 1, max)
  if (method == "sum")
    suitScore <- apply(x, 1, "sum"); suitScore <- suitScore / max(suitScore)
  if (method == "product")
    suitScore <- apply(x, 1, prod)
  if (method == "average")
    suitScore <- apply(x, 1, mean)
  
  if (is.null(interval)) {
    l1 = 0; l2 = 0.25; l3 = 0.5; l4 = 0.75; l5 = 1L;
  } else if (is.numeric(interval)) {
    if (length(interval) != 5L)
      stop("interval should have 5 limits, run ?landSuit for more.")
    else
      l1 = interval[1L]; l2 = interval[2L]; l3 = interval[3L]; l4 = interval[4L]; l5 = interval[5L]; bias <- 0L
  }
  
  sclassFun <- function (x) {
    if ((x >= l1) && (x < l2))
      return("N")
    if ((x >= l2) && (x < l3))
      return("S3")
    if ((x >= l3) && (x < l4))
      return("S2")
    if ((x >= l4) && (x <= l5))
      return("S1")    
  }

  suitClass <- sapply(suitScore, sclassFun)
  
  if (is.null(output))
    return(data.frame("Scores" = suitScore, "Class" = suitClass))
  if (!is.null(output) & output == "scores")
    return(data.frame("Scores" = suitScore))
  if (!is.null(output) & output == "class")
    return(data.frame("Class" = suitClass))
}