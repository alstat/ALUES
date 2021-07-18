#' Overall Suitability Scores/Class of the Land Units
#' @export
#' 
#' @description
#' This function computes the overall suitability scores and class of the land units.
#' 
#' @param x an object of class suitability.
#' @param method the method for computing the overall suitability, choices are:
#'        \code{"minimum"}, \code{"maximum"}, \code{"sum"}, \code{"product"}, and
#'        \code{"average"}. If \code{NULL}, method is set to \code{"minimum"}.
#' @param interval if \code{NULL}, the interval of the suitability class are the following: 0\% - 25\% (Not
#'        suitable, N), 25\% - 50\% (Marginally Suitable, S3), 50\% - 75\% (Moderately Suitable, S2), and
#'        75\% - 100\% (Highly Suitable, S1). But users can assign custom intervals by specifying
#'        the values of the end points of the intervals. Say for intervals: 0\% - 20\% (Not
#'        suitable, N), 20\% - 50\% (Marginally Suitable, S3), 50\% - 80\% (Moderately Suitable, S2), and
#'        80\% - 100\% (Highly Suitable, S1), is equivalent to \code{interval = c(0, 0.2, 0.5, 0.8, 1)}.
#' @param output the output to be returned, either \code{"scores"} or \code{"class"}. If \code{NULL},
#'        both are returned.
#'        
#' @seealso
#' \code{\link{suitability}}
#' 
#' @examples
#' library(ALUES)
#' x <- LaoCaiLT
#' y <- COCONUTSoil
#' 
#' # Compute the suitability of the land units of the Lao Cai land
#' # terrain characteristics using the 
#' coconut_tersuit <- suitability(x = x, y = y)
#' 
#' # Return the first 10 of the observations
#' lapply(coconut_tersuit, function(x) head(x, n = 10))
#' 
#' # Compute the overall suitability of the characteristics
#' head(overall_suit(coconut_tersuit))
overall_suit <- function(x, method = NULL, interval = NULL, output = NULL) {
  if (class(x) != "suitability") 
    stop("x should be an object of class suitability.")
  
  if (ncol(x[[2L]]) == 1L) {
    return (data.frame(x[[2L]], x[[3L]]))
  }
  
  x <- x[[2L]]
  if (!is.character(method) && !is.null(method)) { 
    stop("method should be character, please choose either: 'minimum', 'maximum', 'sum', 'product', 'average'.")
  }
  if (is.character(interval))
    stop("interval should be numeric if not NULL.")
  if (is.null(method) || method == "minimum")
    suitScore <- apply(x, 1L, min)
  if (!is.null(method) && method == "maximum")
    suitScore <- apply(x, 1L, max)
  if (!is.null(method) && method == "sum") {
    suitScore <- apply(x, 1L, sum); suitScore <- suitScore / max(suitScore)
  }
  if (!is.null(method) && method == "product")
    suitScore <- apply(x, 1L, prod)
  if (!is.null(method) && method == "average")
    suitScore <- apply(x, 1L, mean)
  
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