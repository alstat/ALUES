#' Overall Suitability Scores/Class of the Land Units
#' @export
#' 
#' @description
#' This function computes the overall suitability scores and class of the land units.
#' 
#' @param suit an object of class suitability.
#' @param method the method for computing the overall suitability, choices are:
#'        \code{"minimum"}, \code{"maximum"}, \code{"sum"}, \code{"product"}, and
#'        \code{"average"}. If \code{NULL}, method is set to \code{"minimum"}.
#' @param interval if \code{NULL}, the interval of the suitability class are the following: 0\% - 25\% (Not
#'        suitable, N), 25\% - 50\% (Marginally Suitable, S3), 50\% - 75\% (Moderately Suitable, S2), and
#'        75\% - 100\% (Highly Suitable, S1). But users can assign custom intervals by specifying
#'        the values of the end points of the intervals. Say for intervals: 0\% - 20\% (Not
#'        suitable, N), 20\% - 50\% (Marginally Suitable, S3), 50\% - 80\% (Moderately Suitable, S2), and
#'        80\% - 100\% (Highly Suitable, S1), is equivalent to \code{interval = c(0, 0.2, 0.5, 0.8, 1)}.
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
overall_suit <- function(suit, method = NULL, interval = NULL) {
  if (class(suit) != "suitability") 
    stop("suit should be an object of class suitability.")
  
  if (ncol(suit[[2L]]) == 1L) {
    return (data.frame(suit[[2L]], suit[[3L]]))
  }
  
  x <- suit[[2L]]; wts <- suit[[6L]]
  if (!is.character(method) && !is.null(method)) { 
    stop("method should be character, please choose either: 'minimum', 'maximum', 'sum', 'product', 'average'.")
  }
  if (is.character(interval)) {
    stop("interval should be numeric if not NULL.")
  }
  
  if (sum(is.na(wts)) != length(wts)) {
    wts[is.na(wts)] <- max(wts, na.rm = TRUE) + 1
    new_wts <- (sum(wts) - wts)
    new_wts <- new_wts/sum(new_wts)
  }
  
  if (is.null(method) || method == "minimum") {
    suitScore <- apply(x, 1L, function(x) min(as.numeric(x), na.rm = TRUE))
  } else if (!is.null(method) && method == "maximum") {
    suitScore <- apply(x, 1L, function(x) max(as.numeric(x), na.rm = TRUE))
  } else if (!is.null(method) && method == "average") {
    if (sum(is.na(wts)) == length(wts)) {
      suitScore <- apply(x, 1L, function(x) mean(as.numeric(x), na.rm = TRUE))  
    } else {
      suitScore <- apply(x, 1L, function (x) sum(as.numeric(x) * new_wts, na.rm = TRUE))
    }
  } else {
    stop("method available are 'minimum', 'maximum' and 'average'.")
  } 
  
  if (is.null(interval)) {
    l1 = 0; l2 = 0.25; l3 = 0.5; l4 = 0.75; l5 = 1L;
  } else if (is.numeric(interval)) {
    if (length(interval) != 5L) {
      stop("interval should have 5 limits in ascending order from 0 to 1.")
    } else {
      if (interval[1] != 0) {
        stop("minimum limit should be 0.")
      } else if (interval[1] != 1) {
        stop("maximum limit should be 1.")
      } else {
        l1 = interval[1L]; l2 = interval[2L]; l3 = interval[3L]; l4 = interval[4L]; l5 = interval[5L] 
      }
    }
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
  
  return(data.frame("Scores" = suitScore, "Class" = suitClass))
}