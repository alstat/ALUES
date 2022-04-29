#' Overall Suitability Scores/Class of the Land Units
#' @export
#' 
#' @description
#' This function computes the overall suitability scores and class of the land units.
#' 
#' @param suit an object of class suitability.
#' @param method a character for the method for computing the overall suitability, choices are:
#'        \code{"minimum"}, \code{"maximum"}, and
#'        \code{"average"}. If \code{NULL}, method is set to \code{"minimum"}.
#' @param interval if \code{NULL}, the interval of the suitability class are the following: 0\% - 25\% (Not
#'        suitable, N), 25\% - 50\% (Marginally Suitable, S3), 50\% - 75\% (Moderately Suitable, S2), and
#'        75\% - 100\% (Highly Suitable, S1). But users can assign custom intervals by specifying
#'        the values of the end points of the intervals. Say for intervals: 0\% - 20\% (Not
#'        suitable, N), 20\% - 50\% (Marginally Suitable, S3), 50\% - 80\% (Moderately Suitable, S2), and
#'        80\% - 100\% (Highly Suitable, S1), is equivalent to \code{interval = c(0, 0.2, 0.5, 0.8, 1)}.
#'        
#' @return 
#' A data frame with columns:
#' \itemize{
#'  \item \code{Score} - the overall suitability scores
#'  \item \code{Class} - the overall suitability classes
#' }
#' 
#' @seealso
#' \code{https://alstat.github.io/ALUES/}; \code{\link{suit}}

#' 
#' @examples
#' # The overall suitability can be computed using the `overall_suit` function, which takes an object of class suitability. For example,
#' library(ALUES)
#' banana_suit <- suit("banana", terrain=MarinduqueLT)
#' class(banana_suit[["terrain"]])
#' class(banana_suit[["soil"]])
#' 
#' # If we take a look at the output of both terrain and soil characteristics, we have:
#' lapply(banana_suit[["terrain"]], function(x) head(x)) # lapply is used to show the head of each item in the list
#' lapply(banana_suit[["soil"]], function(x) head(x)) 
#' 
#' # There are no factors targetted for the terrain characteristics, that is why the returned value is a string error. Thus, only the soil characteristics can have an overall suitability, and is computed as follows:
#' ovsuit <- overall_suit(banana_suit[["soil"]])
#' head(ovsuit)
#' 
#' # By default, the `overall_suit` function uses minimum as a summary statistics, hence the 0 scores and N classes across land units. To adjust this to average aggregation, use the `method` argument to specify.
#' ovsuit <- overall_suit(banana_suit[["soil"]], method="average")
#' head(ovsuit)
#' 
#' ## Intervals
#' # By default, the `overall_suit` uses an equally spaced interval for the suitability classes, that is, N [0, 0.25), S3 [0.25, 0.50), S2 [0.50, 0.75), and S1 [0.75, 1]. This can be changed using the `interval` argument, for example
#' ovsuit <- overall_suit(banana_suit[["soil"]], method="average", interval=c(0, 0.6, 0.7, 0.9, 1))
#' head(ovsuit)
#' 
#' # The above code sets the suitability class intervals into: N [0, 0.60), S3 [0.60, 0.70), S2 [0.70, 0.90), and S1 [0.90, 1]. It should be emphasized that the `interval` argument cannot be set to `unbias` as in the case of the `interval` argument of the `suit` function. This follows from the fact that the `overall_suit` function does not use a membership function for computing the score, but an aggregation function.
#' 
#' # Other examples
#' library(ALUES)
#' out <- suit("ricebr", terrain=MarinduqueLT, water=MarinduqueWater, temp=MarinduqueTemp, sow_month=1)
#' lapply(out[["terrain"]], function(x) head(x))
#' lapply(out[["water"]], function(x) head(x))
#' 
#' # Soil Overall Suitability
#' head(overall_suit(out[["soil"]]))
#' head(overall_suit(out[["soil"]], "average"))
#' head(overall_suit(out[["soil"]], "maximum"))
#' head(overall_suit(out[["soil"]], "average", c(0, 0.3, 0.35, 0.6, 1.0)))
#' 
#' # Water Overall Suitability
#' head(overall_suit(out[["water"]], "average"))
#' head(overall_suit(out[["water"]], "maximum"))
#' head(overall_suit(out[["water"]], "average", c(0, 0.3, 0.35, 0.6, 1.0)))
#' 
#' # Temperature Overall Suitability
#' head(overall_suit(out[["temp"]], "average"))
#' head(overall_suit(out[["temp"]], "maximum"))
#' head(overall_suit(out[["temp"]], "average", c(0, 0.3, 0.35, 0.6, 1.0)))
overall_suit <- function(suit, method = NULL, interval = NULL) {
  if (class(suit) != "suitability") 
    stop("suit should be an object of class suitability.")
  
  if (ncol(suit[[2L]]) == 1L) {
    warning("No overall suitability computed since there is only one factor.")
    return(data.frame("Score" = suit[[2L]][,1], "Class" = suit[[3L]][,1]))
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
      } else if (interval[5] != 1) {
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
  
  return(data.frame("Score" = suitScore, "Class" = suitClass))
}