overall <- function(x, y, method = NULL, interval = NULL, output = NULL) {  
  if (ncol(x) == 1L) {
    return (data.frame(x, y[[3L]]))
  }
  
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