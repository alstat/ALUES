#' Suitability Scores/Class of the Land Units
#' @export
#' 
#' @description
#' This function calculates the suitability scores and class of the land units.
#' 
#' @param x a data frame consisting the properties of the land units;
#' @param y a data frame consisting the requirements of a given 
#'          characteristics (terrain, soil, water and temperature) for a 
#'          given crop (e.g. coconut, cassava, etc.);
#' @param mf membership function with default assigned to \code{"triangular"} 
#'           fuzzy model. Other fuzzy models included are \code{"trapezoidal"} and
#'           \code{"gaussian"}.
#' @param sow.month sowing month of the crop. Takes integers from 1 to 12 
#'                  (inclusive), representing the twelve months of a year. 
#'                  So if sets to 1, the function assumes sowing month on 
#'                  January.
#' @param min factor's minimum value. If \code{NULL} (default), \code{min} is
#'            set to 0. But if numeric of length one, say 0.5, then minimum 
#'            is set to 0.5, for all factors. If factors on land units 
#'            (\code{x}) have different minimum, then these can be concatenated
#'            to vector of \code{min}s, the length of this vector should be equal
#'            to the number of factors in \code{x}. However, if set to \code{"average"},
#'            then \code{min} is computed from different conditions:
#'            
#'            If for example, using \code{ALUES::COCONUTSoil} (coconut terrain requirements), 
#'            as shown below,
#'            
#'            \code{     code s3_a s2_a  s1_a s1_b s2_b s3_b          wts}\cr
#'            \code{1  CFragm 55.0 35.0  15.0   NA   NA   NA           NA}\cr
#'            \code{2 SoilDpt 50.0 75.0 100.0   NA   NA   NA           NA}\cr
#'            \code{3      BS 19.9 19.9  20.0   NA   NA   NA           NA}\cr
#'            \code{4  SumBCs  1.5  1.5   1.6   NA   NA   NA           NA}\cr
#'            \code{5      OC  0.7  0.7   0.8   NA   NA   NA           NA}\cr
#'            \code{6   ECemh 20.0 16.0  12.0   NA   NA   NA           NA}
#'            
#' @param max maximum value for factors. Default is to \code{"average"}, check on
#'              the details for this option. Assignment on maximum can also be done by
#'              simply entering any real numbers, say 55, then max is 55, we say this is homogeneous,
#'              since the maximum value for all factors then is set to 55. But for heterogeneous. For  \code{max}
#'              on every factor, simply concatenate the different \code{max} for each factor. 
#'              If set to \code{"average"}, check on details below for more.
#' @param interval domains for every suitability class (S1, S2, S3). If fixed, the
#'              interval would be 0 to 25\% for N (Not Suitable), 25\% to 50\% for S3 (Marginally Suitable),
#'              50\% to 75\% for S2 (Moderately Suitable), and 75\% to 100\% for (Highly Suitable).
#' @param sigma If \code{mf = "gaussian"}, then sigma represents the constant sigma in the
#'              gaussian formula, which is often times referred as the variance.
#' @details
#' There are four membership functions and these are triangular, trapezoidal, gaussian, and sigmoidal.
#' For triangular case. If a given factor has values equal for all suitabilities, then the class will 
#' trimmed down to N (not suitable) with domain [0, max), and S1 (highly suitable) with single tone domain \{0\}.
#' 
#' @examples
#' library(ALUES)
#' x <- LaoCaiLT
#' y <- COCONUTSoil
#' 
#' coconut_tersuit <- suitability(x = x, y = y)
#' lapply(coconut_tersuit, function(x) head(x, n = 10))
suitability <- function (x, y, mf = "triangular", sow.month = NULL, min = NULL, max = "average", interval = NULL, sigma = NULL) {
  n1 <- length(names(x))
  n2 <- nrow(y)
  f1 <- f2 <- numeric()
  
  if (is.numeric(sow.month)) {
    f3 <- f4 <- typ <- numeric()
    month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    
    wmav <- c("WmAv1", "WmAv2", "WmAv3", 
              "WmAv4", "WmAv5", "WmAv6")  
    
    tmav <- c("TmAv1", "TmAv2", "TmAv3", 
              "TmAv4", "TmAv5", "TmAv6")
    
    for (i in 1:nrow(y)) {
      for (j in 1:length(wmav)) {
        if (as.character(y[i, 1]) == wmav[j]) {
          f3[i] <- j; f4[i] <- i; typ <- 0
        } else if (as.character(y[i, 1]) == tmav[j]) {
          f3[i] <- j; f4[i] <- i; typ <- 1
        }
      }
    }
    
    f3 <- f3[stats::complete.cases(f3)]
    if (typ == 0) {
      idx <- as.numeric(unlist(strsplit(wmav[f3], "WmAv"))[2])
    } else {
      idx <- as.numeric(unlist(strsplit(tmav[f3], "TmAv"))[2]) 
    }
    
    if (idx > 1) {
      sow.month <- month[sow.month + idx - 1] 
    } else {
      sow.month <- month[sow.month]
    }
    
    y <- as.matrix(y)
    for (i in 1:12) {
      if (sow.month == month[i]) {
        if ((i + length(f3) - 1) > 12) {
          if (typ == 0) {
            y[y[,1] %in% wmav[f3], 1] <- c(rev(rev(month)[1:(length(f3) - ((i + length(f3) - 1) - 12))]), month[1:((i + length(f3) - 1) - 12)]) 
          } else {
            y[y[,1] %in% tmav[f3], 1] <- c(rev(rev(month)[1:(length(f3) - ((i + length(f3) - 1) - 12))]), month[1:((i + length(f3) - 1) - 12)]) 
          }
        } else {
          if (typ == 0) {
            y[y[,1] %in% wmav[f3], 1] <- month[i:(i + length(f3) - 1)]  
          } else {
            y[y[,1] %in% tmav[f3], 1] <- month[i:(i + length(f3) - 1)]  
          }
        }
        
      }
    }
    
    y <- as.data.frame(y)
  }
  
  # extract intersecting parameters between x and y
  for (i in 1:n2) {
    for (j in 1:n1) {
      if (as.character(y[i, 1]) == names(x)[j]) {
        f1[i] <- j; f2[i] <- i
      } 
    }
  }
  
  # update x and y to only intersecting parameters
  LU <- as.matrix(x[, f1[stats::complete.cases(f1)]])
  CR <- as.matrix(y[f2[stats::complete.cases(f1)], ])
  colnames(LU) <- names(x)[f1[stats::complete.cases(f1)]]
  
  if (ncol(LU) == 0) {
    warning("For water characteristic, make sure to input sowing month (sow.month), say 1, w/c implies January")
    stop("No factor(s) to be evaluated, since none matches with the crop requirements.")
  }
  
  # define empty matrix for score and class
  score <- matrix(NA, nrow = nrow(LU), ncol = ncol(LU))
  suiClass <- matrix(character(), nrow = nrow(LU), ncol = ncol(LU))
  colnames(score) <- colnames(LU)
  colnames(suiClass) <- colnames(LU)
  k <- 1
  
  if (is.null(interval)) {
    l1 = 0; l2 = 0.25; l3 = 0.5; l4 = 0.75; l5 = 1; bias <- 0
  } else if (is.numeric(interval) && !is.null(interval)) {
    if (length(interval) != 5) {
      stop("interval should have 5 limits, run ?suitability for more.") 
    } else {
      l1 = interval[1]; l2 = interval[2]; l3 = interval[3]; l4 = interval[4]; l5 = interval[5]; bias <- 0 
    }
  } else if (!is.null(interval) && interval == "unbias") {
    l1 = l2 = l3 = l4 = l5 = NA
    bias <- 1
  }
  
  if (mf == "triangular") {
    mfNum <- 1
  } else if (mf == "trapezoidal") {
    mfNum <- 2
  } else if (mf == "gaussian") {
    mfNum <- 3
  }
  
  if (is.null(sigma)) {
    sigma <- 1
  } else if (is.numeric(sigma)) {
    if (mf != "gaussian") {
      warning("sigma is only use for gaussian membership function. It defines the spread of the gaussian model.")
    } else {
      sigma <- sigma
    }
  }
  
  minVals <- maxVals <- numeric()
  for(j in 1:ncol(LU)){
    rScore <- rev(as.numeric(CR[k, -1][1:6]))
    reqScore <- rev(rScore[stats::complete.cases(rScore)])
    n3 <- length(reqScore)

    # if parameter has no entry, skip
    if (n3 == 0) {
      k <- k + 1
      next
    }
    
    if (n3 == 3) {
      if (reqScore[1] > reqScore[3]) {
        reqScore <- rev(reqScore)
        if ((!is.null(min)) && (min == "average")) {
          Min <- reqScore[1] - ((diff(reqScore[1:2]) + diff(reqScore[2:3])) / 2)
        } else if (is.numeric(min)){
          if (length(min) == 1) {
            Min <- min
          } else if (length(min) > 1) {
            if (length(min) == ncol(x)) {
              Min <- min[f1[stats::complete.cases(f1)][j]]
            } else if (length(min) != ncol(x)) {
              stop("min length should be equal to the number of factors in x.")
            }
          }
        } else if (is.null(min)) {
          Min <- 0
        }
        if (max == "average" && (!is.numeric(max))) {
          Max <- reqScore[3] + ((diff(reqScore[1:2]) + diff(reqScore[2:3])) / 2)
        } else if (is.numeric(max)) {
          if (length(max) == 1) {
            Max <- max
          } else if (length(max) > 1) {
            if (length(max) == ncol(x)) {
              Max <- max[f1[stats::complete.cases(f1)][j]] 
            } else if (length(max) != ncol(x)) {
              stop("max length should be equal to the number of factors in x.")
            }
          }            
        }
        output <- case_a(df = as.matrix(LU), score = score, suiClass = suiClass, Min = Min, Max = Max, mfNum = mfNum,
                         bias = bias, j = j, a = reqScore[1], b = reqScore[2], c = reqScore[3], l1 = l1, l2 = l2, l3 = l3, l4 = l4, l5 = l5, sigma = sigma)
        score <- output[[1]]; suiClass <- output[[2]]
        
      } else if (reqScore[1] < reqScore[3]) {
        if ((!is.null(min)) && (min == "average")) {
          Min <- reqScore[1] - ((diff(reqScore[1:2]) + diff(reqScore[2:3])) / 2)
        } else if (is.numeric(min)) {
          if (length(min) == 1) {
            Min <- min
          } else if (length(min) > 1) {
            if (length(min) == ncol(x)) {
              Min <- min[f1[stats::complete.cases(f1)][j]]
            } else if (length(min) != ncol(x)) {
              stop("min length should be equal to the number of factors in x.")
            }
          }
        } else if (is.null(min)) {
          Min <- 0
        }
        if (max == "average"  && (!is.numeric(max))) {
          Max <- reqScore[3] + ((diff(reqScore[1:2]) + diff(reqScore[2:3])) / 2)
        } else if (is.numeric(max)) {
          if (length(max) == 1) {
            Max <- max
          } else if (length(max) > 1) {
            if (length(max) == ncol(x)) {
              Max <- max[f1[stats::complete.cases(f1)][j]] 
            } else if (length(max) != ncol(x)) {
              stop("max length should be equal to the number of factors in x.")
            }
          }
        }
        output <- case_b(df = as.matrix(LU), score = score, suiClass = suiClass, Min = Min, Max = Max, mfNum = mfNum,
                         bias = bias, j = j, a = reqScore[1], b = reqScore[2], c = reqScore[3], l1 = l1, l2 = l2, l3 = l3, l4 = l4, l5 = l5, sigma = sigma)
        score <- output[[1]]; suiClass <- output[[2]]
      } else if ((reqScore[1] == reqScore[2]) &&
                   (reqScore[1] == reqScore[3]) &&
                   (reqScore[2] == reqScore[3])) {
        if ((!is.null(min)) && (min == "average")) {
          Min <- 0
          warning(paste("min is set to zero for factor", colnames(score)[j],
                        "since all suitability class intervals are equal."))
        } else if (is.numeric(min)) {
          if (length(min) == 1) {
            Min <- min
          } else if (length(min) > 1) {
            if (length(min) == ncol(x)) {
              Min <- min[f1[stats::complete.cases(f1)][j]]
            } else if (length(min) != ncol(x)) {
              stop("min length should be equal to the number of factors in x.")
            }
          }
        } else if (is.null(min)) {
          Min <- 0
        }
        Max <- reqScore[3]
        if (max == "average") {
          Max <- reqScore[3]
          warning(paste("max is set to", reqScore[3], "for factor", colnames(score)[j],
                        "since all suitability class intervals are equal."))
        } else if (is.numeric(max)) {
          if (length(max) == 1) {
            Max <- max
          } else if (length(max) > 1) {
            if (length(max) == ncol(x)) {
              Max <- max[f1[stats::complete.cases(f1)][j]] 
            } else if (length(max) != ncol(x)) {
              stop("max length should be equal to the number of factors in x.")
            }
          }            
        }
        output <- case_b(df = as.matrix(LU), score = score, suiClass = suiClass, Min = Min, Max = Max, mfNum = mfNum,
                         bias = bias, j = j, a = reqScore[1], b = reqScore[2], c = reqScore[3], l1 = l1, l2 = l2, l3 = l3, l4 = l4, l5 = l5, sigma = sigma)
        score <- output[[1]]; suiClass <- output[[2]]
      }
    } else if (n3 == 6) {
      if ((!is.null(min)) && (min == "average")) {
        Min <- reqScore[1] - ((diff(reqScore[1:2]) + diff(reqScore[2:3]) + diff(reqScore[3:4]) + diff(reqScore[4:5]) + diff(reqScore[5:6])) / 5)
      } else if (is.numeric(min)){
        if (length(min) == 1) {
          Min <- min
        } else if (length(min) > 1) {
          if (length(min) == ncol(x)) {
            Min <- min[f1[stats::complete.cases(f1)][j]]
          } else if (length(min) != ncol(x)) {
            stop("min length should be equal to the number of factors in x.")
          }
        }
      } else if (is.null(min)) {
        Min <- 0
      }
      Mid <- mean(reqScore[3:4])
      if (max == "average" && (!is.numeric(max))) {
        Max <- reqScore[6] + ((diff(reqScore[1:2]) + diff(reqScore[2:3]) + diff(reqScore[3:4]) + diff(reqScore[4:5]) + diff(reqScore[5:6])) / 5)
      } else if (is.numeric(max)) {
        if (length(max) == 1) {
          Max <- max
        } else if (length(max) > 1) {
          if (length(max) == ncol(x)) {
            Max <- max[f1[stats::complete.cases(f1)][j]] 
          } else if (length(max) != ncol(x)) {
            stop("max length should be equal to the number of factors in x.")
          }
        }            
      }
      output <- case_c(df = as.matrix(LU), score = score, suiClass = suiClass, Min = Min, Max = Max, Mid = Mid, mfNum = mfNum,
                       bias = bias, j = j, a = reqScore[1], b = reqScore[2], c = reqScore[3], d = reqScore[4], e = reqScore[5], f = reqScore[6], 
                       l1 = l1, l2 = l2, l3 = l3, l4 = l4, l5 = l5, sigma = sigma)
      score <- output[[1]]; suiClass <- output[[2]]
    } else if (n3 == 5) {
      if ((!is.null(min)) && (min == "average")) {
        Min <- reqScore[1] - ((diff(reqScore[1:2]) + diff(reqScore[2:3]) + diff(reqScore[3:4]) + diff(reqScore[4:5])) / 4)
      } else if (is.numeric(min)) {
        if (length(min) == 1) {
          Min <- min
        } else if (length(min) > 1) {
          if (length(min) == ncol(x)) {
            Min <- min[f1[stats::complete.cases(f1)][j]]
          } else if (length(min) != ncol(x)) {
            stop("min length should be equal to the number of factors in x.")
          }
        }
      } else if (is.null(min)) {
        Min <- 0
      }
      Mid <- mean(reqScore[3:4])
      if (max == "average" && (!is.numeric(max))) {
        Max <- reqScore[5]
        warning(paste("max is set to", reqScore[5], "for factor", colnames(score)[j],
                      "since there is a missing value on S3 class above optimum, run ?suitability for more."))
      } else if (is.numeric(max)) {
        if (length(max) == 1) {
          Max <- reqScore[5]
          warning(paste("max is set to", reqScore[5], "for factor", colnames(score)[j],
                        "since there is a missing value on S3 class above optimum, run ?suitability for more.")) 
        } else if (length(max) > 1) {
          if (length(max) == ncol(x)) {
            Max <- reqScore[5]
            warning(paste("max is set to", reqScore[5], "for factor", colnames(score)[j],
                          "since there is a missing value on S3 class above optimum, run ?suitability for more.")) 
          }
          else if (length(max) != ncol(x)) {
            stop("max length should be equal to the number of factors in x.")
          }
        }            
      }
      output <- case_d(df = as.matrix(LU), score = score, suiClass = suiClass, Min = Min, Max = Max, Mid = Mid, mfNum = mfNum,
                       bias = bias, j = j, a = reqScore[1], b = reqScore[2], c = reqScore[3], d = reqScore[4], l1 = l1, l2 = l2, l3 = l3, l4 = l4, l5 = l5, sigma = sigma)
      score <- output[[1]]; suiClass <- output[[2]]
    } else if (n3 == 4) {
      if ((!is.null(min)) && (min == "average")) {
        Min <- reqScore[1] - ((diff(reqScore[1:2]) + diff(reqScore[2:3]) + diff(reqScore[3:4])) / 3)
      } else if (is.numeric(min)){
        if (length(min) == 1) {
          Min <- min
        } else if (length(min) > 1) {
          if (length(min) == ncol(x)) {
            Min <- min[f1[stats::complete.cases(f1)][j]]
          } else if (length(min) != ncol(x)) {
            stop("min length should be equal to the number of factors in x.")
          }
        }
      } else if (is.null(min)) {
        Min <- 0
      }
      Mid <- mean(reqScore[3:4])
      if (max == "average" && (!is.numeric(max))) {
        Max <- reqScore[4]
        warning(paste("max is set to", reqScore[4], "for factor", colnames(score)[j],
                      "since there is a missing value on S3 class above optimum, run ?suitability for more."))
      } else if (is.numeric(max)) {
        if (length(max) == 1) {
          Max <- reqScore[4]
          warning(paste("max is set to", reqScore[4], "for factor", colnames(score)[j],
                        "since there is a missing value on S3 class above optimum, run ?suitability for more.")) 
        }
        else if (length(max) > 1) {
          if (length(max) == ncol(x)) {
            Max <- reqScore[4]
            warning(paste("max is set to", reqScore[4], "for factor", colnames(score)[j],
                          "since there is a missing value on S3 class above optimum, run ?suitability for more.")) 
          }
          else if (length(max) != ncol(x))
            stop("max length should be equal to the number of factors in x.")
        }            
      }
      output <- case_e(df = as.matrix(LU), score = score, suiClass = suiClass, Min = Min, Max = Max, Mid = Mid, mfNum = mfNum,
                       bias = bias, j = j, a = reqScore[1], b = reqScore[2], c = reqScore[3], l1 = l1, l2 = l2, l3 = l3, l4 = l4, l5 = l5, sigma = sigma)
      score <- output[[1]]; suiClass <- output[[2]]
    }
    k <- k + 1
    minVals[j] <- Min; maxVals[j] <- Max
  }
  names(minVals) <- names(maxVals) <- names(x)[f1[stats::complete.cases(f1)]]
  
  outf <- list("Actual Factors Evaluated" = names(minVals), 
               "Suitability Score" = as.data.frame(score), 
               "Suitability Class" = as.data.frame(suiClass), 
               "Factors' Minimum Values" = minVals, 
               "Factors' Maximum Values" = maxVals)
  class(outf) <- "suitability"
  return(outf)
}