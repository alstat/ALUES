#' Suitability Scores/Class of the Land Units
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
#' @param sow_month sowing month of the crop. Takes integers from 1 to 12 
#'                  (inclusive), representing the twelve months of the year. 
#'                  So if sets to 1, the function assumes sowing month to be 
#'                  January.
#' @param minimum factor's minimum value. If \code{NULL} (default), \code{minimum} is
#'            set to 0. But if numeric of length one, say 0.5, then minimum 
#'            is set to 0.5, for all factors. To set multiple minimums for multiple factors,
#'            simply concatenate these into a numeric vector, the length of this vector should be equal
#'            to the number of factors in input land units parameters. However, it can also be set to
#'            \code{"average"}, please refer to the online documentation for more, link in the "See Also" section below.
#'            
#' @param maximum maximum value for factors. To set multiple maximums for multiple factors,
#'            simply concatenate these into a numeric vector, the length of this vector should be equal
#'            to the number of factors in input land units parameters. However, it can also be set to
#'            \code{"average"}, please refer to the online documentation for more, link in the "See Also" section below.
#'            
#' @param interval domains for every suitability class (S1, S2, S3). If fixed (\code{NULL}), the
#'              interval would be 0 to 25\% for N (Not Suitable), 25\% to 50\% for S3 (Marginally Suitable),
#'              50\% to 75\% for S2 (Moderately Suitable), and 75\% to 100\% for (Highly Suitable). If \code{"unbias"},
#'              the package will take into account the shape of the membership function, and provide the 
#'              appropriate suitability class intervals. However, it can also be customized by specifying the 
#'              limits of the suitability classes. Please refer to the online documentation for more, link in the "See Also" section below.
#' @param sigma If \code{mf = "gaussian"}, then sigma represents the constant sigma in the
#'              Gaussian formula.
#'                
#' @return 
#' A list with the following components:
#' \itemize{
#' \item \code{"Factors Evaluated"} - a character of factors that matched between the input land units factor and the targetted crop requirement factor
#' \item \code{"Suitability Score"} - a data frame of suitability scores for each of the matched factors
#' \item \code{"Suitability Class"} - a data frame of suitability classes for each of the matched factors
#' \item \code{"Factors' Minimum Values"} - a numeric of minimum values used in the membership function for computing the suitability scores
#' \item \code{"Factors' Minimum Values"} - a numeric of maximum values used in the membership function for computing the suitability scores
#' \item \code{"Factors' Weights"} - a numeric of weights of the factors specified in the input crop requirements
#' \item \code{"Crop Evaluated"} - a character of the name of the targetted crop requirement dataset
#' }
#' 
#' #' @seealso 
#' \code{https://alstat.github.io/ALUES/}
#' 
suitability <- function (x, y, mf = "triangular", sow_month = NULL, minimum = NULL, maximum = "average", interval = NULL, sigma = NULL) {
  n1 <- length(names(x))
  n2 <- nrow(y)
  f1 <- f2 <- numeric()
  
  if (is.numeric(sow_month)) {
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
        } else {
          if (j < length(wmav)) {
            next
          } else {
            if (i < nrow(y)) {
              break
            } else {
              if (length(f3) == 0 && length(f4) == 0) {
                stop("No factor(s) to be evaluated, since none matches with the crop requirements.")  
              } else {
                break
              }
            }
          }
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
      sow_month <- month[sow_month + idx - 1] 
    } else {
      sow_month <- month[sow_month]
    }
    
    y <- as.matrix(y)
    for (i in 1:12) {
      if (sow_month == month[i]) {
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
    stop("No factor(s) to be evaluated, since none matches with the crop requirements. If water or temp characteristics was specified then maybe you forgot to specify the sow_month argument, read doc for suit.")
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
      stop("interval should have 5 limits, run ?suit for more.") 
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
  } else {
    stop(paste("Unrecognized mf='", mf, "', please choose either 'triangular', 'trapezoidal' or 'gaussian'.", sep=""))
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
        if ((!is.null(minimum)) && (minimum == "average")) {
          Min <- reqScore[1] - ((diff(reqScore[1:2]) + diff(reqScore[2:3])) / 2)
        } else if (is.numeric(minimum)){
          if (length(minimum) == 1) {
            Min <- minimum
          } else if (length(minimum) > 1) {
            if (length(minimum) == ncol(x)) {
              Min <- minimum[f1[stats::complete.cases(f1)][j]]
            } else if (length(minimum) != ncol(x)) {
              stop("minimum length should be equal to the number of factors in x.")
            }
          }
        } else if (is.null(minimum)) {
          Min <- 0
        }
        if (!is.numeric(maximum)) { 
          if (maximum == "average") {
            Max <- reqScore[3] + ((diff(reqScore[1:2]) + diff(reqScore[2:3])) / 2)
          } else {
            stop(paste("Cannot identify maximum='", maximum, "'. maximum can only take 'average' or numeric vector of maximum.", sep=""))
          }
        } else if (is.numeric(maximum)) {
          if (length(maximum) == 1) {
            Max <- maximum
          } else if (length(maximum) > 1) {
            if (length(maximum) == ncol(x)) {
              Max <- maximum[f1[stats::complete.cases(f1)][j]] 
            } else if (length(maximum) != ncol(x)) {
              stop("maximum length should be equal to the number of factors in the input land units.")
            }
          }            
        }
        output <- case_a(df = as.matrix(LU), score = score, suiClass = suiClass, Min = Min, Max = Max, mfNum = mfNum,
                         bias = bias, j = j, a = reqScore[1], b = reqScore[2], c = reqScore[3], l1 = l1, l2 = l2, l3 = l3, l4 = l4, l5 = l5, sigma = sigma)
        score <- output[[1]]; suiClass <- output[[2]]
        
      } else if (reqScore[1] < reqScore[3]) {
        if ((!is.null(minimum)) && (minimum == "average")) {
          Min <- reqScore[1] - ((diff(reqScore[1:2]) + diff(reqScore[2:3])) / 2)
        } else if (is.numeric(minimum)) {
          if (length(minimum) == 1) {
            Min <- minimum
          } else if (length(minimum) > 1) {
            if (length(minimum) == ncol(x)) {
              Min <- minimum[f1[stats::complete.cases(f1)][j]]
            } else if (length(minimum) != ncol(x)) {
              stop("minimum length should be equal to the number of factors in x.")
            }
          }
        } else if (is.null(minimum)) {
          Min <- 0
        }

        if (!is.numeric(maximum)) {
          if (maximum == "average") {
            Max <- reqScore[3] + ((diff(reqScore[1:2]) + diff(reqScore[2:3])) / 2)
          } else {
            stop(paste("Cannot identify maximum='", maximum, "'. maximum can only take 'average' or numeric vector of maximum.", sep=""))
          }
        } else if (is.numeric(maximum)) {
          if (length(maximum) == 1) {
            Max <- maximum
          } else if (length(maximum) > 1) {
            if (length(maximum) == ncol(x)) {
              Max <- maximum[f1[stats::complete.cases(f1)][j]] 
            } else if (length(maximum) != ncol(x)) {
              stop("maximum length should be equal to the number of factors in x.")
            }
          }
        }
        output <- case_b(df = as.matrix(LU), score = score, suiClass = suiClass, Min = Min, Max = Max, mfNum = mfNum,
                         bias = bias, j = j, a = reqScore[1], b = reqScore[2], c = reqScore[3], l1 = l1, l2 = l2, l3 = l3, l4 = l4, l5 = l5, sigma = sigma)
        score <- output[[1]]; suiClass <- output[[2]]
      } else if ((reqScore[1] == reqScore[2]) &&
                   (reqScore[1] == reqScore[3]) &&
                   (reqScore[2] == reqScore[3])) {
        if ((!is.null(minimum)) && (minimum == "average")) {
          Min <- 0
          warning(paste("minimum is set to zero for factor", colnames(score)[j],
                        "since all suitability class intervals are equal."))
        } else if (is.numeric(minimum)) {
          if (length(minimum) == 1) {
            Min <- minimum
          } else if (length(minimum) > 1) {
            if (length(minimum) == ncol(x)) {
              Min <- minimum[f1[stats::complete.cases(f1)][j]]
            } else if (length(minimum) != ncol(x)) {
              stop("minimum length should be equal to the number of factors in x.")
            }
          }
        } else if (is.null(minimum)) {
          Min <- 0
        }
        Max <- reqScore[3]
        if (!is.numeric(maximum)) {
          if (maximum == "average") {
            Max <- reqScore[3]
            warning(paste("maximum is set to", reqScore[3], "for factor", colnames(score)[j],
                          "since all parameter intervals are equal."))
          } else {
            stop(paste("Cannot identify maximum='", maximum, "'. maximum can only take 'average' or numeric vector of maximum.", sep=""))
          }
        } else if (is.numeric(maximum)) {
          if (length(maximum) == 1) {
            Max <- maximum
          } else if (length(maximum) > 1) {
            if (length(maximum) == ncol(x)) {
              Max <- maximum[f1[stats::complete.cases(f1)][j]] 
            } else if (length(maximum) != ncol(x)) {
              stop("maximum length should be equal to the number of factors in x.")
            }
          }            
        }
        output <- case_b(df = as.matrix(LU), score = score, suiClass = suiClass, Min = Min, Max = Max, mfNum = mfNum,
                         bias = bias, j = j, a = reqScore[1], b = reqScore[2], c = reqScore[3], l1 = l1, l2 = l2, l3 = l3, l4 = l4, l5 = l5, sigma = sigma)
        score <- output[[1]]; suiClass <- output[[2]]
      }
    } else if (n3 == 6) {
      if ((!is.null(minimum)) && (minimum == "average")) {
        Min <- reqScore[1] - ((diff(reqScore[1:2]) + diff(reqScore[2:3]) + diff(reqScore[3:4]) + diff(reqScore[4:5]) + diff(reqScore[5:6])) / 5)
      } else if (is.numeric(minimum)){
        if (length(minimum) == 1) {
          Min <- minimum
        } else if (length(minimum) > 1) {
          if (length(minimum) == ncol(x)) {
            Min <- minimum[f1[stats::complete.cases(f1)][j]]
          } else if (length(minimum) != ncol(x)) {
            stop("minimum length should be equal to the number of factors in x.")
          }
        }
      } else if (is.null(minimum)) {
        Min <- 0
      }
      Mid <- mean(reqScore[3:4])
      if (!is.numeric(maximum)) {
        if (maximum == "average") {
          Max <- reqScore[6] + ((diff(reqScore[1:2]) + diff(reqScore[2:3]) + diff(reqScore[3:4]) + diff(reqScore[4:5]) + diff(reqScore[5:6])) / 5)
        } else {
          stop(paste("Cannot identify maximum='", maximum, "'. maximum can only take 'average' or numeric vector of maximum.", sep=""))
        }
      } else if (is.numeric(maximum)) {
        if (length(maximum) == 1) {
          Max <- maximum
        } else if (length(maximum) > 1) {
          if (length(maximum) == ncol(x)) {
            Max <- maximum[f1[stats::complete.cases(f1)][j]] 
          } else if (length(maximum) != ncol(x)) {
            stop("maximum length should be equal to the number of factors in x.")
          }
        }            
      }
      output <- case_c(df = as.matrix(LU), score = score, suiClass = suiClass, Min = Min, Max = Max, Mid = Mid, mfNum = mfNum,
                       bias = bias, j = j, a = reqScore[1], b = reqScore[2], c = reqScore[3], d = reqScore[4], e = reqScore[5], f = reqScore[6], 
                       l1 = l1, l2 = l2, l3 = l3, l4 = l4, l5 = l5, sigma = sigma)
      score <- output[[1]]; suiClass <- output[[2]]
    } else if (n3 == 5) {
      if ((!is.null(minimum)) && (minimum == "average")) {
        Min <- reqScore[1] - ((diff(reqScore[1:2]) + diff(reqScore[2:3]) + diff(reqScore[3:4]) + diff(reqScore[4:5])) / 4)
      } else if (is.numeric(minimum)) {
        if (length(minimum) == 1) {
          Min <- minimum
        } else if (length(minimum) > 1) {
          if (length(minimum) == ncol(x)) {
            Min <- minimum[f1[stats::complete.cases(f1)][j]]
          } else if (length(minimum) != ncol(x)) {
            stop("minimum length should be equal to the number of factors in x.")
          }
        }
      } else if (is.null(minimum)) {
        Min <- 0
      }
      Mid <- mean(reqScore[3:4])
      if (!is.numeric(maximum)) {
        if (maximum == "average") {
          Max <- reqScore[5]
          warning(paste("maximum is set to", reqScore[5], "for factor", colnames(score)[j],
                        "since there is a missing value on S3 class above optimum, run ?suit for more."))
        } else {
          stop(paste("Cannot identify maximum='", maximum, "'. maximum can only take 'average' or numeric vector of maximum.", sep=""))
        }
      } else if (is.numeric(maximum)) {
        if (length(maximum) == 1) {
          Max <- reqScore[5]
          warning(paste("maximum is set to", reqScore[5], "for factor", colnames(score)[j],
                        "since there is a missing value on S3 class above optimum, run ?suit for more.")) 
        } else if (length(maximum) > 1) {
          if (length(maximum) == ncol(x)) {
            Max <- reqScore[5]
            warning(paste("maximum is set to", reqScore[5], "for factor", colnames(score)[j],
                          "since there is a missing value on S3 class above optimum, run ?suit for more.")) 
          }
          else if (length(maximum) != ncol(x)) {
            stop("maximum length should be equal to the number of factors in x.")
          }
        }            
      }
      output <- case_d(df = as.matrix(LU), score = score, suiClass = suiClass, Min = Min, Max = Max, Mid = Mid, mfNum = mfNum,
                       bias = bias, j = j, a = reqScore[1], b = reqScore[2], c = reqScore[3], d = reqScore[4], l1 = l1, l2 = l2, l3 = l3, l4 = l4, l5 = l5, sigma = sigma)
      score <- output[[1]]; suiClass <- output[[2]]
    } else if (n3 == 4) {
      if ((!is.null(minimum)) && (minimum == "average")) {
        Min <- reqScore[1] - ((diff(reqScore[1:2]) + diff(reqScore[2:3]) + diff(reqScore[3:4])) / 3)
      } else if (is.numeric(minimum)){
        if (length(minimum) == 1) {
          Min <- minimum
        } else if (length(minimum) > 1) {
          if (length(minimum) == ncol(x)) {
            Min <- minimum[f1[stats::complete.cases(f1)][j]]
          } else if (length(minimum) != ncol(x)) {
            stop("minimum length should be equal to the number of factors in x.")
          }
        }
      } else if (is.null(minimum)) {
        Min <- 0
      }
      Mid <- mean(reqScore[3:4])
      if (!is.numeric(maximum)) {
        if (maximum == "average") {
          Max <- reqScore[4]
          warning(paste("maximum is set to", reqScore[4], "for factor", colnames(score)[j],
                        "since there is a missing value on S2 class above optimum, run ?suit for more."))
        } else {
          stop(paste("Cannot identify maximum='", maximum, "'. maximum can only take 'average' or numeric vector of maximum.", sep=""))
        }
      } else if (is.numeric(maximum)) {
        if (length(maximum) == 1) {
          Max <- reqScore[4]
          warning(paste("maximum is set to", reqScore[4], "for factor", colnames(score)[j],
                        "since there is a missing value on S2 class above optimum, run ?suit for more.")) 
        }
        else if (length(maximum) > 1) {
          if (length(maximum) == ncol(x)) {
            Max <- reqScore[4]
            warning(paste("maximum is set to", reqScore[4], "for factor", colnames(score)[j],
                          "since there is a missing value on S2 class above optimum, run ?suit for more.")) 
          }
          else if (length(maximum) != ncol(x))
            stop("maximum length should be equal to the number of factors in x.")
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
  
  outf <- list("Factors Evaluated" = names(minVals), 
               "Suitability Score" = as.data.frame(score), 
               "Suitability Class" = as.data.frame(suiClass), 
               "Factors' Minimum Values" = minVals, 
               "Factors' Maximum Values" = maxVals,
               "Factors' Weights" = as.numeric(CR[, 8L]))
  class(outf) <- "suitability"
  return(outf)
}