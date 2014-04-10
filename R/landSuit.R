#' Land Suitability
#' @export
#' 
#' @description
#' This function calculates the suitability scores and class of the land units.
#' 
#' @param x - data consisting the land units;
#' @param y - data consisting the requirements or a given crop;
#' @param mf - membership function, currently \code{'Triangular'}.
#' @param min - minimum value for factors. If \code{NULL}, \code{min} is set to 0. 
#'              To modify this simply set it to any numeric value, say 1 or 0.5, and so on.
#'              For different \code{min} for every factor, simply concatenate the different
#'              \code{min}.
#' @param interval - domains for every suitability class (S1, S2, S3).
#' 
#' @examples
#' library(ALUES)
#' x <- LandTerrain
#' y <- SoyaTerrainCR
#' 
#' SoyaTerrainSuit <- landSuit(x = x, y = y, mf = 'triangular')
#' 
#' # Extract the suitability score of first 10 land units
#' head(SoyaTerrainSuit$Suitability_Score, n = 10)
#' 
#' # Extract the suitability class of first 10 land units
#' head(SoyaTerrainSuit$Suitability_Class, n = 10)
landSuit <- function (x, y, mf = 'triangular', min = NULL, interval = 'fixed') {
  n1 <- length(names(x))
  n2 <- nrow(y)
  f1 <- f2 <- numeric()
  
  for (i in 1:n2) {
    for (j in 1:n1) {
      if (as.character(y[i, 1]) == names(x)[j]) {
        f1[i] <- j; f2[i] <- i
      } 
    }
  }
  
  LU <- data.frame(x[,f1[complete.cases(f1)]])
  CR <- data.frame(y[f2[complete.cases(f1)],])
  score <- matrix(NA, nrow = nrow(LU), ncol = ncol(LU))
  suiClass <- matrix(NA, nrow = nrow(LU), ncol = ncol(LU))
  colnames(score) <- names(LU)
  colnames(suiClass) <- names(LU)
  colnames(score)
  k <- 1
  
  for(j in 1:ncol(LU)){
    reqScore <- as.numeric(CR[k,-c(1,8)])[complete.cases(as.numeric(CR[k,-c(1,8)]))]
    n3 <- length(reqScore)
    if (mf == 'triangular') {
      if (n3 == 3) {
        if (reqScore[1] > reqScore[3]) {
          reqScore <- rev(reqScore)
          if ((!is.null(min)) && (min == 'average')) {
            Min <- reqScore[1] - ((diff(reqScore[1:2]) + diff(reqScore[2:3])) / 2) 
          } else if (is.numeric(min)){
            if (length(min) == 1)
              Min <- min
            else if (length(min) > 1) {
              if (length(min) == ncol(x))
                Min <- min[f1[complete.cases(f1)][j]]
              else if (length(min) != ncol(x))
                stop('min length should be equal to the number of factors in x.')
            }
          } else if (is.null(min)) {
            Min <- 0
          }
          Max <- reqScore[3] + ((diff(reqScore[1:2]) + diff(reqScore[2:3])) / 2)
          for (i in 1:nrow(LU)) {
            if ((LU[i, j] < Min) || (LU[i, j] >= Max)) {
              score[i, j] <- 0; suiClass[i, j] <- 'N'
            } else if (LU[i, j] == Min) {
              score[i, j] <- 1; suiClass[i, j] <- 'S1'
            } else if ((LU[i, j] > Min) && (LU[i, j] < Max)) {
              if (interval == 'fixed')
                l1 = 0; l2 = 0.25; l3 = 0.5; l4 = 0.75; l5 = 1
              if (is.numeric(interval)) {
                if (length(interval) != 5)
                  stop('interval should have 5 limits, run ?landSuit for more.')
                else
                  l1 = interval[1]; l2 = interval[2]; l3 = interval[3]; l4 = interval[4]; l5 = interval[5]
              }
              score[i, j] <- (Max - LU[i, j]) / (Max - Min)
              if ((score[i, j] >= l1) && (score[i, j] < l2))
                suiClass[i, j] <- 'N'
              else if ((score[i, j] >= l2) && (score[i, j] < l3))
                suiClass[i, j] <- 'S3'
              else if ((score[i, j] >= l3) && (score[i, j] < l4))
                suiClass[i, j] <- 'S2'
              else if ((score[i, j] >= l4) && (score[i, j] <= l5))
                suiClass[i, j] <- 'S1'
            }
          }
        } else if (reqScore[1] < reqScore[3]) {
          if ((!is.null(min)) && (min == 'average')) {
            Min <- reqScore[1] - ((diff(reqScore[1:2]) + diff(reqScore[2:3])) / 2) 
          } else if (is.numeric(min)){
            if (length(min) == 1)
              Min <- min
            else if (length(min) > 1) {
              if (length(min) == ncol(x))
                Min <- min[f1[complete.cases(f1)][j]]
              else if (length(min) != ncol(x))
                stop('min length should be equal to the number of factors in x.')
            }
          } else if (is.null(min)) {
            Min <- 0
          }
          Max <- reqScore[3] + ((diff(reqScore[1:2]) + diff(reqScore[2:3])) / 2)
          for (i in 1:nrow(LU)) {
            if ((LU[i, j] <= Min) || (LU[i, j] > Max)) {
              score[i, j] <- 0; suiClass[i, j] <- 'N'
            } else if ((LU[i, j] > Min) && (LU[i, j] <= Max)) {
              if (interval == 'fixed')
                l1 = 0; l2 = 0.25; l3 = 0.5; l4 = 0.75; l5 = 1
              if (is.numeric(interval)) {
                if (length(interval) != 5)
                  stop('interval should have 5 limits, run ?landSuit for more.')
                else
                  l1 = interval[1]; l2 = interval[2]; l3 = interval[3]; l4 = interval[4]; l5 = interval[5]
              }
              score[i, j] <- (LU[i, j] - Min) / (Max - Min)
              if ((score[i, j] >= l1) && (score[i, j] < l2))
                suiClass[i, j] <- 'N'
              else if ((score[i, j] >= l2) && (score[i, j] < l3))
                suiClass[i, j] <- 'S3'
              else if ((score[i, j] >= l3) && (score[i, j] < l4))
                suiClass[i, j] <- 'S2'
              else if ((score[i, j] >= l4) && (score[i, j] <= l5))
                suiClass[i, j] <- 'S1'
            }
          }
        } else if ((reqScore[1] == reqScore[2]) &&
                     (reqScore[1] == reqScore[3]) &&
                     (reqScore[2] == reqScore[3])) {
          if ((!is.null(min)) && (min == 'average')) {
            Min <- 0
            warning(paste('min is set to zero for factor', colnames(score)[j],
                          'since all suitability class intervals are equal.'))
          } else if (is.numeric(min)){
            if (length(min) == 1)
              Min <- min
            else if (length(min) > 1) {
              if (length(min) == ncol(x))
                Min <- min[f1[complete.cases(f1)][j]]
              else if (length(min) != ncol(x))
                stop('min length should be equal to the number of factors in x.')
            }
          } else if (is.null(min)) {
            Min <- 0
          }
          Max <- reqScore[3]
          for (i in 1:nrow(LU)) {
            if ((LU[i, j] <= Min) || (LU[i, j] > Max)) {
              score[i, j] <- 0; suiClass[i, j] <- 'N'
            } else if ((LU[i, j] > Min) && (LU[i, j] <= Max)) {
              if (interval == 'fixed')
                l1 = 0; l2 = 0.25; l3 = 0.5; l4 = 0.75; l5 = 1
              if (is.numeric(interval)) {
                if (length(interval) != 5)
                  stop('interval should have 5 limits, run ?landSuit for more.')
                else
                  l1 = interval[1]; l2 = interval[2]; l3 = interval[3]; l4 = interval[4]; l5 = interval[5]
              }
              score[i, j] <- (LU[i, j] - Min) / (Max - Min)
              if ((score[i, j] >= l1) && (score[i, j] < l2))
                suiClass[i, j] <- 'N'
              else if ((score[i, j] >= l2) && (score[i, j] < l3))
                suiClass[i, j] <- 'S3'
              else if ((score[i, j] >= l3) && (score[i, j] < l4))
                suiClass[i, j] <- 'S2'
              else if ((score[i, j] >= l4) && (score[i, j] <= l5))
                suiClass[i, j] <- 'S1'
            }
          }
        }
      } else if (n3 == 6) {
        if ((!is.null(min)) && (min == 'average')) {
          Min <- reqScore[1] - ((diff(reqScore[1:2]) + diff(reqScore[2:3]) + diff(reqScore[3:4]) + diff(reqScore[4:5]) + diff(reqScore[5:6])) / 2)
        } else if (is.numeric(min)){
          if (length(min) == 1)
            Min <- min
          else if (length(min) > 1) {
            if (length(min) == ncol(x))
              Min <- min[f1[complete.cases(f1)][j]]
            else if (length(min) != ncol(x))
              stop('min length should be equal to the number of factors in x.')
          }
        } else if (is.null(min)) {
          Min <- 0
        }
        Max <- reqScore[3] + ((diff(reqScore[1:2]) + diff(reqScore[2:3]) + diff(reqScore[3:4]) + diff(reqScore[4:5]) + diff(reqScore[5:6])) / 2)
        Mid <- mean(reqScore[3:4])
        for (i in 1:nrow(LU)) {
          if ((LU[i, j] <= Min) || (LU[i, j] >= Max)) {
            score[i, j] <- 0; suiClass[i, j] <- 'N'
          } else if ((LU[i, j] > Min) && (LU[i, j] <= Mid)) {
            if (interval == 'fixed')
              l1 = 0; l2 = 0.25; l3 = 0.5; l4 = 0.75; l5 = 1
            if (is.numeric(interval)) {
              if (length(interval) != 5)
                stop('interval should have 5 limits, run ?landSuit for more.')
              else
                l1 = interval[1]; l2 = interval[2]; l3 = interval[3]; l4 = interval[4]; l5 = interval[5]
            }
            score[i, j] <- (LU[i, j] - Min) / (Mid - Min)
            if ((score[i, j] >= l1) && (score[i, j] < l2))
              suiClass[i, j] <- 'N'
            else if ((score[i, j] >= l2) && (score[i, j] < l3))
              suiClass[i, j] <- 'S3'
            else if ((score[i, j] >= l3) && (score[i, j] < l4))
              suiClass[i, j] <- 'S2'
            else if ((score[i, j] >= l4) && (score[i, j] <= l5))
              suiClass[i, j] <- 'S1'
          } else if ((LU[i, j] > Mid) && (LU[i, j] < Max)) {
            if (interval == 'fixed')
              l1 = 0; l2 = 0.25; l3 = 0.5; l4 = 0.75; l5 = 1
            if (is.numeric(interval)) {
              if (length(interval) != 5)
                stop('interval should have 5 limits, run ?landSuit for more.')
              else
                l1 = interval[1]; l2 = interval[2]; l3 = interval[3]; l4 = interval[4]; l5 = interval[5]
            }
            score[i, j] <- (Max - LU[i, j]) / (Max - Mid)
            if ((score[i, j] >= l1) && (score[i, j] < l2))
              suiClass[i, j] <- 'N'
            else if ((score[i, j] >= l2) && (score[i, j] < l3))
              suiClass[i, j] <- 'S3'
            else if ((score[i, j] >= l3) && (score[i, j] < l4))
              suiClass[i, j] <- 'S2'
            else if ((score[i, j] >= l4) && (score[i, j] <= l5))
              suiClass[i, j] <- 'S1'
          } 
        }
      } else if (n3 == 5) {
        if ((!is.null(min)) && (min == 'average')) {
          Min <- reqScore[1] - ((diff(reqScore[1:2]) + diff(reqScore[2:3]) + diff(reqScore[3:4]) + diff(reqScore[4:5])) / 2)
        } else if (is.numeric(min)){
          if (length(min) == 1)
            Min <- min
          else if (length(min) > 1) {
            if (length(min) == ncol(x))
              Min <- min[f1[complete.cases(f1)][j]]
            else if (length(min) != ncol(x))
              stop('min length should be equal to the number of factors in x.')
          }
        } else if (is.null(min)) {
          Min <- 0
        }
        Max <- reqScore[5]
        Mid <- mean(reqScore[3:4])
        for (i in 1:nrow(LU)) {
          if ((LU[i, j] <= Min) || (LU[i, j] > Max)) {
            score[i, j] <- 0; suiClass[i, j] <- 'N'
          } else if ((LU[i, j] > Min) && (LU[i, j] <= Mid)) {
            if (interval == 'fixed')
              l1 = 0; l2 = 0.25; l3 = 0.5; l4 = 0.75; l5 = 1
            if (is.numeric(interval)) {
              if (length(interval) != 5)
                stop('interval should have 5 limits, run ?landSuit for more.')
              else
                l1 = interval[1]; l2 = interval[2]; l3 = interval[3]; l4 = interval[4]; l5 = interval[5]
            }
            score[i, j] <- (LU[i, j] - Min) / (Mid - Min)
            if ((score[i, j] >= l1) && (score[i, j] < l2))
              suiClass[i, j] <- 'N'
            else if ((score[i, j] >= l2) && (score[i, j] < l3))
              suiClass[i, j] <- 'S3'
            else if ((score[i, j] >= l3) && (score[i, j] < l4))
              suiClass[i, j] <- 'S2'
            else if ((score[i, j] >= l4) && (score[i, j] <= l5))
              suiClass[i, j] <- 'S1'
          } else if ((LU[i, j] > Mid) && (LU[i, j] <= Max)) {
            if (interval == 'fixed')
              l1 = 0; l2 = 0.25; l3 = 0.5; l4 = 0.75; l5 = 1
            if (is.numeric(interval)) {
              if (length(interval) != 5)
                stop('interval should have 5 limits, run ?landSuit for more.')
              else
                l1 = interval[1]; l2 = interval[2]; l3 = interval[3]; l4 = interval[4]; l5 = interval[5]
            }
            score[i, j] <- (((Max - LU[i, j]) / (Max - Mid)) * 1 - l3) + l3
            if ((score[i, j] >= l1) && (score[i, j] < l2))
              suiClass[i, j] <- 'N'
            else if ((score[i, j] >= l2) && (score[i, j] < l3))
              suiClass[i, j] <- 'S3'
            else if ((score[i, j] >= l3) && (score[i, j] < l4))
              suiClass[i, j] <- 'S2'
            else if ((score[i, j] >= l4) && (score[i, j] <= l5))
              suiClass[i, j] <- 'S1'
          } 
        }
      } else if (n3 == 4) {
        if ((!is.null(min)) && (min == 'average')) {
          Min <- reqScore[1] - ((diff(reqScore[1:2]) + diff(reqScore[2:3]) + diff(reqScore[3:4])) / 2)
        } else if (is.numeric(min)){
          if (length(min) == 1)
            Min <- min
          else if (length(min) > 1) {
            if (length(min) == ncol(x))
              Min <- min[f1[complete.cases(f1)][j]]
            else if (length(min) != ncol(x))
              stop('min length should be equal to the number of factors in x.')
          }
        } else if (is.null(min)) {
          Min <- 0
        }
        Max <- reqScore[4]
        Mid <- mean(reqScore[3:4])
        for (i in 1:nrow(LU)) {
          if ((LU[i, j] <= Min) || (LU[i, j] > Max)) {
            score[i, j] <- 0; suiClass[i, j] <- 'N'
          } else if ((LU[i, j] > Min) && (LU[i, j] <= Mid)) {
            if (interval == 'fixed')
              l1 = 0; l2 = 0.25; l3 = 0.5; l4 = 0.75; l5 = 1
            if (is.numeric(interval)) {
              if (length(interval) != 5)
                stop('interval should have 5 limits, run ?landSuit for more.')
              else
                l1 = interval[1]; l2 = interval[2]; l3 = interval[3]; l4 = interval[4]; l5 = interval[5]
            }
            score[i, j] <- (LU[i, j] - Min) / (Mid - Min)
            if ((score[i, j] >= l1) && (score[i, j] < l2))
              suiClass[i, j] <- 'N'
            else if ((score[i, j] >= l2) && (score[i, j] < l3))
              suiClass[i, j] <- 'S3'
            else if ((score[i, j] >= l3) && (score[i, j] < l4))
              suiClass[i, j] <- 'S2'
            else if ((score[i, j] >= l4) && (score[i, j] <= l5))
              suiClass[i, j] <- 'S1'
          } else if ((LU[i, j] > Mid) && (LU[i, j] <= Max)) {
            if (interval == 'fixed')
              l1 = 0; l2 = 0.25; l3 = 0.5; l4 = 0.75; l5 = 1
            if (is.numeric(interval)) {
              if (length(interval) != 5)
                stop('interval should have 5 limits, run ?landSuit for more.')
              else
                l1 = interval[1]; l2 = interval[2]; l3 = interval[3]; l4 = interval[4]; l5 = interval[5]
            }
            score[i, j] <- (((Max - LU[i, j]) / (Max - Mid)) * (1 - l4)) + l4
            if ((score[i, j] >= l1) && (score[i, j] < l2))
              suiClass[i, j] <- 'N'
            else if ((score[i, j] >= l2) && (score[i, j] < l3))
              suiClass[i, j] <- 'S3'
            else if ((score[i, j] >= l3) && (score[i, j] < l4))
              suiClass[i, j] <- 'S2'
            else if ((score[i, j] >= l4) && (score[i, j] <= l5))
              suiClass[i, j] <- 'S1'
          }
        }
      }
    } else if (mf == 'trapezoidal') {
      stop('trapezoidal membership function is still under construction.')
    }
    k <- k + 1
  }
  return(list(Suitability_Scores = score, Suitability_Class = suiClass))
}