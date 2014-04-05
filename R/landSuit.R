#' Land Suitability
#' @export
#' 
#' @description
#' This function calculates the suitability scores and class of the land units.
#' 
#' @param x - data consisting the land units;
#' @param y - data consisting the requirements or a given crop;
#' @param mf - membership function, currently \code{'Triangular'}.
#' 
#' @examples
#' library(ALUES)
#' x <- LandTerrain
#' y <- SoyaTerrainCR
#' 
#' SoyaTerrainSuit <- landSuit(x = x, y = y, mf = 'Triangular')
#' 
#' # Extract the suitability score of first 10 land units
#' head(SoyaTerrainSuit$score, n = 10)
#' 
#' # Extract the suitability class of first 10 land units
#' head(SoyaTerrainSuit$class, n = 10)
landSuit <- function (x, y, mf = 'Triangular') {
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
    k <- 1
    
    for(j in 1:ncol(LU)){
      reqScore <- as.numeric(CR[k,-c(1,8)])[complete.cases(as.numeric(CR[k,-c(1,8)]))]
      n3 <- length(reqScore)
      reqScore
      if (n3 == 3) {
        if (reqScore[1] > reqScore[3]) {
          reqScore <- rev(reqScore)
          Min <- reqScore[1] - ((diff(reqScore[1:2]) + diff(reqScore[2:3])) / 2)
          if (Min < 0)
            Min <- 0
          Max <- reqScore[3] + ((diff(reqScore[1:2]) + diff(reqScore[2:3])) / 2)
          for (i in 1:nrow(LU)) {
            if ((LU[i, j] < Min) || (LU[i, j] >= Max)) {
              score[i, j] <- 0; suiClass[i, j] <- 'N'
            } else if (LU[i, j] == Min) {
              score[i, j] <- 1; suiClass[i, j] <- 'S1'
            } else if ((LU[i, j] > Min) && (LU[i, j] < Max)) {
              score[i, j] <- (Max - LU[i, j]) / (Max - Min)
              if ((score[i, j] >= 0) && (score[i, j] < 0.25))
                suiClass[i, j] <- 'N'
              else if ((score[i, j] >= 0.25) && (score[i, j] < 0.5))
                suiClass[i, j] <- 'S3'
              else if ((score[i, j] >= 0.5) && (score[i, j] < 0.75))
                suiClass[i, j] <- 'S2'
              else if ((score[i, j] >= 0.75) && (score[i, j] <= 1))
                suiClass[i, j] <- 'S1'
            }
          }
        } else if (reqScore[1] < reqScore[3]){
          Min <- reqScore[1] - ((diff(reqScore[1:2]) + diff(reqScore[2:3])) / 2)
          if (Min < 0)
            Min <- 0
          Max <- reqScore[3] + ((diff(reqScore[1:2]) + diff(reqScore[2:3])) / 2)
          for (i in 1:nrow(LU)) {
            if ((LU[i, j] <= Min) || (LU[i, j] > Max)) {
              score[i, j] <- 0; suiClass[i, j] <- 'N'
            } else if ((LU[i, j] > Min) && (LU[i, j] <= Max)) {
              score[i, j] <- (LU[i, j] - Min) / (Max - Min)
              if ((score[i, j] >= 0) && (score[i, j] < 0.25))
                suiClass[i, j] <- 'N'
              else if ((score[i, j] >= 0.25) && (score[i, j] < 0.5))
                suiClass[i, j] <- 'S3'
              else if ((score[i, j] >= 0.5) && (score[i, j] < 0.75))
                suiClass[i, j] <- 'S2'
              else if ((score[i, j] >= 0.75) && (score[i, j] <= 1))
                suiClass[i, j] <- 'S1'
            }
          }
        } else if ((reqScore[1] == reqScore[2]) &&
                     (reqScore[1] == reqScore[3]) &&
                     (reqScore[2] == reqScore[3])) {
          Min <- 0; Max <- reqScore[3]
          for (i in 1:nrow(LU)) {
            if ((LU[i, j] <= Min) || (LU[i, j] > Max)) {
              score[i, j] <- 0; suiClass[i, j] <- 'N'
            } else if ((LU[i, j] > Min) && (LU[i, j] <= Max)) {
              score[i, j] <- (LU[i, j] - Min) / (Max - Min)
              if ((score[i, j] >= 0) && (score[i, j] < 0.25))
                suiClass[i, j] <- 'N'
              else if ((score[i, j] >= 0.25) && (score[i, j] < 0.5))
                suiClass[i, j] <- 'S3'
              else if ((score[i, j] >= 0.5) && (score[i, j] < 0.75))
                suiClass[i, j] <- 'S2'
              else if ((score[i, j] >= 0.75) && (score[i, j] <= 1))
                suiClass[i, j] <- 'S1'
            }
          }
        }
      } 
      k <- k + 1
    }
    return(list(Suitability_Scores = score, Suitability_Class = suiClass))
  }