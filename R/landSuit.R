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
  SoyaCR <- data.frame(y[f2[complete.cases(f1)],])
  score <- matrix(NA, nrow = nrow(LU), ncol = ncol(LU))
  suiClass <- matrix(NA, nrow = nrow(LU), ncol = ncol(LU))
  colnames(score) <- names(LU)
  colnames(suiClass) <- names(LU)
  k <- 1
  
  for(j in 1:ncol(LU)){
    reqScore <- as.numeric(SoyaCR[k,-c(1,8)])[complete.cases(as.numeric(SoyaCR[k,-c(1,8)]))]
    n3 <- length(reqScore)
    
    if (n3 == 3) {
      
      if (reqScore[1] > reqScore[3]){
        reqScore <- rev(reqScore)
        Min <- reqScore[1] - ((diff(reqScore[1:2]) + diff(reqScore[2:3])) / 2)
        Max <- 2 * (reqScore[3] - Min)
        for (i in 1:nrow(LU)) {
          if ((LU[i, j] <= reqScore[3]) && (LU[i, j] > Min)) {
            score[i, j] <- (LU[i, j] - Min) / (reqScore[3] - Min)
            if ((0 <= score[i, j]) && (score[i, j] < (1 / 3)))
              suiClass[i, j] <- 'S1'
            else if (((1 / 3) <= score[i, j]) && (score[i, j] < (2 / 3)))
              suiClass[i, j] <- 'S1'
            else if (((2 / 3) <= score[i, j]) && (score[i, j] < 1))
              suiClass[i, j] <- 'S3'
          } else if ((LU[i, j] > reqScore[3]) && (LU[i, j] < Max)) {
            score[i, j] <- (Max - LU[i, j]) / (Max - reqScore[3])
            suiClass[i, j] <- 'N'
          } else if ((LU[i, j] >= Max) || (LU[i, j] <= Min)) {
            score[i, j] <- 0; suiClass[i, j] <- 'N'
          }
        }
      } else if (reqScore[1] < reqScore[3]){
        Max <- reqScore[3] + ((diff(reqScore[1:2]) + diff(reqScore[2:3])) / 2)
        Min <- 2 * reqScore[1] - Max
        for (i in 1:nrow(LU)) {
          if ((LU[i, j] <= reqScore[1]) && (LU[i, j] > Min)) {
            score[i, j] <- (LU[i, j] - Min) / (reqScore[1] - Min)
            suiClass[i, j] <- 'N'
          } else if ((LU[i, j] > reqScore[1]) && (LU[i, j] < Max)) {
            score[i, j] <- (Max - LU[i, j]) / (Max - reqScore[1])
            if ((0 <= score[i, j]) && (score[i, j] < (1 / 3)))
              suiClass[i, j] <- 'S1'
            else if (((1 / 3) <= score[i, j]) && (score[i, j] < (2 / 3)))
              suiClass[i, j] <- 'S1'
            else if (((2 / 3) <= score[i, j]) && (score[i, j] < 1))
              suiClass[i, j] <- 'S3'
          } else if ((LU[i, j] >= Max) || (LU[i, j] <= Min)) {
            score[i, j] <- 0; suiClass[i, j] <- 'N'
          }
        }
      } else if ((reqScore[1] == reqScore[2]) && (reqScore[1] == reqScore[3]) && (reqScore[2] == reqScore[3])) {
        Min <- 0; Max <- 2 * (reqScore[3] - Min)
        for (i in 1:nrow(LU)) {
          if ((LU[i, j] <= reqScore[3]) && (LU[i, j] > Min)) {
            score[i, j] <- (LU[i, j] - Min) / (reqScore[3] - Min)
            if ((0 <= score[i, j]) && (score[i, j] < (1 / 3)))
              suiClass[i, j] <- 'S1'
            else if (((1 / 3) <= score[i, j]) && (score[i, j] < (2 / 3)))
              suiClass[i, j] <- 'S1'
            else if (((2 / 3) <= score[i, j]) && (score[i, j] < 1))
              suiClass[i, j] <- 'S3'
          } else if ((LU[i, j] > reqScore[3]) && (LU[i, j] < Max)) {
            score[i, j] <- (Max - LU[i, j]) / (Max - reqScore[3])
            suiClass[i, j] <- 'N'
          } else if ((LU[i, j] >= Max) || (LU[i, j] <= Min)) {
            score[i, j] <- 0; suiClass[i, j] <- 'N'
          }
        }
      }
      
    }
    
    k <- k + 1
    
  }
  return(list(score = score, class = suiClass))
}