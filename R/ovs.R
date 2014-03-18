ovs <- function(dat, method = 'minimum') {
  nLU <- nrow(dat[[1]])
  ovsVal <- matrix(NA, nrow = nLU, ncol = length(dat))
  colnames(ovsVal) <- names(dat)
  for (j in 1:length(dat)) {
    if (method == 'minimum') {
      for (i in 1:nLU) {
        ovsVal[i, j] <- min(dat[[j]][i, ])
      }
    }
    if (method == 'maximum') {
      for (i in 1:nLU) {
        ovsVal[i, j] <- max(dat[[j]][i, ])
      }
    }
    if (method == 'average') {
      for (i in 1:nLU) {
        ovsVal[i, j] <- mean(as.numeric(dat[[j]][i, ]))
      }
    }
    if (method == 'exponent') {
      for (i in 1:nLU) {
        ovsVal[i, j] <- prod(as.numeric(dat[[j]][i, ])) ^ (1 / ncol(dat[[1]]))
      }
    }
  }
  return(ovsVal)
}
