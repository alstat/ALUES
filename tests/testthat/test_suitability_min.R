library(testthat)
library(ALUES)

# ------------------------------
# CASE A
# ------------------------------
# Right Face Triangular MF

LaoCaiLT2 <- LaoCaiLT[5:6,]
suit_ <- suitability(LaoCaiLT2, SOYASoil, interval="unbias", min = "average", max = 20)
right_tri <- function (r) {
  x <- LaoCaiLT2[r,"CFragm"]
  reqScore <- as.numeric(SOYASoil[1,2:7])
  clnScore <- rev(reqScore[complete.cases(reqScore)])
  Min <- clnScore[1] - ((diff(clnScore[1:2]) + diff(clnScore[2:3])) / 2)
  
  return(c(Min, 20))
}

test_that("Case A: Triangular", expect_equal(as.numeric(suit_$`Factors' Minimum Values`["CFragm"]), right_tri(1)[1]))
test_that("Case A: Triangular", expect_equal(as.numeric(suit_$`Factors' Maximum Values`["CFragm"]), right_tri(1)[2]))

suit_ <- suitability(LaoCaiLT2, SOYASoil, interval="unbias", min = -1)
test_that("Case A: Triangular", expect_equal(as.numeric(suit_$`Factors' Minimum Values`["CFragm"]), -1))
# 
# suit_ <- suitability(LaoCaiLT2, SOYASoil, interval="unbias", min = rep(-1, 10))
# test_that("Case A: Triangular", expect_equal(as.numeric(suit_$`Factors' Minimum Values`), rep(-1, 7)))
# test_that("Case A: Triangular", expect_error(suitability(LaoCaiLT2, SOYASoil, interval="unbias", min = rep(-1, 5))))

# ------------------------------
# CASE B
# ------------------------------
# Left Face Triangular MF

LaoCaiLT3 <- LaoCaiLT[5:6,]
LaoCaiLT3[1,2] <- 80
suit_ <- suitability(LaoCaiLT3, SOYASoil[8:nrow(SOYASoil),], interval = "unbias", min = "average", max = 20)
left_tri <- function(r) {
  x <- LaoCaiLT3[r,"OC"]
  reqScore <- as.numeric(SOYASoil[8,2:7])
  clnScore <- reqScore[complete.cases(reqScore)]
  Max <- 20
  Min <- clnScore[1] - ((diff(clnScore[1:2]) + diff(clnScore[2:3])) / 2)
  return(c(Min, Max))
}

test_that("Case A: Triangular", expect_equal(as.numeric(suit_$`Factors' Minimum Values`["OC"]), left_tri(1)[1]))
test_that("Case A: Triangular", expect_equal(as.numeric(suit_$`Factors' Maximum Values`["OC"]), left_tri(1)[2]))

suit_ <- suitability(LaoCaiLT3, SOYASoil[8:nrow(SOYASoil),], interval = "unbias", min = -1)
test_that("Case A: Triangular", expect_equal(as.numeric(suit_$`Factors' Minimum Values`["OC"]),-1))
# 
# suit_ <- suitability(LaoCaiLT3, SOYASoil[8:nrow(SOYASoil),], interval = "unbias", min = rep(-1,10))
# test_that("Case A: Triangular", expect_equal(as.numeric(suit_$`Factors' Minimum Values`), rep(-1, 2)))
# test_that("Case A: Triangular", expect_error(suitability(LaoCaiLT3, SOYASoil, interval="unbias", min = rep(-1, 5))))

# ------------------------------
# CASE C
# ------------------------------
# Full Triangular
suit_ <- suitability(MarinduqueLT[1:6,], SAFFLOWERSoil[6, ], interval = "unbias", min = "average", max = 30)
full_tri <- function () {
  x <- MarinduqueLT[6,"pHH2O"]; 
  reqScore <- as.numeric(SAFFLOWERSoil[6,2:7])
  clnScore <- reqScore[complete.cases(reqScore)]
  
  Min <- clnScore[1] - ((diff(clnScore[1:2]) + diff(clnScore[2:3]) + diff(clnScore[3:4]) + diff(clnScore[4:5]) + diff(clnScore[5:6])) / 5)
  Max <- 30
  return(c(Min, Max))
}
test_that("Case C: Triangular", expect_equal(as.numeric(suit_$`Factors' Minimum Values`["pHH2O"]), full_tri()[1]))
test_that("Case C: Triangular", expect_equal(as.numeric(suit_$`Factors' Maximum Values`["pHH2O"]), full_tri()[2]))

suit_ <- suitability(MarinduqueLT[1:6,], SAFFLOWERSoil[6, ], interval = "unbias", min = -1)
test_that("Case C: Triangular", expect_equal(as.numeric(suit_$`Factors' Minimum Values`["pHH2O"]), -1))
# 
# suit_ <- suitability(MarinduqueLT[1:6,], SAFFLOWERSoil[6, ], interval = "unbias", min = rep(-1,6))
# test_that("Case A: Triangular", expect_equal(as.numeric(suit_$`Factors' Minimum Values`), rep(-1, 1)))
# test_that("Case A: Triangular", expect_error(suitability(MarinduqueLT[1:6,], SAFFLOWERSoil[6, ], interval = "unbias", min = rep(-1,9))))

# ------------------------------
# CASE D
# ------------------------------
# Triangular
MarinduqueLTNew <- MarinduqueLT[7:8,]
suit_ <- suitability(MarinduqueLTNew, ALUES::BAMBOOSoil, interval = "unbias", min = "average", max = 40)
full_tri <- function (r) {
  x <- MarinduqueLTNew[r,"SoilTe"]
  reqScore <- as.numeric(ALUES::BAMBOOSoil[3,2:7])
  clnScore <- reqScore[complete.cases(reqScore)]

  Max <- clnScore[5]
  Mid <- mean(clnScore[3:4])
  Min <- clnScore[1] - ((diff(clnScore[1:2]) + diff(clnScore[2:3]) + diff(clnScore[3:4]) + diff(clnScore[4:5])) / 4)
  return(c(Min, Max))
}
test_that("Case D: Triangular", expect_equal(as.numeric(suit_$`Factors' Minimum Values`["SoilTe"]), full_tri(1)[1]))
test_that("Case D: Triangular", expect_equal(as.numeric(suit_$`Factors' Maximum Values`["SoilTe"]), full_tri(1)[2]))

suit_ <- suitability(MarinduqueLTNew, ALUES::BAMBOOSoil, interval = "unbias", min = -1)
test_that("Case D: Triangular", expect_equal(as.numeric(suit_$`Factors' Minimum Values`["SoilTe"]), -1))
# 
# suit_ <- suitability(MarinduqueLT[1:6,],  ALUES::BAMBOOSoil, interval = "unbias", min = rep(-1,6))
# test_that("Case A: Triangular", expect_equal(as.numeric(suit_$`Factors' Minimum Values`), rep(-1, 2)))
# test_that("Case A: Triangular", expect_error(suitability(MarinduqueLT[1:6,],  ALUES::BAMBOOSoil, interval = "unbias", min = rep(-1,3))))

# ------------------------------
# CASE E
# ------------------------------
# Triangular
MarinduqueLTNew <- MarinduqueLT[7:8,]
BAMBOOSoil[3, 6] <- NA
BAMBOOSoil[3, 5] <- 9.0
BAMBOOSoilNew <- BAMBOOSoil
suit_ <- suitability(MarinduqueLTNew, BAMBOOSoilNew, interval="unbias", min = "average", max = 50)
full_tri <- function (r) {
  x <- MarinduqueLTNew[r,"SoilTe"]; 
  reqScore <- as.numeric(BAMBOOSoilNew[3,2:7])
  clnScore <- reqScore[complete.cases(reqScore)]

  Max <- clnScore[4]
  Mid <- mean(clnScore[3:4])
  Min <- clnScore[1] - ((diff(clnScore[1:2]) + diff(clnScore[2:3]) + diff(clnScore[3:4])) / 3)
  return(c(Min, Max))
}
test_that("Case E: Triangular", expect_equal(as.numeric(suit_$`Factors' Minimum Values`["SoilTe"]), full_tri(1)[1]))
test_that("Case E: Triangular", expect_equal(as.numeric(suit_$`Factors' Maximum Values`["SoilTe"]), full_tri(1)[2]))

suit_ <- suitability(MarinduqueLTNew, BAMBOOSoilNew, interval="unbias", min = -1)
test_that("Case E: Triangular", expect_equal(as.numeric(suit_$`Factors' Minimum Values`["SoilTe"]), -1))
# 
# suit_ <- suitability(MarinduqueLTNew,  BAMBOOSoilNew, interval = "unbias", min = rep(-1,6))
# test_that("Case A: Triangular", expect_equal(as.numeric(suit_$`Factors' Minimum Values`), rep(-1, 2)))
# test_that("Case A: Triangular", expect_error(suitability(MarinduqueLTNew,  BAMBOOSoilNew, interval = "unbias", min = rep(-1,3))))
# 
# # 
# test_that("Case E: Triangular", expect_equal(suit_$`Suitability Class`["SoilTe"][1,], "N"))
# test_that("Case E: Triangular", expect_equal(suit_$`Suitability Class`["SoilTe"][2,], "S1"))
# test_that("Case E: Triangular", expect_equal(suit_$`Suitability Score`["SoilTe"][1,], full_tri(1)))
# test_that("Case E: Triangular", expect_equal(suit_$`Suitability Score`["SoilTe"][2,], full_tri(2)))
# 
# # Triangular
# MarinduqueLTNew2 <- MarinduqueLT[7:8,]
# MarinduqueLTNew2[1, 6] <- 5
# suit_ <- suitability(MarinduqueLTNew2, BAMBOOSoilNew, interval="unbias")
# full_tri <- function (r) {
#   x <- MarinduqueLTNew2[r,"SoilTe"]; Min <- 0
#   reqScore <- as.numeric(BAMBOOSoilNew[3,2:7])
#   clnScore <- reqScore[complete.cases(reqScore)]
# 
#   Max <- reqScore[4]
#   Mid <- mean(reqScore[3:4])
#   Min <- 0
#   if (x > Max) {
#     score <- 0
#   } else if (x > Mid) {
#     score <- (Max - x) / (Max - Mid)
#   } else if ((x > Min) && (x <= Mid)) {
#     score <- (x - Min) / (Mid - Min)
#   }
# 
#   return(score)
# }
# test_that("Case E: Triangular", expect_equal(suit_$`Suitability Score`["SoilTe"][1,], full_tri(1)))
# test_that("Case E: Triangular", expect_equal(suit_$`Suitability Class`["SoilTe"][1,], "S2"))
# test_that("Case E: Triangular", expect_equal(suit_$`Suitability Score`["SoilTe"][2,], full_tri(2)))
# test_that("Case E: Triangular", expect_equal(suit_$`Suitability Class`["SoilTe"][2,], "S1"))
# 
# # Trapezoidal
# suit_ <- suitability(MarinduqueLTNew, BAMBOOSoilNew, mf="trapezoidal", interval="unbias")
# full_tra <- function (r) {
#   x <- MarinduqueLTNew[r,"SoilTe"]; Min <- 0
#   reqScore <- as.numeric(BAMBOOSoil[3,2:7])
#   clnScore <- reqScore[complete.cases(reqScore)]
#   Max <- reqScore[4]
#   Mid <- mean(reqScore[3:4])
#   Min <- 0
#   if (x > Max) {
#     score <- 0
#   } else if (x > reqScore[4]) {
#     score <- (Max - x) / (Max - reqScore[4])
#   } else if (x <= reqScore[3]) {
#     score <- (x - Min) / (reqScore[3] - Min)
#   } else if ((x > reqScore[3]) && (x <= reqScore[4])) {
#     score <- 1
#   }
# 
#   return(score)
# }
# test_that("Case E: Trapezoidal", expect_equal(suit_$`Suitability Score`["SoilTe"][1,], full_tra(1)))
# test_that("Case E: Trapezoidal", expect_equal(suit_$`Suitability Class`["SoilTe"][1,], "N"))
# test_that("Case E: Trapezoidal", expect_equal(suit_$`Suitability Score`["SoilTe"][2,], full_tra(2)))
# test_that("Case E: Trapezoidal", expect_equal(suit_$`Suitability Class`["SoilTe"][2,], "S1"))
# 
# MarinduqueLTNew2 <- MarinduqueLT[7:8,]
# MarinduqueLTNew2[1, 6] <- 5
# 
# suit_ <- suitability(MarinduqueLTNew2, BAMBOOSoilNew, mf="trapezoidal", interval="unbias")
# full_tra <- function (r) {
#   x <- MarinduqueLTNew2[r,"SoilTe"]; Min <- 0
#   reqScore <- as.numeric(BAMBOOSoil[3,2:7])
#   clnScore <- reqScore[complete.cases(reqScore)]
#   Max <- reqScore[4]
#   Mid <- mean(reqScore[3:4])
#   Min <- 0
#   if (x > Max) {
#     score <- 0
#   } else if (x > reqScore[4]) {
#     score <- (Max - x) / (Max - reqScore[4])
#   } else if (x <= reqScore[3]) {
#     score <- (x - Min) / (reqScore[3] - Min)
#   } else if ((x > reqScore[3]) && (x <= reqScore[4])) {
#     score <- 1; class_ = "S1"
#   }
# 
# 
#   return(score)
# }
# test_that("Case E: Trapezoidal", expect_equal(suit_$`Suitability Score`["SoilTe"][1,], full_tra(1)))
# test_that("Case E: Trapezoidal", expect_equal(suit_$`Suitability Class`["SoilTe"][1,], "S2"))
# test_that("Case E: Trapezoidal", expect_equal(suit_$`Suitability Score`["SoilTe"][2,], full_tra(2)))
# test_that("Case E: Trapezoidal", expect_equal(suit_$`Suitability Class`["SoilTe"][2,], "S1"))
# 
# # Gaussian
# suit_ <- suitability(MarinduqueLTNew, BAMBOOSoilNew, mf="gaussian", interval="unbias")
# full_gau <- function (r) {
#   x <- MarinduqueLTNew[r,"SoilTe"]; Min <- 0; sigma <- 1
#   reqScore <- as.numeric(BAMBOOSoilNew[3,2:7])
#   clnScore <- reqScore[complete.cases(reqScore)]
#   Max <- reqScore[4]
#   Mid <- mean(reqScore[3:4])
#   Min <- 0
#   score <- exp((-1 / 2) * (((x - Mid) / sigma)^2))
#   return(score)
# }
# test_that("Case E: Gaussian", expect_equal(suit_$`Suitability Score`["SoilTe"][1,], full_gau(1)))
# test_that("Case E: Gaussian", expect_equal(suit_$`Suitability Class`["SoilTe"][1,], "N"))
# test_that("Case E: Gaussian", expect_equal(suit_$`Suitability Score`["SoilTe"][2,], full_gau(2)))
# test_that("Case E: Gaussian", expect_equal(suit_$`Suitability Class`["SoilTe"][2,], "S1"))
# 
# suit_ <- suitability(MarinduqueLTNew2, BAMBOOSoilNew, mf="gaussian", interval="unbias")
# full_gau <- function (r) {
#   x <- MarinduqueLTNew2[r,"SoilTe"]; Min <- 0; sigma <- 1
#   reqScore <- as.numeric(BAMBOOSoilNew[3,2:7])
#   clnScore <- reqScore[complete.cases(reqScore)]
#   Max <- reqScore[4]
#   Mid <- mean(reqScore[3:4])
#   Min <- 0
#   score <- exp((-1 / 2) * (((x - Mid) / sigma)^2))
#   return(score)
# }
# test_that("Case E: Gaussian", expect_equal(suit_$`Suitability Score`["SoilTe"][1,], full_gau(1)))
# test_that("Case E: Gaussian", expect_equal(suit_$`Suitability Class`["SoilTe"][1,], "S2"))
# test_that("Case E: Gaussian", expect_equal(suit_$`Suitability Score`["SoilTe"][2,], full_gau(2)))
# test_that("Case E: Gaussian", expect_equal(suit_$`Suitability Class`["SoilTe"][2,], "S1"))
