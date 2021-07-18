library(testthat)
library(ALUES)

# NA PRESENCE
suit <- suitability(LaoCaiLT, ALFALFASoil)
test_that("Parameter name present:", expect_equal(suit$`Actual Factors Evaluated`[1], "SoilTe"))
test_that("Parameter scores NA all:", 
          expect_equal(sum(is.na(suit$`Suitability Score`["SoilTe"])), 
          nrow(suit$`Suitability Score`["SoilTe"])))
test_that("Parameter classes NA all:", 
          expect_equal(sum(is.na(suit$`Suitability Class`["SoilTe"])), 
                       nrow(suit$`Suitability Class`["SoilTe"])))
test_that("Parameter Minimum NA:", 
          expect_true(is.na(suit$`Factors' Minimum Values`["SoilTe"])))
test_that("Parameter Maximum NA:", 
          expect_true(is.na(suit$`Factors' Maximum Values`["SoilTe"])))

# WARNINGS
test_that("Expecting Warning", expect_warning(suitability(LaoCaiLT, ALFALFASoil)))

# ------------------------------
# CASE A
# ------------------------------
# Right Face Triangular MF
suit <- suitability(LaoCaiLT, SOYASoil)
right_tri <- function () {
  x <- LaoCaiLT[6,"CFragm"]; Min <- 0
  reqScore <- as.numeric(SOYASoil[1,2:7])
  clnScore <- rev(reqScore[complete.cases(reqScore)])
  Max <- clnScore[length(clnScore)] + ((diff(clnScore[1:2]) + diff(clnScore[2:3])) / 2)
  return((Max - x) / (Max - Min))
}
test_that("Right Face Triangular", expect_equal(suit$`Suitability Score`["CFragm"][6,], right_tri()))

# Right Face Trapezoidal MF
suit <- suitability(LaoCaiLT, SOYASoil, mf="trapezoidal")
right_tra <- function () {
  x <- LaoCaiLT[6,"CFragm"]; Min <- 0
  reqScore <- as.numeric(SOYASoil[1,2:7])
  clnScore <- rev(reqScore[complete.cases(reqScore)])
  Max <- clnScore[length(clnScore)] + ((diff(clnScore[1:2]) + diff(clnScore[2:3])) / 2)
  if (x < clnScore[1]) {
    return(1)
  }
}
test_that("Right Face Trapezoidal", expect_equal(suit$`Suitability Score`["CFragm"][6,], right_tra()))

# Right Face Gaussian MF
suit <- suitability(LaoCaiLT[5:6,], SOYASoil[1:3,], mf="gaussian")
right_gau<- function () {
  x <- LaoCaiLT[5:6,"CFragm"]; sigma <- 1
  reqScore <- as.numeric(SOYASoil[1,2:7])
  clnScore <- rev(reqScore[complete.cases(reqScore)])
  Min <- 0
  return(exp((-1 / 2) * `^`(((x - Min) / sigma), 2)))
}
test_that("Right Face Gaussian", expect_equal(as.numeric(suit$`Suitability Score`[,"CFragm"]), right_gau()))

# ------------------------------
# CASE B
# ------------------------------
# Left Face Triangular MF
suit <- suitability(LaoCaiLT[122:124,], SOYASoil[8:nrow(SOYASoil),])
left_tri <- function() {
  x <- LaoCaiLT[122,"OC"]
  reqScore <- as.numeric(SOYASoil[8,2:7])
  clnScore <- reqScore[complete.cases(reqScore)]
  Max <- clnScore[length(clnScore)] + ((diff(clnScore[1:2]) + diff(clnScore[2:3])) / 2)
  Min <- 0
  (x - Min) / (Max - Min)
}
test_that("Left Face Triangular", expect_equal(suit$`Suitability Score`[1,"OC"], left_tri()))

# Left Face Trapezoidal MF
suit <- suitability(LaoCaiLT[122:124,], SOYASoil[8:nrow(SOYASoil),], mf="trapezoidal")
left_tra <- function() {
  x <- LaoCaiLT[122,"OC"]
  reqScore <- as.numeric(SOYASoil[8,2:7])
  clnScore <- reqScore[complete.cases(reqScore)]
  Min <- 0
  (x - Min) / (clnScore[length(clnScore)] - Min)
}
test_that("Left Face Trapezoidal", expect_equal(suit$`Suitability Score`[1,"OC"], left_tra()))

# Left Face Gaussian MF
suit <- suitability(LaoCaiLT[122:124,], SOYASoil[8:nrow(SOYASoil),], mf="gaussian")
left_gau <- function() {
  x <- LaoCaiLT[122,"OC"]; sigma <- 1
  reqScore <- as.numeric(SOYASoil[8,2:7])
  clnScore <- reqScore[complete.cases(reqScore)]
  Max <- clnScore[length(clnScore)] + ((diff(clnScore[1:2]) + diff(clnScore[2:3])) / 2)
  exp((-1 / 2) * (((x - Max) / sigma)^2))
}
test_that("Left Face Gaussian", expect_equal(suit$`Suitability Score`[1,"OC"], left_gau()))




