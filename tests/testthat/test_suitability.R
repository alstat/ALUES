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

# Left Face Trapezoidal MF
# suit <- suitability(LaoCaiLT, SOYASoil, mf="trapezoidal")
# x <- LaoCaiLT[122,]
# reqScore <- as.numeric(SOYASoil[2,2:7])
# clnScore <- reqScore[complete.cases(reqScore)]
# Max <- clnScore[length(clnScore)] + ((diff(clnScore[1:2]) + diff(clnScore[2:3])) / 2)
# Max 
# 
# (x[1] - Min) / (reqScore[3] - Min)
# (x[1] - Min) / (Max - Min)
# 
# suit
