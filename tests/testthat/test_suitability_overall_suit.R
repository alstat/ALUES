library(testthat)
library(ALUES)

suit <- suitability(MarinduqueLT, BANANASoil, interval="unbias")
test_that("Overall Minimum", expect_equal(min(suit$`Suitability Score`[1,]), overall_suit(suit, method="minimum")[1,1]))
test_that("Overall Minimum", expect_equal(max(suit$`Suitability Score`[1,]), overall_suit(suit, method="maximum")[1,1]))

# Weighted Average
wts <- suit$`Factors' Weights`
wts[is.na(wts)] <- max(wts, na.rm = TRUE) + 1
new_wts <- (sum(wts) - wts)
new_wts <- new_wts/sum(new_wts)
sum(suit$`Suitability Score`[1,] * new_wts, na.rm = TRUE)

test_that("Overall Minimum", expect_equal(sum(suit$`Suitability Score`[1,] * new_wts, na.rm = TRUE), overall_suit(suit, method="average")[1,1]))

# Average
suit <- suitability(MarinduqueLT, ALFALFASoil, interval="unbias")
test_that("Overall Minimum", expect_equal(min(suit$`Suitability Score`[1,], na.rm=TRUE), overall_suit(suit, method="minimum")[1,1]))
test_that("Overall Maximum", expect_equal(max(suit$`Suitability Score`[1,], na.rm=TRUE), overall_suit(suit, method="maximum")[1,1]))
test_that("Overall Average", expect_equal(mean(as.numeric(suit$`Suitability Score`[1,]), na.rm=TRUE), overall_suit(suit, method="average")[1,1]))

# Show class only
suit <- suitability(MarinduqueLT, ALFALFASoil, interval="unbias")
test_that("Expecting for overall", expect_error(overall_suit(suit, interval=c(0,2))))
test_that("Expecting for overall", expect_error(overall_suit(suit, interval=c(0,0.5,0.6,0.7,0.9))))
test_that("Expecting for overall", expect_error(overall_suit(suit, interval=c(0,0.5,0.6,0.7,1.2))))
test_that("Expecting for overall", expect_error(overall_suit(suit, interval=c(0.2,0.5,0.6,0.7,1.2))))
test_that("Expecting for overall", expect_error(overall_suit(suit, interval=c(-0.1,0.5,0.6,0.7,1.2))))

# suit
# min(suit$`Suitability Score`[1,])
# max(suit$`Suitability Score`[1,])
# mean(suit[[1L]])
# overall_suit(suit, method="product")