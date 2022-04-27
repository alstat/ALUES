library(testthat)
library(ALUES)

suit_ <- suitability(MarinduqueLT, BANANASoil, interval="unbias")
test_that("Overall Minimum", expect_equal(min(suit_$`Suitability Score`[1,]), overall_suit(suit_, method="minimum")[1,1]))
test_that("Overall Minimum", expect_equal(max(suit_$`Suitability Score`[1,]), overall_suit(suit_, method="maximum")[1,1]))

# Weighted Average
wts <- suit_$`Factors' Weights`
wts[is.na(wts)] <- max(wts, na.rm = TRUE) + 1
new_wts <- (sum(wts) - wts)
new_wts <- new_wts/sum(new_wts)
sum(suit_$`Suitability Score`[1,] * new_wts, na.rm = TRUE)

test_that("Overall Minimum", expect_equal(sum(suit_$`Suitability Score`[1,] * new_wts, na.rm = TRUE), overall_suit(suit_, method="average")[1,1]))

# Average
suit_ <- suitability(MarinduqueLT, ALFALFASoil, interval="unbias")
test_that("Overall Minimum", expect_equal(min(suit_$`Suitability Score`[1,], na.rm=TRUE), overall_suit(suit_, method="minimum")[1,1]))
test_that("Overall Maximum", expect_equal(max(suit_$`Suitability Score`[1,], na.rm=TRUE), overall_suit(suit_, method="maximum")[1,1]))
test_that("Overall Average", expect_equal(mean(as.numeric(suit_$`Suitability Score`[1,]), na.rm=TRUE), overall_suit(suit_, method="average")[1,1]))

# Interval tests
suit_ <- suitability(MarinduqueLT, ALFALFASoil, interval="unbias")
test_that("Expecting for overall", expect_error(overall_suit(suit_, interval=c(0,2))))
test_that("Expecting for overall", expect_error(overall_suit(suit_, interval=c(0,0.5,0.6,0.7,0.9))))
test_that("Expecting for overall", expect_error(overall_suit(suit_, interval=c(0,0.5,0.6,0.7,1.2))))
test_that("Expecting for overall", expect_error(overall_suit(suit_, interval=c(0.2,0.5,0.6,0.7,1.2))))
test_that("Expecting for overall", expect_error(overall_suit(suit_, interval=c(-0.1,0.5,0.6,0.7,1.2))))