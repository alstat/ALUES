library(testthat)
library(ALUES)

# NA PRESENCE
suit_ <- suitability(LaoCaiLT, ALFALFASoil)
test_that("Parameter name present:", expect_equal(suit_$`Factors Evaluated`[1], "SoilTe"))
test_that("Parameter scores NA all:", 
          expect_equal(sum(is.na(suit_$`Suitability Score`["SoilTe"])), 
          nrow(suit_$`Suitability Score`["SoilTe"])))
test_that("Parameter classes NA all:", 
          expect_equal(sum(is.na(suit_$`Suitability Class`["SoilTe"])), 
                       nrow(suit_$`Suitability Class`["SoilTe"])))
test_that("Parameter Minimum NA:", 
          expect_true(is.na(suit_$`Factors' Minimum Values`["SoilTe"])))
test_that("Parameter Maximum NA:", 
          expect_true(is.na(suit_$`Factors' Maximum Values`["SoilTe"])))

# WARNINGS
test_that("Expecting Warning", expect_warning(suitability(LaoCaiLT, ALFALFASoil)))

# ------------------------------
# CASE A
# ------------------------------
# Right Face Triangular MF

# bias intervals
l1 <- 0; l2 <- 0.25; l3 <- 0.5; l4 <- 0.75; l5 <- 1

suit_ <- suitability(LaoCaiLT, SOYASoil)
right_tri <- function () {
  x <- LaoCaiLT[6,"CFragm"]; Min <- 0
  reqScore <- as.numeric(SOYASoil[1,2:7])
  clnScore <- rev(reqScore[complete.cases(reqScore)])
  Max <- clnScore[length(clnScore)] + ((diff(clnScore[1:2]) + diff(clnScore[2:3])) / 2)
  score <- (Max - x) / (Max - Min)
  if ((score >= l1) && (score < l2)) {
    class_ <- "N"
  } else if ((score >= l2) && (score < l3)) {
    class_ <- "S3"
  } else if ((score >= l3) && (score < l4)) {
    class_ <- "S2"
  } else if ((score >= l4) && (score <= l5)) {
    class_ <- "S1"
  } else {
    class_ <- "NA"
  }
  return(list("score" = score, "class" = class_))
}
test_that("Case A: Triangular", expect_equal(suit_$`Suitability Score`["CFragm"][6,], right_tri()[["score"]]))
test_that("Case A: Triangular", expect_equal(suit_$`Suitability Class`["CFragm"][6,], right_tri()[["class"]]))

# Right Face Trapezoidal MF
suit_ <- suitability(LaoCaiLT, SOYASoil, mf="trapezoidal")
right_tra <- function () {
  x <- LaoCaiLT[6,"CFragm"]; Min <- 0
  reqScore <- as.numeric(SOYASoil[1,2:7])
  clnScore <- rev(reqScore[complete.cases(reqScore)])
  Max <- clnScore[length(clnScore)] + ((diff(clnScore[1:2]) + diff(clnScore[2:3])) / 2)
  if (x < clnScore[1]) {
    return(1)
  }
  
}
test_that("Case A: Trapezoidal", expect_equal(suit_$`Suitability Score`["CFragm"][6,], right_tra()))

# Right Face Gaussian MF
suit_ <- suitability(LaoCaiLT[5:6,], SOYASoil[1:3,], mf="gaussian")
right_gau<- function () {
  x <- LaoCaiLT[5:6,"CFragm"]; sigma <- 1
  reqScore <- as.numeric(SOYASoil[1,2:7])
  clnScore <- rev(reqScore[complete.cases(reqScore)])
  Min <- 0
  score <- exp((-1 / 2) * `^`(((x - Min) / sigma), 2))
  class_ <- c()
  for (i in 1:length(score)) {
    if ((score[i] >= l1) && (score[i] < l2)) {
      class_[i] <- "N"
    } else if ((score[i] >= l2) && (score[i] < l3)) {
      class_[i] <- "S3"
    } else if ((score[i] >= l3) && (score[i] < l4)) {
      class_[i] <- "S2"
    } else if ((score[i] >= l4) && (score[i] <= l5)) {
      class_[i] <- "S1"
    } else {
      class_[i] <- "NA"
    }    
  }
  
  return(list("score" = score, "class" = class_))
}
test_that("Case A: Gaussian", expect_equal(as.numeric(suit_$`Suitability Score`[,"CFragm"]), right_gau()[["score"]]))
test_that("Case A: Gaussian", expect_equal(as.character(suit_$`Suitability Class`[,"CFragm"]), right_gau()[["class"]]))

# ------------------------------
# CASE B
# ------------------------------
# Left Face Triangular MF
suit_ <- suitability(LaoCaiLT[122:124,], SOYASoil[8:nrow(SOYASoil),])
left_tri <- function() {
  x <- LaoCaiLT[122,"OC"]
  reqScore <- as.numeric(SOYASoil[8,2:7])
  clnScore <- reqScore[complete.cases(reqScore)]
  Max <- clnScore[length(clnScore)] + ((diff(clnScore[1:2]) + diff(clnScore[2:3])) / 2)
  Min <- 0
  score <- (x - Min) / (Max - Min)
  if ((score >= l1) && (score < l2)) {
    class_ <- "N"
  } else if ((score >= l2) && (score < l3)) {
    class_ <- "S3"
  } else if ((score >= l3) && (score < l4)) {
    class_ <- "S2"
  } else if ((score >= l4) && (score <= l5)) {
    class_ <- "S1"
  } else {
    class_ <- "NA"
  }
  return(list("score" = score, "class" = class_))
}
test_that("Case B: Triangular", expect_equal(suit_$`Suitability Score`[1,"OC"], left_tri()[["score"]]))
test_that("Case B: Triangular", expect_equal(suit_$`Suitability Class`[1,"OC"], left_tri()[["class"]]))

# Left Face Trapezoidal MF
suit_ <- suitability(LaoCaiLT[122:124,], SOYASoil[8:nrow(SOYASoil),], mf="trapezoidal")
left_tra <- function() {
  x <- LaoCaiLT[122,"OC"]
  reqScore <- as.numeric(SOYASoil[8,2:7])
  clnScore <- reqScore[complete.cases(reqScore)]
  Min <- 0
  score <- (x - Min) / (clnScore[length(clnScore)] - Min)
  if ((score >= l1) && (score < l2)) {
    class_ <- "N"
  } else if ((score >= l2) && (score < l3)) {
    class_ <- "S3"
  } else if ((score >= l3) && (score < l4)) {
    class_ <- "S2"
  } else if ((score >= l4) && (score <= l5)) {
    class_ <- "S1"
  } else {
    class_ <- "NA"
  }
  return(list("score" = score, "class" = class_))
}
test_that("Case B: Trapezoidal", expect_equal(suit_$`Suitability Score`[1,"OC"], left_tra()[["score"]]))
test_that("Case B: Trapezoidal", expect_equal(suit_$`Suitability Class`[1,"OC"], left_tra()[["class"]]))

# Left Face Gaussian MF
suit_ <- suitability(LaoCaiLT[122:124,], SOYASoil[8:nrow(SOYASoil),], mf="gaussian")
left_gau <- function() {
  x <- LaoCaiLT[122,"OC"]; sigma <- 1
  reqScore <- as.numeric(SOYASoil[8,2:7])
  clnScore <- reqScore[complete.cases(reqScore)]
  Max <- clnScore[length(clnScore)] + ((diff(clnScore[1:2]) + diff(clnScore[2:3])) / 2)
  score <- exp((-1 / 2) * (((x - Max) / sigma)^2))
  if ((score >= l1) && (score < l2)) {
    class_ <- "N"
  } else if ((score >= l2) && (score < l3)) {
    class_ <- "S3"
  } else if ((score >= l3) && (score < l4)) {
    class_ <- "S2"
  } else if ((score >= l4) && (score <= l5)) {
    class_ <- "S1"
  } else {
    class_ <- "NA"
  }
  return(list("score" = score, "class" = class_))
}
test_that("Case B: Gaussian", expect_equal(suit_$`Suitability Score`[1,"OC"], left_gau()[["score"]]))
test_that("Case B: Gaussian", expect_equal(suit_$`Suitability Class`[1,"OC"], left_gau()[["class"]]))

# ------------------------------
# CASE C
# ------------------------------
# Full Triangular
suit_ <- suitability(MarinduqueLT, SAFFLOWERSoil)
full_tri <- function () {
  x <- MarinduqueLT[6,"pHH2O"]; Min <- 0
  reqScore <- as.numeric(SAFFLOWERSoil[6,2:7])
  clnScore <- reqScore[complete.cases(reqScore)]
  Max <- clnScore[length(clnScore)] + ((diff(clnScore[1:2]) + diff(clnScore[2:3])) / 2)
  if (x > Max) {
    return(0)
  }
}
test_that("Case C: Triangular", expect_equal(suit_$`Suitability Score`["pHH2O"][6,], full_tri()))

MarinduqueLTNew <- tail(MarinduqueLT)
MarinduqueLTNew[1, "pHH2O"] <- 7.6
MarinduqueLTNew[2, "pHH2O"] <- 5.6
suit_ <- suitability(MarinduqueLTNew, SAFFLOWERSoil)
full_tri <- function (r) {
  x <- MarinduqueLTNew[r,"pHH2O"]; Min <- 0
  reqScore <- as.numeric(SAFFLOWERSoil[6,2:7])
  clnScore <- reqScore[complete.cases(reqScore)]
  Max <- reqScore[6] + ((diff(reqScore[1:2]) + diff(reqScore[2:3]) + diff(reqScore[3:4]) + diff(reqScore[4:5]) + diff(reqScore[5:6])) / 5)
  Mid <- mean(reqScore[3:4])
  Min <- 0
  if (x > Max) {
    score <- 0
  } else if (x > Mid) {
    score <- (Max - x) / (Max - Mid)
  } else if (x <= Mid) {
    score <- (x - Min) / (Mid - Min)
  }
  if ((score >= l1) && (score < l2)) {
    class_ <- "N"
  } else if ((score >= l2) && (score < l3)) {
    class_ <- "S3"
  } else if ((score >= l3) && (score < l4)) {
    class_ <- "S2"
  } else if ((score >= l4) && (score <= l5)) {
    class_ <- "S1"
  } else {
    class_ <- "NA"
  }
  return(list("score" = score, "class" = class_))
}
test_that("Case C: Triangular", expect_equal(suit_$`Suitability Score`["pHH2O"][1,], full_tri(1)[["score"]]))
test_that("Case C: Triangular", expect_equal(suit_$`Suitability Class`["pHH2O"][1,], full_tri(1)[["class"]]))
test_that("Case C: Triangular", expect_equal(suit_$`Suitability Score`["pHH2O"][2,], full_tri(2)[["score"]]))
test_that("Case C: Triangular", expect_equal(suit_$`Suitability Class`["pHH2O"][2,], full_tri(2)[["class"]]))

# Full Trapezoidal
suit_ <- suitability(MarinduqueLT, SAFFLOWERSoil, mf="trapezoidal")
full_tra <- function () {
  x <- MarinduqueLT[6,"pHH2O"]; Min <- 0
  reqScore <- as.numeric(SAFFLOWERSoil[6,2:7])
  clnScore <- reqScore[complete.cases(reqScore)]
  Max <- clnScore[length(clnScore)] + ((diff(clnScore[1:2]) + diff(clnScore[2:3])) / 2)
  if (x > Max) {
    return(0)
  }
}
test_that("Case C: Trapezoidal", expect_equal(suit_$`Suitability Score`["pHH2O"][6,], full_tra()))

suit_ <- suitability(MarinduqueLTNew, SAFFLOWERSoil, mf="trapezoidal")
full_tra <- function (r) {
  x <- MarinduqueLTNew[r,"pHH2O"]; Min <- 0
  reqScore <- as.numeric(SAFFLOWERSoil[6,2:7])
  clnScore <- reqScore[complete.cases(reqScore)]
  Max <- reqScore[6] + ((diff(reqScore[1:2]) + diff(reqScore[2:3]) + diff(reqScore[3:4]) + diff(reqScore[4:5]) + diff(reqScore[5:6])) / 5)
  Mid <- mean(reqScore[3:4])
  Min <- 0
  if (x > Max) {
    score <- 0
  } else if (x > reqScore[4]) {
    score <- (Max - x) / (Max - reqScore[4])
  } else if (x <= reqScore[3]) {
    score <- (x - Min) / (reqScore[3] - Min)
  }
  
  if ((score >= l1) && (score < l2)) {
    class_ <- "N"
  } else if ((score >= l2) && (score < l3)) {
    class_ <- "S3"
  } else if ((score >= l3) && (score < l4)) {
    class_ <- "S2"
  } else if ((score >= l4) && (score <= l5)) {
    class_ <- "S1"
  } else {
    class_ <- "NA"
  }
  return(list("score" = score, "class" = class_))
}
test_that("Case C: Trapezoidal", expect_equal(suit_$`Suitability Score`["pHH2O"][1,], full_tra(1)[["score"]]))
test_that("Case C: Trapezoidal", expect_equal(suit_$`Suitability Class`["pHH2O"][1,], full_tra(1)[["class"]]))
test_that("Case C: Trapezoidal", expect_equal(suit_$`Suitability Score`["pHH2O"][2,], full_tra(2)[["score"]]))
test_that("Case C: Trapezoidal", expect_equal(suit_$`Suitability Class`["pHH2O"][2,], full_tra(2)[["class"]]))

# Full Gaussian
suit_ <- suitability(MarinduqueLTNew, SAFFLOWERSoil[6,], mf="gaussian")
full_gau <- function (r) {
  x <- MarinduqueLTNew[r,"pHH2O"]; Min <- 0; sigma <- 1
  reqScore <- as.numeric(SAFFLOWERSoil[6,2:7])
  clnScore <- reqScore[complete.cases(reqScore)]
  Max <- reqScore[6] + ((diff(reqScore[1:2]) + diff(reqScore[2:3]) + diff(reqScore[3:4]) + diff(reqScore[4:5]) + diff(reqScore[5:6])) / 5)
  Mid <- mean(reqScore[3:4])
  Min <- 0
  score <- exp((-1 / 2) * (((x - Mid) / sigma)^2))
  if ((score >= l1) && (score < l2)) {
    class_ <- "N"
  } else if ((score >= l2) && (score < l3)) {
    class_ <- "S3"
  } else if ((score >= l3) && (score < l4)) {
    class_ <- "S2"
  } else if ((score >= l4) && (score <= l5)) {
    class_ <- "S1"
  } else {
    class_ <- "NA"
  }
  return(list("score" = score, "class" = class_))
}
test_that("Case C: Gaussian", expect_equal(suit_$`Suitability Score`["pHH2O"][1,], full_gau(1)[["score"]]))
test_that("Case C: Gaussian", expect_equal(suit_$`Suitability Class`["pHH2O"][1,], full_gau(1)[["class"]]))
test_that("Case C: Gaussian", expect_equal(suit_$`Suitability Score`["pHH2O"][2,], full_gau(2)[["score"]]))
test_that("Case C: Gaussian", expect_equal(suit_$`Suitability Class`["pHH2O"][2,], full_gau(2)[["class"]]))

# ------------------------------
# CASE D
# ------------------------------
# Triangular
MarinduqueLTNew <- MarinduqueLT[7:8,]
suit_ <- suitability(MarinduqueLTNew, BAMBOOSoil)
full_tri <- function (r) {
  x <- MarinduqueLTNew[r,"SoilTe"]; Min <- 0
  reqScore <- as.numeric(BAMBOOSoil[3,2:7])
  clnScore <- reqScore[complete.cases(reqScore)]
  
  Max <- reqScore[5]
  Mid <- mean(reqScore[3:4])
  Min <- 0
  if (x > Max) {
    score <- 0
  } else if (x > Mid) {
    score <- (Max - x) / (Max - Mid)
  } else if (x <= Mid) {
    score <- (x - Min) / (Mid - Min)
  }
  if ((score >= l1) && (score < l2)) {
    class_ <- "N"
  } else if ((score >= l2) && (score < l3)) {
    class_ <- "S3"
  } else if ((score >= l3) && (score < l4)) {
    class_ <- "S2"
  } else if ((score >= l4) && (score <= l5)) {
    class_ <- "S1"
  } else {
    class_ <- "NA"
  }
  return(list("score" = score, "class" = class_))
}
test_that("Case D: Triangular", expect_equal(suit_$`Suitability Score`["SoilTe"][1,], full_tri(1)[["score"]]))
test_that("Case D: Triangular", expect_equal(suit_$`Suitability Class`["SoilTe"][1,], full_tri(1)[["class"]]))
test_that("Case D: Triangular", expect_equal(suit_$`Suitability Score`["SoilTe"][2,], full_tri(2)[["score"]]))
test_that("Case D: Triangular", expect_equal(suit_$`Suitability Class`["SoilTe"][2,], full_tri(2)[["class"]]))

MarinduqueLTNew2 <- MarinduqueLT[7:8,]
MarinduqueLTNew2[1, 6] <- 5
suit_ <- suitability(MarinduqueLTNew2, ALUES::BAMBOOSoil)
full_tri <- function (r) {
  x <- MarinduqueLTNew2[r,"SoilTe"]; Min <- 0
  reqScore <- as.numeric(ALUES::BAMBOOSoil[3,2:7])
  clnScore <- reqScore[complete.cases(reqScore)]
  
  Max <- reqScore[5]
  Mid <- mean(reqScore[3:4])
  Min <- 0
  if (x > Max) {
    score <- 0
  } else if (x > Mid) {
    score <- (Max - x) / (Max - Mid)
    if ((score >= l3) && (score < l4)) {
      class_ <- "S2"
    } else if ((score >= l4) && (score <= l5)) {
      class_ <- "S1"
    } else {
      class_ <- "N"
    }
  } else if (x <= Mid) {
    score <- (x - Min) / (Mid - Min)
    if ((score >= l1) && (score < l2)) {
      class_ <- "N"
    } else if ((score >= l2) && (score < l3)) {
      class_ <- "S3"
    } else if ((score >= l3) && (score < l4)) {
      class_ <- "S2"
    } else if ((score >= l4) && (score <= l5)) {
      class_ <- "S1"
    } else {
      class_ <- "NA"
    }
  }
  
  return(list("score" = score, "class" = class_))
}
test_that("Case D: Triangular", expect_equal(suit_$`Suitability Score`["SoilTe"][1,], full_tri(1)[["score"]]))
test_that("Case D: Triangular", expect_equal(suit_$`Suitability Class`["SoilTe"][1,], full_tri(1)[["class"]]))
test_that("Case D: Triangular", expect_equal(suit_$`Suitability Score`["SoilTe"][2,], full_tri(2)[["score"]]))
test_that("Case D: Triangular", expect_equal(suit_$`Suitability Class`["SoilTe"][2,], full_tri(2)[["class"]]))

# Trapezoidal
suit_ <- suitability(MarinduqueLTNew, BAMBOOSoil, mf="trapezoidal")
full_tra <- function (r) {
  x <- MarinduqueLTNew[r,"SoilTe"]; Min <- 0
  reqScore <- as.numeric(BAMBOOSoil[3,2:7])
  clnScore <- reqScore[complete.cases(reqScore)]
  Max <- reqScore[5]
  Mid <- mean(reqScore[3:4])
  Min <- 0
  if (x > Max) {
    score <- 0
  } else if (x > reqScore[4]) {
    score <- (Max - x) / (Max - reqScore[4])
  } else if (x <= reqScore[3]) {
    score <- (x - Min) / (reqScore[3] - Min)
  } else if ((x > reqScore[3]) && (x <= reqScore[4])) {
    score <- 1
  }
  
  if ((score >= l1) && (score < l2)) {
    class_ <- "N"
  } else if ((score >= l2) && (score < l3)) {
    class_ <- "S3"
  } else if ((score >= l3) && (score < l4)) {
    class_ <- "S2"
  } else if ((score >= l4) && (score <= l5)) {
    class_ <- "S1"
  } else {
    class_ <- "NA"
  }
  return(list("score" = score, "class" = class_))
}
test_that("Case D: Trapezoidal", expect_equal(suit_$`Suitability Score`["SoilTe"][1,], full_tra(1)[["score"]]))
test_that("Case D: Trapezoidal", expect_equal(suit_$`Suitability Class`["SoilTe"][1,], full_tra(1)[["class"]]))
test_that("Case D: Trapezoidal", expect_equal(suit_$`Suitability Score`["SoilTe"][2,], full_tra(2)[["score"]]))
test_that("Case D: Trapezoidal", expect_equal(suit_$`Suitability Class`["SoilTe"][2,], full_tra(2)[["class"]]))

suit_ <- suitability(MarinduqueLTNew2, ALUES::BAMBOOSoil, mf="trapezoidal")
full_tra <- function (r) {
  x <- MarinduqueLTNew2[r,"SoilTe"]; Min <- 0
  reqScore <- as.numeric(ALUES::BAMBOOSoil[3,2:7])
  clnScore <- reqScore[complete.cases(reqScore)]
  Max <- reqScore[5]
  Mid <- mean(reqScore[3:4])
  Min <- 0
  if (x > Max) {
    score <- 0
  } else if (x > reqScore[4]) {
    score <- (Max - x) / (Max - reqScore[4])
  } else if (x <= reqScore[3]) {
    score <- (x - Min) / (reqScore[3] - Min)
  } else if ((x > reqScore[3]) && (x <= reqScore[4])) {
    score <- 1
  }
  
  if ((score >= l1) && (score < l2)) {
    class_ <- "N"
  } else if ((score >= l2) && (score < l3)) {
    class_ <- "S3"
  } else if ((score >= l3) && (score < l4)) {
    class_ <- "S2"
  } else if ((score >= l4) && (score <= l5)) {
    class_ <- "S1"
  } else {
    class_ <- "NA"
  }
  return(list("score" = score, "class" = class_))
}
test_that("Case D: Trapezoidal", expect_equal(suit_$`Suitability Score`["SoilTe"][1,], full_tra(1)[["score"]]))
test_that("Case D: Trapezoidal", expect_equal(suit_$`Suitability Class`["SoilTe"][1,], full_tra(1)[["class"]]))
test_that("Case D: Trapezoidal", expect_equal(suit_$`Suitability Score`["SoilTe"][2,], full_tra(2)[["score"]]))
test_that("Case D: Trapezoidal", expect_equal(suit_$`Suitability Class`["SoilTe"][2,], full_tra(2)[["class"]]))

MarinduqueLTNew3 <- MarinduqueLTNew2
MarinduqueLTNew3[1, 6] <- 8.3
suit_ <- suitability(MarinduqueLTNew3, ALUES::BAMBOOSoil, mf="trapezoidal")
full_tra <- function (r) {
  x <- MarinduqueLTNew3[r,"SoilTe"]; Min <- 0
  reqScore <- as.numeric(ALUES::BAMBOOSoil[3,2:7])
  clnScore <- reqScore[complete.cases(reqScore)]
  Max <- reqScore[5]
  Mid <- mean(reqScore[3:4])
  Min <- 0
  if (x > Max) {
    score <- 0
  } else if ((x > reqScore[4]) && (x <= Max)) {
    score <- (Max - x) / (Max - reqScore[4])
    if ((score >= l3) && (score < l4)) {
      class_ <- "S2"
    } else if (score >= l4) {
      class_ <- "S1"
    } else {
      class_ <- "N"
    }
  } else if (x <= reqScore[3]) {
    score <- (x - Min) / (reqScore[3] - Min)
    if ((score >= l1) && (score < l2)) {
      class_ <- "N"
    } else if ((score >= l2) && (score < l3)) {
      class_ <- "S3"
    } else if ((score >= l3) && (score < l4)) {
      class_ <- "S2"
    } else if ((score >= l4) && (score <= l5)) {
      class_ <- "S1"
    } else {
      class_ <- "NA"
    }
  } else if ((x > reqScore[3]) && (x <= reqScore[4])) {
    score <- 1; class_ <- "S1"
  }
  
  return(list("score" = score, "class" = class_))
}
test_that("Case D: Trapezoidal", expect_equal(suit_$`Suitability Score`["SoilTe"][1,], full_tra(1)[["score"]]))
test_that("Case D: Trapezoidal", expect_equal(suit_$`Suitability Class`["SoilTe"][1,], full_tra(1)[["class"]]))
test_that("Case D: Trapezoidal", expect_equal(suit_$`Suitability Score`["SoilTe"][2,], full_tra(2)[["score"]]))
test_that("Case D: Trapezoidal", expect_equal(suit_$`Suitability Class`["SoilTe"][2,], full_tra(2)[["class"]]))

# Gaussian
suit_ <- suitability(MarinduqueLTNew, BAMBOOSoil, mf="gaussian")
full_gau <- function (r) {
  x <- MarinduqueLTNew[r,"SoilTe"]; Min <- 0; sigma <- 1
  reqScore <- as.numeric(BAMBOOSoil[3,2:7])
  clnScore <- reqScore[complete.cases(reqScore)]
  Max <- reqScore[5]
  Mid <- mean(reqScore[3:4])
  Min <- 0
  score <- exp((-1 / 2) * (((x - Mid) / sigma)^2))
  if ((score >= l1) && (score < l2)) {
    class_ <- "N"
  } else if ((score >= l2) && (score < l3)) {
    class_ <- "S3"
  } else if ((score >= l3) && (score < l4)) {
    class_ <- "S2"
  } else if ((score >= l4) && (score <= l5)) {
    class_ <- "S1"
  } else {
    class_ <- "NA"
  }
  return(list("score" = score, "class" = class_))
}
test_that("Case D: Gaussian", expect_equal(suit_$`Suitability Score`["SoilTe"][1,], full_gau(1)[["score"]]))
test_that("Case D: Gaussian", expect_equal(suit_$`Suitability Class`["SoilTe"][1,], full_gau(1)[["class"]]))
test_that("Case D: Gaussian", expect_equal(suit_$`Suitability Score`["SoilTe"][2,], full_gau(2)[["score"]]))
test_that("Case D: Gaussian", expect_equal(suit_$`Suitability Class`["SoilTe"][2,], full_gau(2)[["class"]]))

suit_ <- suitability(MarinduqueLTNew2, ALUES::BAMBOOSoil, mf="gaussian")
full_gau <- function (r) {
  x <- MarinduqueLTNew2[r,"SoilTe"]; Min <- 0; sigma <- 1
  reqScore <- as.numeric(ALUES::BAMBOOSoil[3,2:7])
  clnScore <- reqScore[complete.cases(reqScore)]
  Max <- reqScore[5]
  Mid <- mean(reqScore[3:4])
  Min <- 0
  score <- exp((-1 / 2) * (((x - Mid) / sigma)^2))
  if ((score >= l1) && (score < l2)) {
    class_ <- "N"
  } else if ((score >= l2) && (score < l3)) {
    class_ <- "S3"
  } else if ((score >= l3) && (score < l4)) {
    class_ <- "S2"
  } else if ((score >= l4) && (score <= l5)) {
    class_ <- "S1"
  } else {
    class_ <- "NA"
  }
  return(list("score" = score, "class" = class_))
}
test_that("Case D: Gaussian", expect_equal(suit_$`Suitability Score`["SoilTe"][1,], full_gau(1)[["score"]]))
test_that("Case D: Gaussian", expect_equal(suit_$`Suitability Class`["SoilTe"][1,], full_gau(1)[["class"]]))
test_that("Case D: Gaussian", expect_equal(suit_$`Suitability Score`["SoilTe"][2,], full_gau(2)[["score"]]))
test_that("Case D: Gaussian", expect_equal(suit_$`Suitability Class`["SoilTe"][2,], full_gau(2)[["class"]]))

# ------------------------------
# CASE E
# ------------------------------
# Triangular
MarinduqueLTNew <- MarinduqueLT[7:8,]
BAMBOOSoil[3, 6] <- NA
BAMBOOSoil[3, 5] <- 9.0
BAMBOOSoilNew <- BAMBOOSoil
suit_ <- suitability(MarinduqueLTNew, BAMBOOSoilNew)
full_tri <- function (r) {
  x <- MarinduqueLTNew[r,"SoilTe"]; Min <- 0
  reqScore <- as.numeric(BAMBOOSoilNew[3,2:7])
  clnScore <- reqScore[complete.cases(reqScore)]
  
  Max <- reqScore[4]
  Mid <- mean(reqScore[3:4])
  Min <- 0
  if (x > Max) {
    score <- 0
  } else if (x > Mid) {
    score <- (Max - x) / (Max - Mid)
  } else if (x <= Mid) {
    score <- (x - Min) / (Mid - Min)
  }
  if ((score >= l4) && (score <= l5)) {
    class_ <- "S1"
  } else {
    class_ <- "N"
  }
  return(list("score" = score, "class" = class_))
}
test_that("Case E: Triangular", expect_equal(suit_$`Suitability Score`["SoilTe"][1,], full_tri(1)[["score"]]))
test_that("Case E: Triangular", expect_equal(suit_$`Suitability Class`["SoilTe"][1,], full_tri(1)[["class"]]))
test_that("Case E: Triangular", expect_equal(suit_$`Suitability Score`["SoilTe"][2,], full_tri(2)[["score"]]))
test_that("Case E: Triangular", expect_equal(suit_$`Suitability Class`["SoilTe"][2,], full_tri(2)[["class"]]))

# Triangular
MarinduqueLTNew2 <- MarinduqueLT[7:8,]
MarinduqueLTNew2[1, 6] <- 5
suit_ <- suitability(MarinduqueLTNew2, BAMBOOSoilNew)
full_tri <- function (r) {
  x <- MarinduqueLTNew2[r,"SoilTe"]; Min <- 0
  reqScore <- as.numeric(BAMBOOSoilNew[3,2:7])
  clnScore <- reqScore[complete.cases(reqScore)]
  
  Max <- reqScore[4]
  Mid <- mean(reqScore[3:4])
  Min <- 0
  if (x > Max) {
    score <- 0
  } else if (x > Mid) {
    score <- (Max - x) / (Max - Mid)
    if ((score >= l4) && (score <= l5)) {
      class_ <- "S1"
    } else {
      class_ <- "N"
    }
  } else if ((x > Min) && (x <= Mid)) {
    score <- (x - Min) / (Mid - Min)
    if ((score >= l1) && (score < l2)) {
      class_ <- "N"
    } else if ((score >= l2) && (score < l3)) {
      class_ <- "S3"
    } else if ((score >= l3) && (score < l4)) {
      class_ <- "S2"
    } else if ((score >= l4) && (score <= l5)) {
      class_ <- "S1"
    } else {
      class_ <- "NA"
    }
  }
 
  return(list("score" = score, "class" = class_))
}
test_that("Case E: Triangular", expect_equal(suit_$`Suitability Score`["SoilTe"][1,], full_tri(1)[["score"]]))
test_that("Case E: Triangular", expect_equal(suit_$`Suitability Class`["SoilTe"][1,], full_tri(1)[["class"]]))
test_that("Case E: Triangular", expect_equal(suit_$`Suitability Score`["SoilTe"][2,], full_tri(2)[["score"]]))
test_that("Case E: Triangular", expect_equal(suit_$`Suitability Class`["SoilTe"][2,], full_tri(2)[["class"]]))

# Trapezoidal
suit_ <- suitability(MarinduqueLTNew, BAMBOOSoilNew, mf="trapezoidal")
full_tra <- function (r) {
  x <- MarinduqueLTNew[r,"SoilTe"]; Min <- 0
  reqScore <- as.numeric(BAMBOOSoil[3,2:7])
  clnScore <- reqScore[complete.cases(reqScore)]
  Max <- reqScore[4]
  Mid <- mean(reqScore[3:4])
  Min <- 0
  if (x > Max) {
    score <- 0
  } else if (x > reqScore[4]) {
    score <- (Max - x) / (Max - reqScore[4])
  } else if (x <= reqScore[3]) {
    score <- (x - Min) / (reqScore[3] - Min)
    if ((score >= l1) && (score < l2)) {
      class_ <- "N"
    } else if ((score >= l2) && (score < l3)) {
      class_ <- "S3"
    } else if ((score >= l3) && (score < l4)) {
      class_ <- "S2"
    } else if ((score >= l4) && (score <= l5)) {
      class_ <- "S1"
    } else {
      class_ <- "NA"
    }
  } else if ((x > reqScore[3]) && (x <= reqScore[4])) {
    score <- 1
  }
  
  if ((score >= l4) && (score <= l5)) {
    class_ <- "S1"
  } else {
    class_ <- "N"
  }
  return(list("score" = score, "class" = class_))
}
test_that("Case E: Trapezoidal", expect_equal(suit_$`Suitability Score`["SoilTe"][1,], full_tra(1)[["score"]]))
test_that("Case E: Trapezoidal", expect_equal(suit_$`Suitability Class`["SoilTe"][1,], full_tra(1)[["class"]]))
test_that("Case E: Trapezoidal", expect_equal(suit_$`Suitability Score`["SoilTe"][2,], full_tra(2)[["score"]]))
test_that("Case E: Trapezoidal", expect_equal(suit_$`Suitability Class`["SoilTe"][2,], full_tra(2)[["class"]]))

suit_ <- suitability(MarinduqueLTNew2, BAMBOOSoilNew, mf="trapezoidal")
full_tra <- function (r) {
  x <- MarinduqueLTNew2[r,"SoilTe"]; Min <- 0
  reqScore <- as.numeric(BAMBOOSoil[3,2:7])
  clnScore <- reqScore[complete.cases(reqScore)]
  Max <- reqScore[4]
  Mid <- mean(reqScore[3:4])
  Min <- 0
  if (x > Max) {
    score <- 0
  } else if (x > reqScore[4]) {
    score <- (Max - x) / (Max - reqScore[4])
  } else if (x <= reqScore[3]) {
    score <- (x - Min) / (reqScore[3] - Min)
    if ((score >= l1) && (score < l2)) {
      class_ <- "N"
    } else if ((score >= l2) && (score < l3)) {
      class_ <- "S3"
    } else if ((score >= l3) && (score < l4)) {
      class_ <- "S2"
    } else if ((score >= l4) && (score <= l5)) {
      class_ <- "S1"
    } else {
      class_ <- "NA"
    }
  } else if ((x > reqScore[3]) && (x <= reqScore[4])) {
    score <- 1; class_ = "S1"
  }
  
  
  return(list("score" = score, "class" = class_))
}
test_that("Case E: Trapezoidal", expect_equal(suit_$`Suitability Score`["SoilTe"][1,], full_tra(1)[["score"]]))
test_that("Case E: Trapezoidal", expect_equal(suit_$`Suitability Class`["SoilTe"][1,], full_tra(1)[["class"]]))
test_that("Case E: Trapezoidal", expect_equal(suit_$`Suitability Score`["SoilTe"][2,], full_tra(2)[["score"]]))
test_that("Case E: Trapezoidal", expect_equal(suit_$`Suitability Class`["SoilTe"][2,], full_tra(2)[["class"]]))

# Gaussian
suit_ <- suitability(MarinduqueLTNew, BAMBOOSoilNew, mf="gaussian")
full_gau <- function (r) {
  x <- MarinduqueLTNew[r,"SoilTe"]; Min <- 0; sigma <- 1
  reqScore <- as.numeric(BAMBOOSoilNew[3,2:7])
  clnScore <- reqScore[complete.cases(reqScore)]
  Max <- reqScore[4]
  Mid <- mean(reqScore[3:4])
  Min <- 0
  score <- exp((-1 / 2) * (((x - Mid) / sigma)^2))
  if ((score >= l1) && (score < l2)) {
    class_ <- "N"
  } else if ((score >= l2) && (score < l3)) {
    class_ <- "S3"
  } else if ((score >= l3) && (score < l4)) {
    class_ <- "S2"
  } else if ((score >= l4) && (score <= l5)) {
    class_ <- "S1"
  } else {
    class_ <- "NA"
  }
  return(list("score" = score, "class" = class_))
}
test_that("Case E: Gaussian", expect_equal(suit_$`Suitability Score`["SoilTe"][1,], full_gau(1)[["score"]]))
test_that("Case E: Gaussian", expect_equal(suit_$`Suitability Class`["SoilTe"][1,], full_gau(1)[["class"]]))
test_that("Case E: Gaussian", expect_equal(suit_$`Suitability Score`["SoilTe"][2,], full_gau(2)[["score"]]))
test_that("Case E: Gaussian", expect_equal(suit_$`Suitability Class`["SoilTe"][2,], full_gau(2)[["class"]]))

suit_ <- suitability(MarinduqueLTNew2, BAMBOOSoilNew, mf="gaussian")
full_gau <- function (r) {
  x <- MarinduqueLTNew2[r,"SoilTe"]; Min <- 0; sigma <- 1
  reqScore <- as.numeric(BAMBOOSoilNew[3,2:7])
  clnScore <- reqScore[complete.cases(reqScore)]
  Max <- reqScore[4]
  Mid <- mean(reqScore[3:4])
  Min <- 0
  score <- exp((-1 / 2) * (((x - Mid) / sigma)^2))
  if ((score >= l1) && (score < l2)) {
    class_ <- "N"
  } else if ((score >= l2) && (score < l3)) {
    class_ <- "S3"
  } else if ((score >= l3) && (score < l4)) {
    class_ <- "S2"
  } else if ((score >= l4) && (score <= l5)) {
    class_ <- "S1"
  } else {
    class_ <- "NA"
  }
  return(list("score" = score, "class" = class_))
}
test_that("Case E: Gaussian", expect_equal(suit_$`Suitability Score`["SoilTe"][1,], full_gau(1)[["score"]]))
test_that("Case E: Gaussian", expect_equal(suit_$`Suitability Class`["SoilTe"][1,], full_gau(1)[["class"]]))
test_that("Case E: Gaussian", expect_equal(suit_$`Suitability Score`["SoilTe"][2,], full_gau(2)[["score"]]))
test_that("Case E: Gaussian", expect_equal(suit_$`Suitability Class`["SoilTe"][2,], full_gau(2)[["class"]]))