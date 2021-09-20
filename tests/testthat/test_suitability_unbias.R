library(testthat)
library(ALUES)

# NA PRESENCE
suit <- suitability(LaoCaiLT, ALFALFASoil, interval="unbias")
test_that("Parameter name present:", expect_equal(suit$`Factors Evaluated`[1], "SoilTe"))
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

LaoCaiLT2 <- LaoCaiLT[5:6,]
suit <- suitability(LaoCaiLT2, SOYASoil, interval="unbias")
right_tri <- function (r) {
  x <- LaoCaiLT2[r,"CFragm"]; Min <- 0
  reqScore <- as.numeric(SOYASoil[1,2:7])
  clnScore <- rev(reqScore[complete.cases(reqScore)])
  Max <- clnScore[length(clnScore)] + ((diff(clnScore[1:2]) + diff(clnScore[2:3])) / 2)
  score <- (Max - x) / (Max - Min)
  
  l1 = 0; l2 = (Max - clnScore[3]) / (Max - Min); l3 = (Max - clnScore[2]) / (Max - Min); l4 = (Max - clnScore[1]) / (Max - Min); l5 = 1;
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
test_that("Case A: Triangular", expect_equal(suit$`Suitability Score`["CFragm"][1,], right_tri(1)[["score"]]))
test_that("Case A: Triangular", expect_equal(suit$`Suitability Class`["CFragm"][1,], right_tri(1)[["class"]]))
test_that("Case A: Triangular", expect_equal(suit$`Suitability Score`["CFragm"][2,], right_tri(2)[["score"]]))
test_that("Case A: Triangular", expect_equal(suit$`Suitability Class`["CFragm"][2,], right_tri(2)[["class"]]))


LaoCaiLT3 <- LaoCaiLT[5:6,]
LaoCaiLT3[1,2] <- 80
suit <- suitability(LaoCaiLT3, SOYASoil, interval="unbias")
right_tri <- function (r) {
  x <- LaoCaiLT3[r,"CFragm"]; Min <- 0
  reqScore <- as.numeric(SOYASoil[1,2:7])
  clnScore <- rev(reqScore[complete.cases(reqScore)])
  Max <- clnScore[length(clnScore)] + ((diff(clnScore[1:2]) + diff(clnScore[2:3])) / 2)
  
  score <- (Max - x) / (Max - Min)
  
  l1 = 0; l2 = (Max - clnScore[3]) / (Max - Min); l3 = (Max - clnScore[2]) / (Max - Min); l4 = (Max - clnScore[1]) / (Max - Min); l5 = 1;
  if ((score < Min) || (score > Max)) {
    class_ <- "N"; score <- 0
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
test_that("Case A: Triangular", expect_equal(suit$`Suitability Score`["CFragm"][1,], right_tri(1)[["score"]]))
test_that("Case A: Triangular", expect_equal(suit$`Suitability Class`["CFragm"][1,], right_tri(1)[["class"]]))
test_that("Case A: Triangular", expect_equal(suit$`Suitability Score`["CFragm"][2,], right_tri(2)[["score"]]))
test_that("Case A: Triangular", expect_equal(suit$`Suitability Class`["CFragm"][2,], right_tri(2)[["class"]]))

# Right Face Trapezoidal MF
LaoCaiLT3 <- LaoCaiLT[5:6,]
LaoCaiLT3[1,2] <- 80
suit <- suitability(LaoCaiLT3, SOYASoil, mf="trapezoidal", interval="unbias")
right_tra <- function (r) {
  x <- LaoCaiLT3[r,"CFragm"]; Min <- 0
  reqScore <- as.numeric(SOYASoil[1,2:7])
  clnScore <- rev(reqScore[complete.cases(reqScore)])
  Max <- clnScore[length(clnScore)] + ((diff(clnScore[1:2]) + diff(clnScore[2:3])) / 2)
  if ((x >= Min) && (x < clnScore[1])) {
    score <- 1; class_ <- "S1"
  } else if ((x < Min) || (x > Max)) {
    score <- 0; class_ <- "N"
  }
  return(list("score" = score, "class" = class_))
}
test_that("Case A: Trapezoidal", expect_equal(suit$`Suitability Score`["CFragm"][1,], right_tra(1)[["score"]]))
test_that("Case A: Trapezoidal", expect_equal(suit$`Suitability Class`["CFragm"][1,], right_tra(1)[["class"]]))
test_that("Case A: Trapezoidal", expect_equal(suit$`Suitability Score`["CFragm"][2,], right_tra(2)[["score"]]))
test_that("Case A: Trapezoidal", expect_equal(suit$`Suitability Class`["CFragm"][2,], right_tra(2)[["class"]]))

LaoCaiLT3 <- LaoCaiLT[5:6,]
LaoCaiLT3[1,2] <- 80
LaoCaiLT3[2,2] <- 25
suit <- suitability(LaoCaiLT3, SOYASoil, mf="trapezoidal", interval="unbias")
right_tra <- function (r) {
  x <- LaoCaiLT3[r,"CFragm"]; Min <- 0
  reqScore <- as.numeric(SOYASoil[1,2:7])
  clnScore <- rev(reqScore[complete.cases(reqScore)])
  Max <- clnScore[length(clnScore)] + ((diff(clnScore[1:2]) + diff(clnScore[2:3])) / 2)
  if ((x >= Min) && (x < clnScore[1])) {
    score <- 1; class_ <- "S1"
  } else if ((x < Min) || (x > Max)) {
    score <- 0; class_ <- "N"
  } else if ((x > clnScore[1]) && (x <= Max)) {
    score <- (Max - x) / (Max - clnScore[1])
    l1 = 0; l2 = (Max - clnScore[3]) / (Max - clnScore[1]); l3 = (Max - clnScore[2]) / (Max - clnScore[1]); l4 = (Max - clnScore[1]) / (Max - clnScore[1]); l5 = 1;
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
test_that("Case A: Trapezoidal", expect_equal(suit$`Suitability Score`["CFragm"][1,], right_tra(1)[["score"]]))
test_that("Case A: Trapezoidal", expect_equal(suit$`Suitability Class`["CFragm"][1,], right_tra(1)[["class"]]))
test_that("Case A: Trapezoidal", expect_equal(suit$`Suitability Score`["CFragm"][2,], right_tra(2)[["score"]]))
test_that("Case A: Trapezoidal", expect_equal(suit$`Suitability Class`["CFragm"][2,], right_tra(2)[["class"]]))

# Right Face Gaussian MF
suit <- suitability(LaoCaiLT3, SOYASoil[1:3,], mf="gaussian", interval="unbias")
right_gau<- function (r) {
  x <- LaoCaiLT3[r,"CFragm"]; sigma <- 1
  reqScore <- as.numeric(SOYASoil[1,2:7])
  clnScore <- rev(reqScore[complete.cases(reqScore)])
  Min <- 0
  score <- exp((-1 / 2) * `^`(((x - Min) / sigma), 2))
  
  l1 <- 0; l2 <- exp((-1 / 2) * `^`(((clnScore[3] - Min) / sigma), 2))
  l3 <- exp((-1 / 2) * `^`(((clnScore[2] - Min) / sigma), 2))
  l4 <- exp((-1 / 2) * `^`(((clnScore[1] - Min) / sigma), 2))
  l5 <- 1
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
test_that("Case A: Gaussian", expect_equal(suit$`Suitability Score`["CFragm"][1,], right_gau(1)[["score"]]))
test_that("Case A: Gaussian", expect_equal(suit$`Suitability Class`["CFragm"][1,], right_gau(1)[["class"]]))
test_that("Case A: Gaussian", expect_equal(suit$`Suitability Score`["CFragm"][2,], right_gau(2)[["score"]]))
test_that("Case A: Gaussian", expect_equal(suit$`Suitability Class`["CFragm"][2,], right_gau(2)[["class"]]))

# ------------------------------
# CASE B
# ------------------------------
# Left Face Triangular MF
suit <- suitability(LaoCaiLT3, SOYASoil[8:nrow(SOYASoil),], interval = "unbias")
left_tri <- function(r) {
  x <- LaoCaiLT3[r,"OC"]
  reqScore <- as.numeric(SOYASoil[8,2:7])
  clnScore <- reqScore[complete.cases(reqScore)]
  Max <- clnScore[length(clnScore)] + ((diff(clnScore[1:2]) + diff(clnScore[2:3])) / 2)
  Min <- 0
  score <- (x - Min) / (Max - Min)
  
  l1 = 0; l2 = (clnScore[1] - Min) / (Max - Min); l3 = (clnScore[2] - Min) / (Max - Min); l4 = (clnScore[3] - Min) / (Max - Min); l5 = 1;
  if ((score < Min) || (score > Max)) {
    class_ <- "N"; score <- 0
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
test_that("Case B: Triangular", expect_equal(suit$`Suitability Score`[1,"OC"], left_tri(1)[["score"]]))
test_that("Case B: Triangular", expect_equal(suit$`Suitability Class`[1,"OC"], left_tri(1)[["class"]]))
test_that("Case B: Triangular", expect_equal(suit$`Suitability Score`[2,"OC"], left_tri(2)[["score"]]))
test_that("Case B: Triangular", expect_equal(suit$`Suitability Class`[2,"OC"], left_tri(2)[["class"]]))

# ------------------------------
# CASE C
# ------------------------------
# Full Triangular
suit <- suitability(MarinduqueLT[1:6,], SAFFLOWERSoil[6, ], interval = "unbias")
full_tri <- function () {
  x <- MarinduqueLT[6,"pHH2O"]; Min <- 0
  reqScore <- as.numeric(SAFFLOWERSoil[6,2:7])
  clnScore <- reqScore[complete.cases(reqScore)]
  Max <- clnScore[length(clnScore)] + ((diff(clnScore[1:2]) + diff(clnScore[2:3])) / 2)
  if (x > Max) {
    return(0)
  }
}
test_that("Case C: Triangular", expect_equal(suit$`Suitability Score`["pHH2O"][6,], full_tri()))

MarinduqueLTNew <- tail(MarinduqueLT)
MarinduqueLTNew[1, "pHH2O"] <- 7.6
MarinduqueLTNew[2, "pHH2O"] <- 5.6
suit <- suitability(MarinduqueLTNew, SAFFLOWERSoil, interval = "unbias")
full_tri <- function (r) {
  x <- MarinduqueLTNew[r,"pHH2O"]; Min <- 0
  reqScore <- as.numeric(SAFFLOWERSoil[6,2:7])
  clnScore <- reqScore[complete.cases(reqScore)]
  Max <- clnScore[6] + ((diff(clnScore[1:2]) + diff(clnScore[2:3]) + diff(clnScore[3:4]) + diff(clnScore[4:5]) + diff(clnScore[5:6])) / 5)
  Mid <- mean(clnScore[3:4])
  Min <- 0
  
  if (x > Max) {
    score <- 0; class_ <- "N"
  } else if ((x > Mid) && (x <= Max)) {
    score <- (Max - x) / (Max - Mid)
    l1 = 0; l2 = (Max - clnScore[6]) / (Max - Mid); l3 = (Max - clnScore[5]) / (Max - Mid); l4 = (Max - clnScore[4]) / (Max - Mid); l5 = 1;
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
  } else if (x <= Mid) {
    score <- (x - Min) / (Mid - Min)
    l1 = 0; l2 = (clnScore[1] - Min) / (Mid - Min); l3 = (clnScore[2] - Min) / (Mid - Min); l4 = (clnScore[3] - Min) / (Mid - Min); l5 = 1;
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
test_that("Case C: Triangular", expect_equal(suit$`Suitability Score`["pHH2O"][1,], full_tri(1)[["score"]]))
test_that("Case C: Triangular", expect_equal(suit$`Suitability Class`["pHH2O"][1,], full_tri(1)[["class"]]))
test_that("Case C: Triangular", expect_equal(suit$`Suitability Score`["pHH2O"][2,], full_tri(2)[["score"]]))
test_that("Case C: Triangular", expect_equal(suit$`Suitability Class`["pHH2O"][2,], full_tri(2)[["class"]]))

# # Full Trapezoidal
suit <- suitability(MarinduqueLTNew, SAFFLOWERSoil, mf="trapezoidal", interval = "unbias")
full_tra <- function (r) {
  x <- MarinduqueLTNew[r,"pHH2O"]; Min <- 0
  reqScore <- as.numeric(SAFFLOWERSoil[6,2:7])
  clnScore <- reqScore[complete.cases(reqScore)]
  Max <- reqScore[6] + ((diff(reqScore[1:2]) + diff(reqScore[2:3]) + diff(reqScore[3:4]) + diff(reqScore[4:5]) + diff(reqScore[5:6])) / 5)
  Mid <- mean(reqScore[3:4])
  Min <- 0
  if ((x > Max) && (x < Min)) {
    score <- 0; class_ <- "N"
  } else if (x > reqScore[4]) {
    score <- (Max - x) / (Max - reqScore[4])
    l1 = 0; l2 = (Max - clnScore[6]) / (Max - clnScore[4]); l3 = (Max - clnScore[5]) / (Max - clnScore[6]); l4 = 1; l5 = 1;
  } else if ((x > Min) && (x <= reqScore[3])) {
    score <- (x - Min) / (reqScore[3] - Min)
    l1 = 0; l2 = (clnScore[1] - Min) / (clnScore[3] - Min); l3 = (clnScore[2] - Min) / (clnScore[3] - Min); l4 = 1; l5 = 1;
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
test_that("Case C: Trapezoidal", expect_equal(suit$`Suitability Score`["pHH2O"][1,], full_tra(1)[["score"]]))
test_that("Case C: Trapezoidal", expect_equal(suit$`Suitability Class`["pHH2O"][1,], full_tra(1)[["class"]]))
test_that("Case C: Trapezoidal", expect_equal(suit$`Suitability Score`["pHH2O"][2,], full_tra(2)[["score"]]))
test_that("Case C: Trapezoidal", expect_equal(suit$`Suitability Class`["pHH2O"][2,], full_tra(2)[["class"]]))

# Full Gaussian
suit <- suitability(MarinduqueLTNew, SAFFLOWERSoil[6,], mf="gaussian", interval = "unbias")
full_gau <- function (r) {
  x <- MarinduqueLTNew[r,"pHH2O"]; Min <- 0; sigma <- 1
  reqScore <- as.numeric(SAFFLOWERSoil[6,2:7])
  clnScore <- reqScore[complete.cases(reqScore)]
  Max <- reqScore[6] + ((diff(reqScore[1:2]) + diff(reqScore[2:3]) + diff(reqScore[3:4]) + diff(reqScore[4:5]) + diff(reqScore[5:6])) / 5)
  Mid <- mean(reqScore[3:4])
  Min <- 0
  score <- exp((-1 / 2) * (((x - Mid) / sigma)^2))
  
  if ((x > Min) && (x <= Mid)) {
    l1 <- 0; l2 <- exp((-1 / 2) * `^`(((clnScore[1] - Mid) / sigma), 2))
    l3 <- exp((-1 / 2) * `^`(((clnScore[2] - Mid) / sigma), 2))
    l4 <- exp((-1 / 2) * `^`(((clnScore[3] - Mid) / sigma), 2))
    l5 <- 1
  } else if ((x > Mid) && (x <= Max)) {
    l1 <- 0; l2 <- exp((-1 / 2) * `^`(((clnScore[6] - Mid) / sigma), 2))
    l3 <- exp((-1 / 2) * `^`(((clnScore[5] - Mid) / sigma), 2))
    l4 <- exp((-1 / 2) * `^`(((clnScore[4] - Mid) / sigma), 2))
    l5 <- 1
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
test_that("Case C: Gaussian", expect_equal(suit$`Suitability Score`["pHH2O"][1,], full_gau(1)[["score"]]))
test_that("Case C: Gaussian", expect_equal(suit$`Suitability Class`["pHH2O"][1,], full_gau(1)[["class"]]))
test_that("Case C: Gaussian", expect_equal(suit$`Suitability Score`["pHH2O"][2,], full_gau(2)[["score"]]))
test_that("Case C: Gaussian", expect_equal(suit$`Suitability Class`["pHH2O"][2,], full_gau(2)[["class"]]))


# Full Gaussian
MarinduqueLTNew[1, "pHH2O"] <- 7.3
MarinduqueLTNew[2, "pHH2O"] <- 6.8
suit <- suitability(MarinduqueLTNew, SAFFLOWERSoil[6,], mf="gaussian", interval = "unbias")
full_gau <- function (r) {
  x <- MarinduqueLTNew[r,"pHH2O"]; Min <- 0; sigma <- 1
  reqScore <- as.numeric(SAFFLOWERSoil[6,2:7])
  clnScore <- reqScore[complete.cases(reqScore)]
  Max <- reqScore[6] + ((diff(reqScore[1:2]) + diff(reqScore[2:3]) + diff(reqScore[3:4]) + diff(reqScore[4:5]) + diff(reqScore[5:6])) / 5)
  Mid <- mean(reqScore[3:4])
  Min <- 0
  score <- exp((-1 / 2) * (((x - Mid) / sigma)^2))

  return(score)
}
test_that("Case C: Gaussian", expect_equal(suit$`Suitability Score`["pHH2O"][1,], full_gau(1)))
test_that("Case C: Gaussian", expect_equal(suit$`Suitability Class`["pHH2O"][1,], "S2"))
test_that("Case C: Gaussian", expect_equal(suit$`Suitability Score`["pHH2O"][2,], full_gau(2)))
test_that("Case C: Gaussian", expect_equal(suit$`Suitability Class`["pHH2O"][2,], "S1"))

# # ------------------------------
# # CASE D
# # ------------------------------
# Triangular
MarinduqueLTNew <- MarinduqueLT[7:8,]
suit <- suitability(MarinduqueLTNew, BAMBOOSoil, interval = "unbias")
full_tri <- function (r) {
  x <- MarinduqueLTNew[r,"SoilTe"]; Min <- 0
  reqScore <- as.numeric(BAMBOOSoil[3,2:7])
  clnScore <- reqScore[complete.cases(reqScore)]

  Max <- reqScore[5]
  Mid <- mean(reqScore[3:4])
  Min <- 0
  if (x > Max) {
    score <- 0; class_ <- "N"
  } else if (x > Mid) {
    score <- (Max - x) / (Max - Mid)
    l3 = (Max - clnScore[5]) / (Max - Mid); l4 = (Max - clnScore[4]) / (Max - Mid); l5 = 1;
    if ((score >= l3) && (score < l4)) {
      class_ <- "S2"
    } else if ((score >= l4) && (score <= l5)) {
      class_ <- "S1"
    } else {
      class_ <- "NA"
    }
  } else if (x <= Mid) {
    score <- (x - Min) / (Mid - Min)
  }
  return(list("score" = score, "class" = class_))
}
test_that("Case D: Triangular", expect_equal(suit$`Suitability Score`["SoilTe"][1,], full_tri(1)[["score"]]))
test_that("Case D: Triangular", expect_equal(suit$`Suitability Class`["SoilTe"][1,], full_tri(1)[["class"]]))
test_that("Case D: Triangular", expect_equal(suit$`Suitability Score`["SoilTe"][2,], full_tri(2)[["score"]]))
test_that("Case D: Triangular", expect_equal(suit$`Suitability Class`["SoilTe"][2,], full_tri(2)[["class"]]))

MarinduqueLTNew2 <- MarinduqueLT[7:8,]
MarinduqueLTNew2[1, 6] <- 5
suit <- suitability(MarinduqueLTNew2, ALUES::BAMBOOSoil, interval = "unbias")
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
    l3 = (Max - clnScore[5]) / (Max - Mid); l4 = (Max - clnScore[4]) / (Max - Mid); l5 = 1;
    if ((score >= l3) && (score < l4)) {
      class_ <- "S2"
    } else if ((score >= l4) && (score <= l5)) {
      class_ <- "S1"
    } else {
      class_ <- "NA"
    }
  } else if (x <= Mid) {
    score <- (x - Min) / (Mid - Min)
    l1 = 0; l2 = (clnScore[1] - Min) / (Mid - Min); l3 = (clnScore[2] - Min) / (Mid - Min); l4 = (clnScore[3] - Min) / (Mid - Min); l5 = 1;
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
test_that("Case D: Triangular", expect_equal(suit$`Suitability Score`["SoilTe"][1,], full_tri(1)[["score"]]))
test_that("Case D: Triangular", expect_equal(suit$`Suitability Class`["SoilTe"][1,], full_tri(1)[["class"]]))
test_that("Case D: Triangular", expect_equal(suit$`Suitability Score`["SoilTe"][2,], full_tri(2)[["score"]]))
test_that("Case D: Triangular", expect_equal(suit$`Suitability Class`["SoilTe"][2,], full_tri(2)[["class"]]))

# Trapezoidal
suit <- suitability(MarinduqueLTNew, BAMBOOSoil, mf="trapezoidal", interval = "unbias")
full_tra <- function (r) {
  x <- MarinduqueLTNew[r,"SoilTe"]; Min <- 0
  reqScore <- as.numeric(BAMBOOSoil[3,2:7])
  clnScore <- reqScore[complete.cases(reqScore)]
  Max <- reqScore[5]
  Mid <- mean(reqScore[3:4])
  Min <- 0
  if (x > Max) {
    score <- 0; class_ <- "N"
  } else if (x > reqScore[4]) {
    score <- (Max - x) / (Max - reqScore[4])
  } else if (x <= reqScore[3]) {
    score <- (x - Min) / (reqScore[3] - Min)
  } else if ((x > reqScore[3]) && (x <= reqScore[4])) {
    score <- 1; class_ <- "S1"
  }
  return(list("score" = score, "class" = class_))
}
test_that("Case D: Trapezoidal", expect_equal(suit$`Suitability Score`["SoilTe"][1,], full_tra(1)[["score"]]))
test_that("Case D: Trapezoidal", expect_equal(suit$`Suitability Class`["SoilTe"][1,], full_tra(1)[["class"]]))
test_that("Case D: Trapezoidal", expect_equal(suit$`Suitability Score`["SoilTe"][2,], full_tra(2)[["score"]]))
test_that("Case D: Trapezoidal", expect_equal(suit$`Suitability Class`["SoilTe"][2,], full_tra(2)[["class"]]))

suit <- suitability(MarinduqueLTNew2, ALUES::BAMBOOSoil, mf="trapezoidal", interval = "unbias")
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
    l1 = 0; l2 = (clnScore[1] - Min) / (clnScore[3] - Min); l3 = (clnScore[2] - Min) / (clnScore[3] - Min); l4 = 1; l5 = 1;
    
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
test_that("Case D: Trapezoidal", expect_equal(suit$`Suitability Score`["SoilTe"][1,], full_tra(1)[["score"]]))
test_that("Case D: Trapezoidal", expect_equal(suit$`Suitability Class`["SoilTe"][1,], full_tra(1)[["class"]]))
test_that("Case D: Trapezoidal", expect_equal(suit$`Suitability Score`["SoilTe"][2,], full_tra(2)[["score"]]))
test_that("Case D: Trapezoidal", expect_equal(suit$`Suitability Class`["SoilTe"][2,], full_tra(2)[["class"]]))

MarinduqueLTNew2 <- MarinduqueLT[7:8,]
MarinduqueLTNew2[1, 6] <- 5

MarinduqueLTNew3 <- MarinduqueLTNew2
MarinduqueLTNew3[1, 6] <- 8.3
suit <- suitability(MarinduqueLTNew3, ALUES::BAMBOOSoil, mf="trapezoidal", interval = "unbias")
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
    l3 = (Max - clnScore[5]) / (Max - clnScore[4]); l4 = 1; l5 = 1;
    if ((score >= l3) && (score < l4)) {
      class_ <- "S2"
    } else if (score >= l4) {
      class_ <- "S1"
    } else {
      class_ <- "N"
    }
  } else if (x <= reqScore[3]) {
    score <- (x - Min) / (reqScore[3] - Min)
    l1 = 0; l2 = (clnScore[1] - Min) / (clnScore[3] - Min); l3 = (clnScore[2] - Min) / (clnScore[3] - Min); l4 = 1; l5 = 1;
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
test_that("Case D: Trapezoidal", expect_equal(suit$`Suitability Score`["SoilTe"][1,], full_tra(1)[["score"]]))
test_that("Case D: Trapezoidal", expect_equal(suit$`Suitability Class`["SoilTe"][1,], full_tra(1)[["class"]]))
test_that("Case D: Trapezoidal", expect_equal(suit$`Suitability Score`["SoilTe"][2,], full_tra(2)[["score"]]))
test_that("Case D: Trapezoidal", expect_equal(suit$`Suitability Class`["SoilTe"][2,], full_tra(2)[["class"]]))

# Gaussian
MarinduqueLTNew <- MarinduqueLT[7:8,]
suit <- suitability(MarinduqueLTNew, BAMBOOSoil, mf="gaussian", interval = "unbias")
full_gau <- function (r) {
  x <- MarinduqueLTNew[r,"SoilTe"]; Min <- 0; sigma <- 1
  reqScore <- as.numeric(BAMBOOSoil[3,2:7])
  clnScore <- reqScore[complete.cases(reqScore)]
  Max <- reqScore[5]
  Mid <- mean(reqScore[3:4])
  Min <- 0
  score <- exp((-1 / 2) * (((x - Mid) / sigma)^2))
  
  if ((x > Min) && (x <= Mid)) {
    l1 <- 0; l2 <- exp((-1 / 2) * `^`(((clnScore[1] - Mid) / sigma), 2))
    l3 <- exp((-1 / 2) * `^`(((clnScore[2] - Mid) / sigma), 2))
    l4 <- exp((-1 / 2) * `^`(((clnScore[3] - Mid) / sigma), 2))
    l5 <- 1
  } else if ((x > Mid)) {
    l3 <- exp((-1 / 2) * `^`(((clnScore[5] - Mid) / sigma), 2))
    l4 <- exp((-1 / 2) * `^`(((clnScore[4] - Mid) / sigma), 2))
    l5 <- 1
  } 
  
  if ((score >= l3) && (score < l4)) {
    class_ <- "S2"
  } else if ((score >= l4) && (score <= l5)) {
    class_ <- "S1"
  } else {
    class_ <- "N"
  }
  return(list("score" = score, "class" = class_))
}
test_that("Case D: Gaussian", expect_equal(suit$`Suitability Score`["SoilTe"][1,], full_gau(1)[["score"]]))
test_that("Case D: Gaussian", expect_equal(suit$`Suitability Class`["SoilTe"][1,], full_gau(1)[["class"]]))
test_that("Case D: Gaussian", expect_equal(suit$`Suitability Score`["SoilTe"][2,], full_gau(2)[["score"]]))
test_that("Case D: Gaussian", expect_equal(suit$`Suitability Class`["SoilTe"][2,], full_gau(2)[["class"]]))

suit <- suitability(MarinduqueLTNew2, ALUES::BAMBOOSoil, mf="gaussian", interval = "unbias")
print("ooetijeotjo")
print(suit)
full_gau <- function (r) {
  x <- MarinduqueLTNew2[r,"SoilTe"]; Min <- 0; sigma <- 1
  reqScore <- as.numeric(ALUES::BAMBOOSoil[3,2:7])
  clnScore <- reqScore[complete.cases(reqScore)]
  Max <- reqScore[5]
  Mid <- mean(reqScore[3:4])
  Min <- 0
  score <- exp((-1 / 2) * (((x - Mid) / sigma)^2))
  
  if ((x > Min) && (x <= Mid)) {
    l1 <- 0; l2 <- exp((-1 / 2) * `^`(((clnScore[1] - Mid) / sigma), 2))
    l3 <- exp((-1 / 2) * `^`(((clnScore[2] - Mid) / sigma), 2))
    l4 <- exp((-1 / 2) * `^`(((clnScore[3] - Mid) / sigma), 2))
    l5 <- 1
  } else if ((x > Mid)) {
    l3 <- exp((-1 / 2) * `^`(((clnScore[5] - Mid) / sigma), 2))
    l4 <- exp((-1 / 2) * `^`(((clnScore[4] - Mid) / sigma), 2))
    l5 <- 1
  } 
  
  if ((score >= l3) && (score < l4)) {
    class_ <- "S2"
  } else if ((score >= l4) && (score <= l5)) {
    class_ <- "S1"
  } else {
    class_ <- "N"
  }
  return(list("score" = score, "class" = class_))
}
test_that("Case D: Gaussian", expect_equal(suit$`Suitability Score`["SoilTe"][1,], full_gau(1)[["score"]]))
test_that("Case D: Gaussian", expect_equal(suit$`Suitability Class`["SoilTe"][1,], full_gau(1)[["class"]]))
test_that("Case D: Gaussian", expect_equal(suit$`Suitability Score`["SoilTe"][2,], full_gau(2)[["score"]]))
test_that("Case D: Gaussian", expect_equal(suit$`Suitability Class`["SoilTe"][2,], full_gau(2)[["class"]]))

# ------------------------------
# CASE E
# ------------------------------
# Triangular
MarinduqueLTNew <- MarinduqueLT[7:8,]
BAMBOOSoil[3, 6] <- NA
BAMBOOSoil[3, 5] <- 9.0
BAMBOOSoilNew <- BAMBOOSoil
suit <- suitability(MarinduqueLTNew, BAMBOOSoilNew, interval="unbias")
r = 1
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
  return(score)
}

test_that("Case E: Triangular", expect_equal(suit$`Suitability Class`["SoilTe"][1,], "N"))
test_that("Case E: Triangular", expect_equal(suit$`Suitability Class`["SoilTe"][2,], "S1"))
test_that("Case E: Triangular", expect_equal(suit$`Suitability Score`["SoilTe"][1,], full_tri(1)))
test_that("Case E: Triangular", expect_equal(suit$`Suitability Score`["SoilTe"][2,], full_tri(2)))

# Triangular
MarinduqueLTNew2 <- MarinduqueLT[7:8,]
MarinduqueLTNew2[1, 6] <- 5
suit <- suitability(MarinduqueLTNew2, BAMBOOSoilNew, interval="unbias")
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
  } else if ((x > Min) && (x <= Mid)) {
    score <- (x - Min) / (Mid - Min)
  }

  return(score)
}
test_that("Case E: Triangular", expect_equal(suit$`Suitability Score`["SoilTe"][1,], full_tri(1)))
test_that("Case E: Triangular", expect_equal(suit$`Suitability Class`["SoilTe"][1,], "S2"))
test_that("Case E: Triangular", expect_equal(suit$`Suitability Score`["SoilTe"][2,], full_tri(2)))
test_that("Case E: Triangular", expect_equal(suit$`Suitability Class`["SoilTe"][2,], "S1"))

# Trapezoidal
suit <- suitability(MarinduqueLTNew, BAMBOOSoilNew, mf="trapezoidal", interval="unbias")
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
  } else if ((x > reqScore[3]) && (x <= reqScore[4])) {
    score <- 1
  }

  return(score)
}
test_that("Case E: Trapezoidal", expect_equal(suit$`Suitability Score`["SoilTe"][1,], full_tra(1)))
test_that("Case E: Trapezoidal", expect_equal(suit$`Suitability Class`["SoilTe"][1,], "N"))
test_that("Case E: Trapezoidal", expect_equal(suit$`Suitability Score`["SoilTe"][2,], full_tra(2)))
test_that("Case E: Trapezoidal", expect_equal(suit$`Suitability Class`["SoilTe"][2,], "S1"))

MarinduqueLTNew2 <- MarinduqueLT[7:8,]
MarinduqueLTNew2[1, 6] <- 5

suit <- suitability(MarinduqueLTNew2, BAMBOOSoilNew, mf="trapezoidal", interval="unbias")
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
  } else if ((x > reqScore[3]) && (x <= reqScore[4])) {
    score <- 1; class_ = "S1"
  }


  return(score)
}
test_that("Case E: Trapezoidal", expect_equal(suit$`Suitability Score`["SoilTe"][1,], full_tra(1)))
test_that("Case E: Trapezoidal", expect_equal(suit$`Suitability Class`["SoilTe"][1,], "S2"))
test_that("Case E: Trapezoidal", expect_equal(suit$`Suitability Score`["SoilTe"][2,], full_tra(2)))
test_that("Case E: Trapezoidal", expect_equal(suit$`Suitability Class`["SoilTe"][2,], "S1"))

# Gaussian
suit <- suitability(MarinduqueLTNew, BAMBOOSoilNew, mf="gaussian", interval="unbias")
full_gau <- function (r) {
  x <- MarinduqueLTNew[r,"SoilTe"]; Min <- 0; sigma <- 1
  reqScore <- as.numeric(BAMBOOSoilNew[3,2:7])
  clnScore <- reqScore[complete.cases(reqScore)]
  Max <- reqScore[4]
  Mid <- mean(reqScore[3:4])
  Min <- 0
  score <- exp((-1 / 2) * (((x - Mid) / sigma)^2))
  return(score)
}
test_that("Case E: Gaussian", expect_equal(suit$`Suitability Score`["SoilTe"][1,], full_gau(1)))
test_that("Case E: Gaussian", expect_equal(suit$`Suitability Class`["SoilTe"][1,], "N"))
test_that("Case E: Gaussian", expect_equal(suit$`Suitability Score`["SoilTe"][2,], full_gau(2)))
test_that("Case E: Gaussian", expect_equal(suit$`Suitability Class`["SoilTe"][2,], "S1"))

suit <- suitability(MarinduqueLTNew2, BAMBOOSoilNew, mf="gaussian", interval="unbias")
full_gau <- function (r) {
  x <- MarinduqueLTNew2[r,"SoilTe"]; Min <- 0; sigma <- 1
  reqScore <- as.numeric(BAMBOOSoilNew[3,2:7])
  clnScore <- reqScore[complete.cases(reqScore)]
  Max <- reqScore[4]
  Mid <- mean(reqScore[3:4])
  Min <- 0
  score <- exp((-1 / 2) * (((x - Mid) / sigma)^2))
  return(score)
}
test_that("Case E: Gaussian", expect_equal(suit$`Suitability Score`["SoilTe"][1,], full_gau(1)))
test_that("Case E: Gaussian", expect_equal(suit$`Suitability Class`["SoilTe"][1,], "S2"))
test_that("Case E: Gaussian", expect_equal(suit$`Suitability Score`["SoilTe"][2,], full_gau(2)))
test_that("Case E: Gaussian", expect_equal(suit$`Suitability Class`["SoilTe"][2,], "S1"))
