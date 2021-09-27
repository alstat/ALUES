library(testthat)
library(ALUES)

out <- suit("ricebr", water=MarinduqueWater, sow_month=1)
test_that("suit: water", expect_equal(names(out), "water"))
test_that("suit: water", expect_equal(out[["water"]][["Crop Evaluated"]], "RICEBRWater"))

out <- suit("ricebr", temp=MarinduqueTemp, sow_month=1)
test_that("suit: temp", expect_equal(names(out), "temp"))
test_that("suit: temp", expect_equal(out[["temp"]][["Crop Evaluated"]], "RICEBRTemp"))

out <- suit("ricebr", water=MarinduqueWater, temp=MarinduqueTemp, sow_month=1)
test_that("suit: water and temp", expect_equal(names(out), c("water", "temp")))
test_that("suit: water and temp", expect_equal(out[["temp"]][["Crop Evaluated"]], "RICEBRTemp"))
test_that("suit: water and temp", expect_equal(out[["water"]][["Crop Evaluated"]], "RICEBRWater"))

out <- suit("coconut", terrain=LaoCaiLT)
test_that("suit: terrain", expect_equal(names(out), c("terrain", "soil")))
test_that("suit: terrain", expect_equal(out[["terrain"]][["Crop Evaluated"]], "COCONUTTerrain"))
test_that("suit: terrain", expect_equal(out[["soil"]][["Crop Evaluated"]], "COCONUTSoil"))

out <- suit("ricebr", terrain=MarinduqueLT, water=MarinduqueWater, temp=MarinduqueTemp, sow_month=1)
test_that("suit: terrain, water and temp", expect_equal(names(out), c("terrain", "soil", "water", "temp")))
# test_that("suit: terrain, water and temp", expect_equal(out[["terrain"]], "Error: No factor(s) to be evaluated, since none matches with the crop requirements."))
test_that("suit: terrain, water and temp", expect_equal(out[["soil"]]$`Crop Evaluated`, "RICEBRSoil"))
test_that("suit: terrain, water and temp", expect_equal(out[["water"]]$`Crop Evaluated`, "RICEBRWater"))
test_that("suit: terrain, water and temp", expect_equal(out[["temp"]]$`Crop Evaluated`, "RICEBRTemp"))

out <- suit("ricebr", terrain=MarinduqueLT, water=MarinduqueWater, sow_month=1)
test_that("suit: terrain, water and temp", expect_equal(names(out), c("terrain", "soil", "water")))
# test_that("suit: terrain, water and temp", expect_equal(out[["terrain"]], "Error: No factor(s) to be evaluated, since none matches with the crop requirements."))
test_that("suit: terrain, water and temp", expect_equal(out[["soil"]]$`Crop Evaluated`, "RICEBRSoil"))
test_that("suit: terrain, water and temp", expect_equal(out[["water"]]$`Crop Evaluated`, "RICEBRWater"))

out <- suit("ricebr", terrain=MarinduqueLT, temp=MarinduqueTemp, sow_month=1)
test_that("suit: terrain, water and temp", expect_equal(names(out), c("terrain", "soil", "temp")))
# test_that("suit: terrain, water and temp", expect_equal(out[["terrain"]], "Error: No factor(s) to be evaluated, since none matches with the crop requirements."))
test_that("suit: terrain, water and temp", expect_equal(out[["soil"]]$`Crop Evaluated`, "RICEBRSoil"))
test_that("suit: terrain, water and temp", expect_equal(out[["temp"]]$`Crop Evaluated`, "RICEBRTemp"))

out <- suit("ricebr", terrain=LaoCaiLT)
test_that("suit: catch terrain", expect_equal(out[["terrain"]]$`Factors Evaluated`, "Flood"))
test_that("suit: catch soil", expect_equal(out[["soil"]]$`Factors Evaluated`, c("CFragm", "CECc", "BS", "SumBCs", "pHH2O", "OC")))

out <- suit("ricebr", water=LaoCaiWater, sow_month=1)
test_that("suit: catch water", expect_equal(out[["water"]]$`Factors Evaluated`, c("Jan", "Feb", "Mar", "Apr")))

out <- suit("ricebr", temp=LaoCaiTemp, sow_month=1)
test_that("suit: catch temp", expect_equal(out[["temp"]]$`Factors Evaluated`, "Feb"))

out <- suit("ricebr", terrain=LaoCaiLT, temp=LaoCaiTemp, sow_month=1)
test_that("suit: catch terrain", expect_equal(out[["terrain"]]$`Factors Evaluated`, "Flood"))
test_that("suit: catch temp", expect_equal(out[["temp"]]$`Factors Evaluated`, "Feb"))

out <- suit("ricebr", terrain=LaoCaiLT, water=LaoCaiWater, sow_month=1)
test_that("suit: catch terrain", expect_equal(out[["terrain"]]$`Factors Evaluated`, "Flood"))
test_that("suit: catch water", expect_equal(out[["water"]]$`Factors Evaluated`, c("Jan", "Feb", "Mar", "Apr")))

out <- suit("ricebr", terrain=LaoCaiLT)
test_that("suit: warning soil", expect_equal(out[["soil"]]$Warning, "maximum is set to 16 for factor CECc since all parameter intervals are equal."))

out <- suit("ricebr", terrain=LaoCaiLT, water=LaoCaiWater, temp=LaoCaiTemp, sow_month=1, sigma=1)
test_that("suit: warning terrain", expect_equal(out[["terrain"]]$Warning, "sigma is only use for gaussian membership function. It defines the spread of the gaussian model."))
test_that("suit: warning soil", expect_equal(out[["soil"]]$Warning, "sigma is only use for gaussian membership function. It defines the spread of the gaussian model."))
test_that("suit: warning water", expect_equal(out[["water"]]$Warning, "sigma is only use for gaussian membership function. It defines the spread of the gaussian model."))
test_that("suit: warning temp", expect_equal(out[["temp"]]$Warning, "sigma is only use for gaussian membership function. It defines the spread of the gaussian model."))

out <- suit("ricebr", terrain=LaoCaiLT, water=LaoCaiWater, sow_month=1, sigma=1)
test_that("suit: warning terrain", expect_equal(out[["terrain"]]$Warning, "sigma is only use for gaussian membership function. It defines the spread of the gaussian model."))
test_that("suit: warning soil", expect_equal(out[["soil"]]$Warning, "sigma is only use for gaussian membership function. It defines the spread of the gaussian model."))
test_that("suit: warning water", expect_equal(out[["water"]]$Warning, "sigma is only use for gaussian membership function. It defines the spread of the gaussian model."))

out <- suit("ricebr", terrain=LaoCaiLT, temp=LaoCaiTemp, sow_month=1, sigma=1)
test_that("suit: warning terrain", expect_equal(out[["terrain"]]$Warning, "sigma is only use for gaussian membership function. It defines the spread of the gaussian model."))
test_that("suit: warning soil", expect_equal(out[["soil"]]$Warning, "sigma is only use for gaussian membership function. It defines the spread of the gaussian model."))
test_that("suit: warning temp", expect_equal(out[["temp"]]$Warning, "sigma is only use for gaussian membership function. It defines the spread of the gaussian model."))

out <- suit("ricebr", temp=LaoCaiTemp, sow_month=1, sigma=1)
test_that("suit: warning temp", expect_equal(out[["temp"]]$Warning, "sigma is only use for gaussian membership function. It defines the spread of the gaussian model."))

out <- suit("ricebr", water=LaoCaiWater, temp=LaoCaiTemp, sow_month=1, sigma=1)
test_that("suit: warning water", expect_equal(out[["water"]]$Warning, "sigma is only use for gaussian membership function. It defines the spread of the gaussian model."))
test_that("suit: warning temp", expect_equal(out[["temp"]]$Warning, "sigma is only use for gaussian membership function. It defines the spread of the gaussian model."))

out <- suit("ricebr", water=LaoCaiWater, sow_month=1, sigma=1)
test_that("suit: warning water", expect_equal(out[["water"]]$Warning, "sigma is only use for gaussian membership function. It defines the spread of the gaussian model."))

out <- suit("ricebr", water=LaoCaiWater, temp=LaoCaiTemp, sow_month=1)
test_that("suit: catch water", expect_equal(out[["water"]]$`Factors Evaluated`, c("Jan", "Feb", "Mar", "Apr")))
test_that("suit: catch temp", expect_equal(out[["temp"]]$`Factors Evaluated`, "Feb"))

test_that("suit: error", expect_error(suit("ricebr", terrain=MarinduqueLT, water=MarinduqueWater, temp=MarinduqueTemp)))
test_that("suit: error", expect_warning(suit("rice", water=LaoCaiWater, sow_month=1)))
test_that("suit: error", expect_error(suit("ricebr")))
test_that("suit: error", expect_warning(suit("coffee", terrain=LaoCaiLT)))
test_that("suit: error", expect_warning(suit("potato", terrain=LaoCaiLT)))
test_that("suit: error", expect_error(suit("sdfa", terrain=LaoCaiLT)))
test_that("suit: error", expect_error(suit("ricebr", water=MarinduqueWater)))
test_that("suit: error", expect_error(suit("ricebr", temp=MarinduqueTemp)))
test_that("suit: error", expect_equal(suit("coconut", water=LaoCaiWater, sow_month=1)$water, "Error: No factor(s) to be evaluated, since none matches with the crop requirements."))
test_that("suit: error", expect_equal(suit("coconut", terrain=LaoCaiLT, mf="sdf")$terrain, "Error: Unrecognized mf='sdf', please choose either 'triangular', 'trapezoidal' or 'gaussian'."))
test_that("suit: error", expect_equal(suit("coconut", terrain=LaoCaiLT, mf="sdf")$soil, "Error: Unrecognized mf='sdf', please choose either 'triangular', 'trapezoidal' or 'gaussian'."))

