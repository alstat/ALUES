#' Suitability Scores/Class of the Land Units
#' @export
#' 
#' @description
#' This function calculates the suitability scores and class of the land units.
#' 
#' @param crop a string for the name of the crop;
#' @param terrain a data frame for the terrain characteristics of the input land units;
#' @param water a data frame for the water characteristics of the input land units;
#' @param temp a data frame for the temp characteristics of the input land units;
#' @param mf membership function with default assigned to \code{"triangular"} 
#'           fuzzy model. Other fuzzy models included are \code{"trapezoidal"} and
#'           \code{"gaussian"}.
#' @param sow_month sowing month of the crop. Takes integers from 1 to 12 
#'                  (inclusive), representing the twelve months of a year. 
#'                  So if sets to 1, the function assumes sowing month on 
#'                  January.
#' @param min factor's minimum value. If \code{NULL} (default), \code{min} is
#'            set to 0. But if numeric of length one, say 0.5, then minimum 
#'            is set to 0.5, for all factors. If factors on land units 
#'            (\code{x}) have different minimum, then these can be concatenated
#'            to vector of \code{min}s, the length of this vector should be equal
#'            to the number of factors in \code{x}. However, if set to \code{"average"},
#'            then \code{min} is computed from different conditions:
#'            
#'            If for example, using \code{ALUES::COCONUTSoil} (coconut terrain requirements), 
#'            as shown below,
#'            
#'            \code{     code s3_a s2_a  s1_a s1_b s2_b s3_b          wts}\cr
#'            \code{1  CFragm 55.0 35.0  15.0   NA   NA   NA           NA}\cr
#'            \code{2 SoilDpt 50.0 75.0 100.0   NA   NA   NA           NA}\cr
#'            \code{3      BS 19.9 19.9  20.0   NA   NA   NA           NA}\cr
#'            \code{4  SumBCs  1.5  1.5   1.6   NA   NA   NA           NA}\cr
#'            \code{5      OC  0.7  0.7   0.8   NA   NA   NA           NA}\cr
#'            \code{6   ECemh 20.0 16.0  12.0   NA   NA   NA           NA}
#'            
#' @param max maximum value for factors. Default is to \code{"average"}, check on
#'              the details for this option. Assignment on maximum can also be done by
#'              simply entering any real numbers, say 55, then max is 55, we say this is homogeneous,
#'              since the maximum value for all factors then is set to 55. But for heterogeneous. For  \code{max}
#'              on every factor, simply concatenate the different \code{max} for each factor. 
#'              If set to \code{"average"}, check on details below for more.
#' @param interval domains for every suitability class (S1, S2, S3). If fixed (\code{NULL}), the
#'              interval would be 0 to 25\% for N (Not Suitable), 25\% to 50\% for S3 (Marginally Suitable),
#'              50\% to 75\% for S2 (Moderately Suitable), and 75\% to 100\% for (Highly Suitable). If \code{"unbias"},
#'              the package will take into account the shape of the membership function, and provide the 
#'              appropriate suitability class intervals.
#' @param sigma If \code{mf = "gaussian"}, then sigma represents the constant sigma in the
#'              gaussian formula, which is often times referred as the variance.
#' @details
#' There are four membership functions and these are triangular, trapezoidal, gaussian, and sigmoidal.
#' For triangular case. If a given factor has values equal for all suitabilities, then the class will 
#' trimmed down to N (not suitable) with domain [0, max), and S1 (highly suitable) with single tone domain \{0\}.
#' 
#' @examples
#' library(ALUES)
#' 
#' rice_suit <- suit("ricebr", water=MarinduqueWater, temp=MarinduqueTemp, sow_month = 1)
#' rice_suit[["water"]] # access results for water suitability
#' rice_suit[["temp"]] # access results for temperature suitability
suit <- function (crop, terrain=NULL, water=NULL, temp=NULL, mf = "triangular", sow_month = NULL, min = NULL, max = "average", interval = NULL, sigma = NULL) {
  if (is.null(terrain) && is.null(water) && is.null(temp)) {
    stop("Please specify at least one land characteristics: terrain, water, or temp.")
  }
  
  d <- utils::data(package = "ALUES")
  alues_data <- d$results[, "Item"]
  crop_data <- regmatches(alues_data, gregexpr(paste0("^[A-Z]{2,}", collapse = "|"), alues_data))
  crop_data <- unique(unlist(lapply(crop_data, function(x) substr(x, 1, nchar(x)-1))))
  
  if (toupper(crop) %in% crop_data) {
     crop <- toupper(crop)
  } else {
    if (crop == "rice") {
      warning("Defaulting to 'ricebr', other options for rice: 'riceiw', 'ricenf', and 'riceur'. Specify accordingly.")
      crop <- toupper("ricebr")
    } else if (crop == "coffee") {
      warning("Defaulting to 'coffeear', other options for coffee: 'coffeero'. Specify accordingly.")
      crop <- toupper("coffeear")
    } else if (crop == "potato") {
      warning("Defaulting to 'potato', other options for potato: 'potatosw'")
      crop <- toupper("potato")
    } else {
      stop(paste("Input crop='", crop, "' is not available in the database, see docs for list of ALUES data.", sep=""))
    }
  }
  
  if (!is.null(terrain) && !is.null(water) && !is.null(temp)) {
    if (is.null(sow_month)) {
      stop("Please specify sowing month to match the corresponding factors in input land units.")
    }
    crop_terrain <- eval(parse(text=paste(crop, "Terrain", sep="")), envir=.GlobalEnv)
    crop_soil <- eval(parse(text=paste(crop, "Soil", sep="")), envir=.GlobalEnv)
    suit_terrain <- tryCatch(
      {
        suit_terrain <- suitability(terrain, crop_terrain, mf=mf, sow_month=NULL, min=min, max=max, interval=interval, sigma=sigma)
        suit_terrain[["Crop Evaluated"]] <- paste(crop, "Terrain", sep="")
        suit_terrain
      },
      error = function(x) {
        return(x)
      }
    )
    suit_soil <- tryCatch(
      {
        suit_soil <- suitability(terrain, crop_soil, mf=mf, sow_month=NULL, min=min, max=max, interval=interval, sigma=sigma)
        suit_soil[["Crop Evaluated"]] <- paste(crop, "Soil", sep="")
        suit_soil
      },
      error = function(x) {
        return(x)
      }
    )
    
    crop_water <- eval(parse(text=paste(crop, "Water", sep="")), envir=.GlobalEnv)
    suit_water <- suitability(water, crop_water, mf=mf, sow_month=sow_month, min=min, max=max, interval=interval, sigma=sigma)
    suit_water[["Crop Evaluated"]] <- paste(crop, "Water", sep="")
    
    crop_temp <- eval(parse(text=paste(crop, "Temp", sep="")), envir=.GlobalEnv)
    suit_temp <- suitability(temp, crop_temp, mf=mf, sow_month=sow_month, min=min, max=max, interval=interval, sigma=sigma)
    suit_temp[["Crop Evaluated"]] <- paste(crop, "Temp", sep="")
    return(list("terrain" = suit_terrain, "soil" = suit_soil, "water" = suit_water, "temp" = suit_temp))
  } else if (!is.null(terrain) && !is.null(water)) {
    if (is.null(sow_month)) {
      stop("Please specify sowing month to match the corresponding factors in input land units.")
    }
    crop_terrain <- eval(parse(text=paste(crop, "Terrain", sep="")), envir=.GlobalEnv)
    crop_soil <- eval(parse(text=paste(crop, "Soil", sep="")), envir=.GlobalEnv)
    suit_terrain <- tryCatch(
      {
        suit_terrain <- suitability(terrain, crop_terrain, mf=mf, sow_month=NULL, min=min, max=max, interval=interval, sigma=sigma)
        suit_terrain[["Crop Evaluated"]] <- paste(crop, "Terrain", sep="")
        suit_terrain
      },
      error = function(x) {
        return(x)
      }
    )
    suit_soil <- tryCatch(
      {
        suit_soil <- suitability(terrain, crop_soil, mf=mf, sow_month=NULL, min=min, max=max, interval=interval, sigma=sigma)
        suit_soil[["Crop Evaluated"]] <- paste(crop, "Soil", sep="")
        suit_soil
      },
      error = function(x) {
        return(x)
      }
    )
    
    crop_water <- eval(parse(text=paste(crop, "Water", sep="")), envir=.GlobalEnv)
    suit_water <- suitability(water, crop_water, mf=mf, sow_month=sow_month, min=min, max=max, interval=interval, sigma=sigma)
    suit_water[["Crop Evaluated"]] <- paste(crop, "Water", sep="")
    return(list("terrain" = suit_terrain, "soil" = suit_soil, "water" = suit_water))
  } else if (!is.null(terrain) && !is.null(temp)) {
    if (is.null(sow_month)) {
      stop("Please specify sowing month to match the corresponding factors in input land units.")
    }
    crop_terrain <- eval(parse(text=paste(crop, "Terrain", sep="")), envir=.GlobalEnv)
    crop_soil <- eval(parse(text=paste(crop, "Soil", sep="")), envir=.GlobalEnv)
    suit_terrain <- tryCatch(
      {
        suit_terrain <- suitability(terrain, crop_terrain, mf=mf, sow_month=NULL, min=min, max=max, interval=interval, sigma=sigma)
        suit_terrain[["Crop Evaluated"]] <- paste(crop, "Terrain", sep="")
        suit_terrain
      },
      error = function(x) {
        return(x)
      }
    )
    suit_soil <- tryCatch(
      {
        suit_soil <- suitability(terrain, crop_soil, mf=mf, sow_month=NULL, min=min, max=max, interval=interval, sigma=sigma)
        suit_soil[["Crop Evaluated"]] <- paste(crop, "Soil", sep="")
        suit_soil
      },
      error = function(x) {
        return(x)
      }
    )
    
    crop_temp <- eval(parse(text=paste(crop, "Temp", sep="")), envir=.GlobalEnv)
    suit_temp <- suitability(temp, crop_temp, mf=mf, sow_month=sow_month, min=min, max=max, interval=interval, sigma=sigma)
    suit_temp[["Crop Evaluated"]] <- paste(crop, "Temp", sep="")
    return(list("terrain" = suit_terrain, "soil" = suit_soil, "temp" = suit_temp))
  } else if (!is.null(water) && !is.null(temp)) {
    if (is.null(sow_month)) {
      stop("Please specify sowing month to match the corresponding factors in input land units.")
    }
    crop_water <- eval(parse(text=paste(crop, "Water", sep="")), envir=.GlobalEnv)
    suit_water <- suitability(water, crop_water, mf=mf, sow_month=sow_month, min=min, max=max, interval=interval, sigma=sigma)
    suit_water[["Crop Evaluated"]] <- paste(crop, "Water", sep="")
    
    crop_temp <- eval(parse(text=paste(crop, "Temp", sep="")), envir=.GlobalEnv)
    suit_temp <- suitability(temp, crop_temp, mf=mf, sow_month=sow_month, min=min, max=max, interval=interval, sigma=sigma)
    suit_temp[["Crop Evaluated"]] <- paste(crop, "Temp", sep="")
    return(list("water" = suit_water, "temp" = suit_temp))
  } else if (!is.null(terrain)) {
    crop_terrain <- eval(parse(text=paste(crop, "Terrain", sep="")), envir=.GlobalEnv)
    crop_soil <- eval(parse(text=paste(crop, "Soil", sep="")), envir=.GlobalEnv)
    suit_terrain <- tryCatch(
      {
        suit_terrain <- suitability(terrain, crop_terrain, mf=mf, sow_month=NULL, min=min, max=max, interval=interval, sigma=sigma)
        suit_terrain[["Crop Evaluated"]] <- paste(crop, "Terrain", sep="")
        suit_terrain
      },
      error = function(x) {
        return(x)
      }
    )
    suit_soil <- tryCatch(
      {
        suit_soil <- suitability(terrain, crop_soil, mf=mf, sow_month=NULL, min=min, max=max, interval=interval, sigma=sigma)
        suit_soil[["Crop Evaluated"]] <- paste(crop, "Soil", sep="")
        suit_soil
      },
      error = function(x) {
        return(x)
      }
    )
    return(list("terrain" = suit_terrain, "soil" = suit_soil))
  } else if (!is.null(water)) {
    if (is.null(sow_month)) {
      stop("Please specify sowing month to match the corresponding factors in input land units.")
    }
    crop_water <- eval(parse(text=paste(crop, "Water", sep="")), envir=.GlobalEnv)
    suit_water <- suitability(water, crop_water, mf=mf, sow_month=sow_month, min=min, max=max, interval=interval, sigma=sigma)
    suit_water[["Crop Evaluated"]] <- paste(crop, "Water", sep="")
    return(list("water" = suit_water))
  } else if (!is.null(temp)) {
    if (is.null(sow_month)) {
      stop("Please specify sowing month to match the corresponding factors in input land units.")
    }
    crop_temp <- eval(parse(text=paste(crop, "Temp", sep="")), envir=.GlobalEnv)
    suit_temp <- suitability(temp, crop_temp, mf=mf, sow_month=sow_month, min=min, max=max, interval=interval, sigma=sigma)
    suit_temp[["Crop Evaluated"]] <- paste(crop, "Temp", sep="")
    return(list("temp" = suit_temp))
  }
}
