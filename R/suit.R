#' Suitability Scores/Class of the Land Units
#' @export
#' 
#' @description
#' This function calculates the suitability scores and class of the land units.
#' 
#' @param crop a string for the name of the crop;
#' @param terrain a data frame for the terrain characteristics of the input land units;
#' @param water a data frame for the water characteristics of the input land units;
#' @param temp a data frame for the temperature characteristics of the input land units;
#' @param mf membership function with default assigned to \code{"triangular"} 
#'           fuzzy model. Other fuzzy models included are \code{"trapezoidal"} and
#'           \code{"gaussian"}.
#' @param sow_month sowing month of the crop. Takes integers from 1 to 12 
#'                  (inclusive), representing the twelve months of the year. 
#'                  So if sets to 1, the function assumes sowing month to be 
#'                  January.
#' @param minimum factor's minimum value. If \code{NULL} (default), \code{minimum} is
#'            set to 0. But if numeric of length one, say 0.5, then minimum 
#'            is set to 0.5, for all factors. To set multiple minimums for multiple factors,
#'            simply concatenate these into a numeric vector, the length of this vector should be equal
#'            to the number of factors in input land units parameters. However, it can also be set to
#'            \code{"average"}, please refer to the online documentation for more, link in the "See Also" section below.
#'            
#' @param maximum maximum value for factors. To set multiple maximums for multiple factors,
#'            simply concatenate these into a numeric vector, the length of this vector should be equal
#'            to the number of factors in input land units parameters. However, it can also be set to
#'            \code{"average"}, please refer to the online documentation for more, link in the "See Also" section below.
#'            
#' @param interval domains for every suitability class (S1, S2, S3). If fixed (\code{NULL}), the
#'              interval would be 0 to 25\% for N (Not Suitable), 25\% to 50\% for S3 (Marginally Suitable),
#'              50\% to 75\% for S2 (Moderately Suitable), and 75\% to 100\% for (Highly Suitable). If \code{"unbias"},
#'              the package will take into account the shape of the membership function, and provide the 
#'              appropriate suitability class intervals. However, it can also be customized by specifying the 
#'              limits of the suitability classes. Please refer to the online documentation for more, link in the "See Also" section below.
#' @param sigma If \code{mf = "gaussian"}, then sigma represents the constant sigma in the
#'              Gaussian formula.
#' 
#' @return
#' A list of outputs of target characteristics, with the following components: 
#' \itemize{
#' \item \code{"terrain"} - a list of outputs for terrain characteristics
#' \item \code{"soil"} - a list of outputs for soil characteristics
#' \item \code{"water"} - a list of outputs for water characteristics
#' \item \code{"temp"} - a list of outputs for temperature characteristics
#' }
#' These components are only available when specified as the target characteristics in either 
#' of the arguments above, that is, if \code{terrain} argument is specified above, then the \code{"terrain"}
#' and \code{"soil"} components will be available in the output list. This is also true if \code{water} and \code{temp}
#' are specified in the arguments above. 
#'
#' Each of the components returned above contains a list of outputs as well
#' with the following components: 
#' \itemize{
#' \item \code{"Factors Evaluated"} - a character of factors that matched between the input land units factor and the targetted crop requirement factor
#' \item \code{"Suitability Score"} - a data frame of suitability scores for each of the matched factors
#' \item \code{"Suitability Class"} - a data frame of suitability classes for each of the matched factors
#' \item \code{"Factors' Minimum Values"} - a numeric of minimum values used in the membership function for computing the suitability scores
#' \item \code{"Factors' Minimum Values"} - a numeric of maximum values used in the membership function for computing the suitability scores
#' \item \code{"Factors' Weights"} - a numeric of weights of the factors specified in the input crop requirements
#' \item \code{"Crop Evaluated"} - a character of the name of the targetted crop requirement dataset
#' }
#' 
#' @seealso 
#' \code{https://alstat.github.io/ALUES/}
#' 
#' @examples
#' library(ALUES)
#' 
#' rice_suit <- suit("ricebr", water=MarinduqueWater, temp=MarinduqueTemp, sow_month = 1)
#' lapply(rice_suit[["water"]], function(x) head(x)) # access results for water suitability
#' lapply(rice_suit[["temp"]], function(x) head(x)) # access results for temperature suitability
#' rice_suit <- suit("ricebr", terrain=MarinduqueLT)
#' lapply(rice_suit[["terrain"]], function(x) head(x))
#' lapply(rice_suit[["soil"]], function(x) head(x))
suit <- function (crop, terrain=NULL, water=NULL, temp=NULL, mf = "triangular", sow_month = NULL, minimum = NULL, maximum = "average", interval = NULL, sigma = NULL) {
  if (is.null(terrain) && is.null(water) && is.null(temp)) {
    stop("Please specify at least one land characteristics: terrain, water, or temp.")
  }
  
  if (!is.character(crop) && is.data.frame(crop)) {
    if (!is.null(terrain)) {
      suit_terrain <- tryCatch({
          suit_terrain <- suitability(terrain, crop, mf=mf, sow_month=NULL, minimum=minimum, maximum=maximum, interval=interval, sigma=sigma)
          suit_terrain[["Crop Evaluated"]] <- "Custom Crop for Terrain"
          suit_terrain
        },
        warning=function(w) {
          suit_terrain <- suitability(terrain, crop, mf=mf, sow_month=NULL, minimum=minimum, maximum=maximum, interval=interval, sigma=sigma)
          suit_terrain[["Crop Evaluated"]] <- "Custom Crop for Terrain"
          suit_terrain[["Warning"]] <- w$message
          suit_terrain
        },
        error = function(x) {
          return(paste("Error: ", x$message, sep=""))
        }
      )
      return(list("terrain" = suit_terrain))
    } else if (!is.null(water)) {
      suit_water <- tryCatch({
          suit_water <- suitability(water, crop, mf=mf, sow_month=NULL, minimum=minimum, maximum=maximum, interval=interval, sigma=sigma)
          suit_water[["Crop Evaluated"]] <- "Custom Crop for Water"
          suit_water
        },
        warning=function(w) {
          suit_water <- suitability(water, crop, mf=mf, sow_month=NULL, minimum=minimum, maximum=maximum, interval=interval, sigma=sigma)
          suit_water[["Crop Evaluated"]] <- "Custom Crop for Water"
          suit_water[["Warning"]] <- w$message
          suit_water
        },
        error = function(x) {
          return(paste("Error: ", x$message, sep=""))
        }
      )
      return(list("water" = suit_water))
    } else if (!is.null(temp)) {
      suit_temp <- tryCatch({
          suit_temp <- suitability(temp, crop, mf=mf, sow_month=NULL, minimum=minimum, maximum=maximum, interval=interval, sigma=sigma)
          suit_temp[["Crop Evaluated"]] <- "Custom Crop for Temperature"
          suit_temp
        },
        warning=function(w) {
          suit_temp <- suitability(temp, crop, mf=mf, sow_month=NULL, minimum=minimum, maximum=maximum, interval=interval, sigma=sigma)
          suit_temp[["Crop Evaluated"]] <- "Custom Crop for Temperature"
          suit_temp[["Warning"]] <- w$message
          suit_temp
        },
        error = function(x) {
          return(paste("Error: ", x$message, sep=""))
        }
      )
      return(list("temp" = suit_temp))
    }
  } else if (is.character(crop)) {
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
      crop_water <- eval(parse(text=paste(crop, "Water", sep="")), envir=.GlobalEnv)
      crop_temp <- eval(parse(text=paste(crop, "Temp", sep="")), envir=.GlobalEnv)
      suit_terrain <- tryCatch(
        {
          suit_terrain <- suitability(terrain, crop_terrain, mf=mf, sow_month=NULL, minimum=minimum, maximum=maximum, interval=interval, sigma=sigma)
          suit_terrain[["Crop Evaluated"]] <- paste(crop, "Terrain", sep="")
          suit_terrain
        },
        warning=function(w) {
          suit_terrain <- suitability(terrain, crop_terrain, mf=mf, sow_month=NULL, minimum=minimum, maximum=maximum, interval=interval, sigma=sigma)
          suit_terrain[["Crop Evaluated"]] <- paste(crop, "Terrain", sep="")
          suit_terrain[["Warning"]] <- w$message
          suit_terrain
        },
        error = function(x) {
          return(paste("Error: ", x$message, sep=""))
        }
      )
      suit_soil <- tryCatch(
        {
          suit_soil <- suitability(terrain, crop_soil, mf=mf, sow_month=NULL, minimum=minimum, maximum=maximum, interval=interval, sigma=sigma)
          suit_soil[["Crop Evaluated"]] <- paste(crop, "Soil", sep="")
          suit_soil
        },
        warning=function(w) {
          suit_soil <- suitability(terrain, crop_soil, mf=mf, sow_month=NULL, minimum=minimum, maximum=maximum, interval=interval, sigma=sigma)
          suit_soil[["Crop Evaluated"]] <- paste(crop, "Soil", sep="")
          suit_soil[["Warning"]] <- w$message
          suit_soil
        },
        error = function(x) {
          return(paste("Error: ", x$message, sep=""))
        }
      )
      suit_water <- tryCatch(
        {
          suit_water <- suitability(water, crop_water, mf=mf, sow_month=sow_month, minimum=minimum, maximum=maximum, interval=interval, sigma=sigma)
          suit_water[["Crop Evaluated"]] <- paste(crop, "Water", sep="")
          suit_water
        },
        warning=function(w) {
          suit_water <- suitability(water, crop_water, mf=mf, sow_month=sow_month, minimum=minimum, maximum=maximum, interval=interval, sigma=sigma)
          suit_water[["Crop Evaluated"]] <- paste(crop, "Water", sep="")
          suit_water[["Warning"]]  <- w$message
          suit_water
        },
        error = function(x) {
          return(paste("Error: ", x$message, sep=""))
        }
      )
      suit_temp <- tryCatch(
        {
          suit_temp <- suitability(temp, crop_temp, mf=mf, sow_month=sow_month, minimum=minimum, maximum=maximum, interval=interval, sigma=sigma)
          suit_temp[["Crop Evaluated"]] <- paste(crop, "Temp", sep="")
          suit_temp
        },
        warning=function(w) {
          suit_temp <- suitability(temp, crop_temp, mf=mf, sow_month=sow_month, minimum=minimum, maximum=maximum, interval=interval, sigma=sigma)
          suit_temp[["Crop Evaluated"]] <- paste(crop, "Temp", sep="")
          suit_temp[["Warning"]] <- w$message
          suit_temp
        },
        error = function(x) {
          return(paste("Error: ", x$message, sep=""))
        }
      )
      return(list("terrain" = suit_terrain, "soil" = suit_soil, "water" = suit_water, "temp" = suit_temp))
    } else if (!is.null(terrain) && !is.null(water)) {
      if (is.null(sow_month)) {
        stop("Please specify sowing month to match the corresponding factors in input land units.")
      }
      crop_terrain <- eval(parse(text=paste(crop, "Terrain", sep="")), envir=.GlobalEnv)
      crop_soil <- eval(parse(text=paste(crop, "Soil", sep="")), envir=.GlobalEnv)
      crop_water <- eval(parse(text=paste(crop, "Water", sep="")), envir=.GlobalEnv)
      suit_terrain <- tryCatch(
        {
          suit_terrain <- suitability(terrain, crop_terrain, mf=mf, sow_month=NULL, minimum=minimum, maximum=maximum, interval=interval, sigma=sigma)
          suit_terrain[["Crop Evaluated"]] <- paste(crop, "Terrain", sep="")
          suit_terrain
        },
        warning=function(w) {
          suit_terrain <- suitability(terrain, crop_terrain, mf=mf, sow_month=NULL, minimum=minimum, maximum=maximum, interval=interval, sigma=sigma)
          suit_terrain[["Crop Evaluated"]] <- paste(crop, "Terrain", sep="")
          suit_terrain[["Warning"]] <- w$message
          suit_terrain
        },
        error = function(x) {
          return(paste("Error: ", x$message, sep=""))
        }
      )
      suit_soil <- tryCatch(
        {
          suit_soil <- suitability(terrain, crop_soil, mf=mf, sow_month=NULL, minimum=minimum, maximum=maximum, interval=interval, sigma=sigma)
          suit_soil[["Crop Evaluated"]] <- paste(crop, "Soil", sep="")
          suit_soil
        },
        warning=function(w) {
          suit_soil <- suitability(terrain, crop_soil, mf=mf, sow_month=NULL, minimum=minimum, maximum=maximum, interval=interval, sigma=sigma)
          suit_soil[["Crop Evaluated"]] <- paste(crop, "Soil", sep="")
          suit_soil[["Warning"]] <- w$message
          suit_soil
        },
        error = function(x) {
          return(paste("Error: ", x$message, sep=""))
        }
      )
      suit_water <- tryCatch(
        {
          suit_water <- suitability(water, crop_water, mf=mf, sow_month=sow_month, minimum=minimum, maximum=maximum, interval=interval, sigma=sigma)
          suit_water[["Crop Evaluated"]] <- paste(crop, "Water", sep="")
          suit_water
        },
        warning=function(w) {
          suit_water <- suitability(water, crop_water, mf=mf, sow_month=sow_month, minimum=minimum, maximum=maximum, interval=interval, sigma=sigma)
          suit_water[["Crop Evaluated"]] <- paste(crop, "Water", sep="")
          suit_water[["Warning"]]  <- w$message
          suit_water
        },
        error = function(x) {
          return(paste("Error: ", x$message, sep=""))
        }
      )
      return(list("terrain" = suit_terrain, "soil" = suit_soil, "water" = suit_water))
    } else if (!is.null(terrain) && !is.null(temp)) {
      if (is.null(sow_month)) {
        stop("Please specify sowing month to match the corresponding factors in input land units.")
      }
      crop_terrain <- eval(parse(text=paste(crop, "Terrain", sep="")), envir=.GlobalEnv)
      crop_soil <- eval(parse(text=paste(crop, "Soil", sep="")), envir=.GlobalEnv)
      crop_temp <- eval(parse(text=paste(crop, "Temp", sep="")), envir=.GlobalEnv)
      suit_terrain <- tryCatch(
        {
          suit_terrain <- suitability(terrain, crop_terrain, mf=mf, sow_month=NULL, minimum=minimum, maximum=maximum, interval=interval, sigma=sigma)
          suit_terrain[["Crop Evaluated"]] <- paste(crop, "Terrain", sep="")
          suit_terrain
        },
        warning=function(w) {
          suit_terrain <- suitability(terrain, crop_terrain, mf=mf, sow_month=NULL, minimum=minimum, maximum=maximum, interval=interval, sigma=sigma)
          suit_terrain[["Crop Evaluated"]] <- paste(crop, "Terrain", sep="")
          suit_terrain[["Warning"]] <- w$message
          suit_terrain
        },
        error = function(x) {
          return(paste("Error: ", x$message, sep=""))
        }
      )
      suit_soil <- tryCatch(
        {
          suit_soil <- suitability(terrain, crop_soil, mf=mf, sow_month=NULL, minimum=minimum, maximum=maximum, interval=interval, sigma=sigma)
          suit_soil[["Crop Evaluated"]] <- paste(crop, "Soil", sep="")
          suit_soil
        },
        warning=function(w) {
          suit_soil <- suitability(terrain, crop_soil, mf=mf, sow_month=NULL, minimum=minimum, maximum=maximum, interval=interval, sigma=sigma)
          suit_soil[["Crop Evaluated"]] <- paste(crop, "Soil", sep="")
          suit_soil[["Warning"]] <- w$message
          suit_soil
        },
        error = function(x) {
          return(paste("Error: ", x$message, sep=""))
        }
      )
      suit_temp <- tryCatch(
        {
          suit_temp <- suitability(temp, crop_temp, mf=mf, sow_month=sow_month, minimum=minimum, maximum=maximum, interval=interval, sigma=sigma)
          suit_temp[["Crop Evaluated"]] <- paste(crop, "Temp", sep="")
          suit_temp
        },
        warning=function(w) {
          suit_temp <- suitability(temp, crop_temp, mf=mf, sow_month=sow_month, minimum=minimum, maximum=maximum, interval=interval, sigma=sigma)
          suit_temp[["Crop Evaluated"]] <- paste(crop, "Temp", sep="")
          suit_temp[["Warning"]] <- w$message
          suit_temp
        },
        error = function(x) {
          return(paste("Error: ", x$message, sep=""))
        }
      )
      return(list("terrain" = suit_terrain, "soil" = suit_soil, "temp" = suit_temp))
    } else if (!is.null(water) && !is.null(temp)) {
      if (is.null(sow_month)) {
        stop("Please specify sowing month to match the corresponding factors in input land units.")
      }
      crop_water <- eval(parse(text=paste(crop, "Water", sep="")), envir=.GlobalEnv)
      suit_water <- tryCatch(
        {
          suit_water <- suitability(water, crop_water, mf=mf, sow_month=sow_month, minimum=minimum, maximum=maximum, interval=interval, sigma=sigma)
          suit_water[["Crop Evaluated"]] <- paste(crop, "Water", sep="")
          suit_water
        },
        warning=function(w) {
          suit_water <- suitability(water, crop_water, mf=mf, sow_month=sow_month, minimum=minimum, maximum=maximum, interval=interval, sigma=sigma)
          suit_water[["Crop Evaluated"]] <- paste(crop, "Water", sep="")
          suit_water[["Warning"]]  <- w$message
          suit_water
        },
        error = function(x) {
          return(paste("Error: ", x$message, sep=""))
        }
      )
      crop_temp <- eval(parse(text=paste(crop, "Temp", sep="")), envir=.GlobalEnv)
      suit_temp <- tryCatch(
        {
          suit_temp <- suitability(temp, crop_temp, mf=mf, sow_month=sow_month, minimum=minimum, maximum=maximum, interval=interval, sigma=sigma)
          suit_temp[["Crop Evaluated"]] <- paste(crop, "Temp", sep="")
          suit_temp
        },
        warning=function(w) {
          suit_temp <- suitability(temp, crop_temp, mf=mf, sow_month=sow_month, minimum=minimum, maximum=maximum, interval=interval, sigma=sigma)
          suit_temp[["Crop Evaluated"]] <- paste(crop, "Temp", sep="")
          suit_temp[["Warning"]] <- w$message
          suit_temp
        },
        error = function(x) {
          return(paste("Error: ", x$message, sep=""))
        }
      )
      return(list("water" = suit_water, "temp" = suit_temp))
    } else if (!is.null(terrain)) {
      crop_terrain <- eval(parse(text=paste(crop, "Terrain", sep="")), envir=.GlobalEnv)
      crop_soil <- eval(parse(text=paste(crop, "Soil", sep="")), envir=.GlobalEnv)
      suit_terrain <- tryCatch(
        {
          suit_terrain <- suitability(terrain, crop_terrain, mf=mf, sow_month=NULL, minimum=minimum, maximum=maximum, interval=interval, sigma=sigma)
          suit_terrain[["Crop Evaluated"]] <- paste(crop, "Terrain", sep="")
          suit_terrain
        },
        warning=function(w) {
          suit_terrain <- suitability(terrain, crop_terrain, mf=mf, sow_month=NULL, minimum=minimum, maximum=maximum, interval=interval, sigma=sigma)
          suit_terrain[["Crop Evaluated"]] <- paste(crop, "Terrain", sep="")
          suit_terrain[["Warning"]] <- w$message
          suit_terrain
        },
        error = function(x) {
          return(paste("Error: ", x$message, sep=""))
        }
      )
      suit_soil <- tryCatch(
        {
          suit_soil <- suitability(terrain, crop_soil, mf=mf, sow_month=NULL, minimum=minimum, maximum=maximum, interval=interval, sigma=sigma)
          suit_soil[["Crop Evaluated"]] <- paste(crop, "Soil", sep="")
          suit_soil
        },
        warning=function(w) {
          suit_soil <- suitability(terrain, crop_soil, mf=mf, sow_month=NULL, minimum=minimum, maximum=maximum, interval=interval, sigma=sigma)
          suit_soil[["Crop Evaluated"]] <- paste(crop, "Soil", sep="")
          suit_soil[["Warning"]] <- w$message
          suit_soil
        },
        error = function(x) {
          return(paste("Error: ", x$message, sep=""))
        }
      )
      return(list("terrain" = suit_terrain, "soil" = suit_soil))
    } else if (!is.null(water)) {
      if (is.null(sow_month)) {
        stop("Please specify sowing month to match the corresponding factors in input land units.")
      }
      crop_water <- eval(parse(text=paste(crop, "Water", sep="")), envir=.GlobalEnv)
      suit_water <- tryCatch(
        {
          suit_water <- suitability(water, crop_water, mf=mf, sow_month=sow_month, minimum=minimum, maximum=maximum, interval=interval, sigma=sigma)
          suit_water[["Crop Evaluated"]] <- paste(crop, "Water", sep="")
          suit_water
        },
        warning=function(w) {
          suit_water <- suitability(water, crop_water, mf=mf, sow_month=sow_month, minimum=minimum, maximum=maximum, interval=interval, sigma=sigma)
          suit_water[["Crop Evaluated"]] <- paste(crop, "Water", sep="")
          suit_water[["Warning"]]  <- w$message
          suit_water
        },
        error = function(x) {
          return(paste("Error: ", x$message, sep=""))
        }
      )
      return(list("water" = suit_water))
    } else if (!is.null(temp)) {
      if (is.null(sow_month)) {
        stop("Please specify sowing month to match the corresponding factors in input land units.")
      }
      crop_temp <- eval(parse(text=paste(crop, "Temp", sep="")), envir=.GlobalEnv)
      suit_temp <- tryCatch(
        {
          suit_temp <- suitability(temp, crop_temp, mf=mf, sow_month=sow_month, minimum=minimum, maximum=maximum, interval=interval, sigma=sigma)
          suit_temp[["Crop Evaluated"]] <- paste(crop, "Temp", sep="")
          suit_temp
        },
        warning=function(w) {
          suit_temp <- suitability(temp, crop_temp, mf=mf, sow_month=sow_month, minimum=minimum, maximum=maximum, interval=interval, sigma=sigma)
          suit_temp[["Crop Evaluated"]] <- paste(crop, "Temp", sep="")
          suit_temp[["Warning"]] <- w$message
          suit_temp
        },
        error = function(x) {
          return(paste("Error: ", x$message, sep=""))
        }
      )
      return(list("temp" = suit_temp))
    } 
  }
}
