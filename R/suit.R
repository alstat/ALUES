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
#' \code{https://alstat.github.io/ALUES/}; \code{\link{overall_suit}}
#' 
#' @examples
#' library(ALUES)
#' banana_suit <- suit("banana", terrain=MarinduqueLT)
#' names(banana_suit)
#' 
#' # The warning above simply tells the user that one of the factor, 
#' # CECc, in the target crop requirement, has parameter intervals for 
#' # all suitability classes equal to 16, and the package used this value 
#' # as the maximum constant for computing the suitability scores. For more,
#' # please refer to the **Article 2: Methodology used in ALUES** of the documentation.
#' # The `suit` function returns a list of output of target 
#' # characteristics, in this case `"terrain"` and `"soil"`. To access 
#' # the output, simply run the following:
#'
#' # lapply is used to display the head of each items in the list
#' lapply(banana_suit[["terrain"]], function(x) head(x)) 
#' lapply(banana_suit[["soil"]], function(x) head(x))
#' 
#' # Each of these are lists, with the following names:
#' names(banana_suit[["soil"]])
#' 
#' # So that, to access the factors evaluated, simply run the following:
#' banana_suit[["soil"]][["Factors Evaluated"]]
#' 
#' ## Targetting Crop
#' # There are 56 crops available in ALUES, and what we've illustrated 
#' # above is for banana only. Other crops are listed below:
#' d <- utils::data(package = "ALUES")
#' alues_data <- d$results[, "Item"]
#' crop_data <- regmatches(alues_data, gregexpr(paste0("^[A-Z]{2,}", 
#' collapse = "|"), alues_data))
#' crop_data <- unique(unlist(lapply(crop_data, 
#' function(x) substr(x, 1, nchar(x)-1))))
#' crop_data
#' 
#' # These are the names for the input string for the 
#' # `suit` function. For example, to target sweet potato the 
#' # input string is not `"sweet potato"` but rather `potatosw`. That is,
#' # potato_suit1 <- suit("sweet potato", terrain=MarinduqueLT)
#' potato_suit2 <- suit("potatosw", terrain=MarinduqueLT)
#' 
#' ## Targetting Crop Factors
#' # The idea of evaluating a land unit is to match the 
#' # quality of the land against the standard value of the 
#' # target factor. Therefore, if the crop does not include 
#' # the factor you are targeting, then there won't be any 
#' # matching to be done. For example, the land units evaluated 
#' # above are those in Marindque, which has the following soil 
#' # and terrain characteristics:
#' head(MarinduqueLT)
#' # The crop that we are trying to target is banana. The `suit`
#' # function simply require the user to input a string name for 
#' # the target crop, and the function will look for the corresponding 
#' # crop datasets. For example, for banana these are the crop 
#' # requirements datasets for the four characteristics:
#' BANANATerrain
#' BANANASoil
#' BANANAWater
#' BANANATemp
#' # These datasets are used by the `suit` function depending 
#' # on the targetted characteristics of the input land units 
#' # (specified by the user) on the said function. So for 
#' # `banana_suit` object above, the target crop datasets were 
#' # `BANANATerrain` and `BANANASoil` since the input land unit 
#' # specified is `terrain=MarinduqueLT`. Further, the input land 
#' # unit only targetted the soil factors and not the terrain factors, 
#' # since none of the factors in `MarinduqueLT` matched with the 
#' # factors in `BANANATerrain`. That is why, accessing the output 
#' # for the terrain characteristics for the `banana_suit` object 
#' # will return the following:
#' banana_suit[["terrain"]]
#' 
#' ## Targetting Multiple Characteristics
#' # The example above only targetted the terrain and soil 
#' # characteristics, but the `suit` function allows user to 
#' # also target water and temp simultaneously. For examples, 
#' # we can evaluate the land units of Lao Cai, Vietnam for all 
#' # three characteristics as follows for irrigated rice (`riceiw``):
#' riceiw_multi <- suit("riceiw", terrain=LaoCaiLT, water=LaoCaiWater, 
#' temp=LaoCaiTemp, sow_month=10)
#' names(riceiw_multi)
#' 
#' # It is necessary to specify the sowing month when specifying 
#' # the water and temperature characteristics of the input land 
#' # units. In this case, we are saying that the first sowing 
#' # month for both water and temperature characteristics 
#' # correspond to October (*See* Article 6 for more on this). 
#' # No factors were targetted by input land unit for banana for
#' # terrain, water and temperature characteristics.
#' lapply(riceiw_multi[["terrain"]], function(x) head(x))
#' lapply(riceiw_multi[["soil"]], function(x) head(x))
#' lapply(riceiw_multi[["water"]], function(x) head(x))
#' lapply(riceiw_multi[["temp"]], function(x) head(x))
#' 
#' # Only the head (first six) of the output of the items are shown.
#' 
#' ## Membership Function
#' # There are three membership functions (MFs) available in 
#' # the `suit` function, namely *triangular*, *trapezoidal* 
#' # and *Gaussian*. For example, the following computes the 
#' # suitability scores and classes using trapezoidal MF.
#' banana_suit <- suit("banana", terrain=MarinduqueLT, mf="trapezoidal")
#' head(banana_suit[["soil"]][["Suitability Score"]])
#' head(banana_suit[["soil"]][["Suitability Class"]])
#' 
#' ## Intervals
#' # Another option available in the `suit` function is the 
#' # `interval`. By default, ALUES uses an equally spaced 
#' # suitability class intervals for deriving the suitability 
#' # class. That is, for N [0, 0.25), S3 [0.25, 0.50), 
#' # S2 [0.50, 0.75), and S1 [0.75, 1].
#' 
#' ### Custom Intervals
#' # Users can modify the default equally spaced intervals, for example:
#' banana_suit <- suit("banana", terrain=MarinduqueLT, 
#' mf="trapezoidal", interval=c(0, 0.3, 0.6, 0.9, 1))
#' head(banana_suit[["soil"]][["Suitability Score"]])
#' head(banana_suit[["soil"]][["Suitability Class"]])
#' # The above code sets the new suitability class intervals 
#' # into: N [0, 0.3), S3 [0.3, 0.6), S2 [0.6, 0.9), and S1 [0.9, 1].
#' 
#' ### Unbias Intervals
#' # The problem with the fixed interval is that the said 
#' # intervals does not take into account the shape of the 
#' # membership function and the spacing of the parameter 
#' # interval limits (*See* Article 2 for parameter intervals). 
#' # Custom intervals might be able to capture this if the user 
#' # computed the interval limits manually, but ALUES provides 
#' # an option just for this, by setting `interval="unbias"`. 
#' # That is,
#' banana_suit <- suit("banana", terrain=MarinduqueLT, 
#' mf="trapezoidal", interval="unbias")
#' head(banana_suit[["soil"]][["Suitability Score"]])
#' head(banana_suit[["soil"]][["Suitability Class"]])
#' # By setting the `interval="unbias"`, the `suit` 
#' # function will generate a different likely unequally 
#' # spaced suitability class intervals, but the interval 
#' # limits are mathematically correct, in terms of the mapping
#' # of the parameter interval limits to suitability class limits 
#' # via the membership function.
#' 
#' ## Maximum and Minimum
#' # Another parameter that can be set for `suit` are the 
#' # `minimum` and `maximum`. These are the constants used 
#' # by the membership function for computing the suitability 
#' # score.
#' banana_suit <- suit("banana", terrain=MarinduqueLT, 
#' mf="trapezoidal", interval="unbias")
#' banana_suit[["soil"]][["Factors Evaluated"]]
#' 
#' # From the above result, there are four factors targetted
#' # by the input land unit, these are CFragm, CECc, pHH2O and 
#' # SoilTe. Suppose we know the maximum value that these factors 
#' # can take, say 60 for CFragm, 20 for CECc, 9 for pHH2O, and 10
#' # for SoilTe. We can specify these as follows:
#' banana_suit <- suit("banana", terrain=MarinduqueLT, mf="trapezoidal",
#' interval="unbias", maximum=c(60, 20, 9, 10))
#' banana_suit
#' # The result gave us an error. We understand the error 
#' # for terrain characteristics, but for soil it says that
#' # the argument `maximum` must be equal in length with the
#' # target factors specified in the input land unit datasets.
#' # We know that there should be 4 factors, but upon checking, 
#' # we see that the `MarinduqueLT` also have Lon and Lat columns,
#' # which ALUES assumes to be a target factor as well. Indeed, we 
#' # need to exclude these columns (those that are not the target 
#' # factors, rather spatial variables) when specifying `minimum` 
#' # or `maximum` constants. Thus, it should be:
#' MarinduqueLT2 <- MarinduqueLT[, 3:ncol(MarinduqueLT)]
#' banana_suit <- suit("banana", terrain=MarinduqueLT2, 
#' mf="trapezoidal", interval="unbias", maximum=c(60, 20, 9, 10))
#' head(banana_suit[["soil"]][["Suitability Score"]])
#' head(banana_suit[["soil"]][["Suitability Class"]])
#' 
#' ## Sigma of Gaussian
#' # The `sigma` argument is used to specify the scale of the 
#' # Gaussian membership function. That is, it is only 
#' # applicable for `mf="gaussian"`.
#' 
#' # Other examples
#' library(ALUES)
#' 
#' rice_suit <- suit("ricebr", water=MarinduqueWater, 
#' temp=MarinduqueTemp, sow_month = 1)
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
